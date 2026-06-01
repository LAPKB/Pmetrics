use anyhow::{anyhow, Context, Result as AnyResult};
use extendr_api::{list, List};
use pmcore::api::{FitControl, FitProgress};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::io::{BufRead, BufReader, ErrorKind, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

static LIVE_SESSIONS: OnceLock<Mutex<HashMap<String, Arc<LiveSession>>>> = OnceLock::new();

fn registry() -> &'static Mutex<HashMap<String, Arc<LiveSession>>> {
    LIVE_SESSIONS.get_or_init(|| Mutex::new(HashMap::new()))
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum LiveSessionMessage {
    SessionStarted { session_id: String },
    Progress { event: FitProgress },
    FitFailed { message: String },
    FinalReportReady {
        result_payload: String,
        report_generated_at: String,
    },
    ReportFailed { message: String },
    SessionClosed,
}

#[derive(Debug, Deserialize)]
struct LiveCommandEnvelope {
    kind: String,
}

/// Maximum number of messages to keep in the backlog for reconnecting clients.
/// This prevents unbounded memory growth on long runs.
const MAX_BACKLOG_SIZE: usize = 1000;

#[derive(Debug, Default)]
struct LiveSessionState {
    backlog: Vec<String>,
    command_queue: VecDeque<FitControl>,
    client_writer: Option<TcpStream>,
    connected: bool,
    closed: bool,
}

#[derive(Debug)]
pub(crate) struct LiveSession {
    id: String,
    host: String,
    port: u16,
    state: Mutex<LiveSessionState>,
}

impl LiveSession {
    fn new(listener: TcpListener) -> AnyResult<Arc<Self>> {
        listener
            .set_nonblocking(true)
            .context("failed to configure live session listener")?;

        let port = listener
            .local_addr()
            .context("failed to read live session listener address")?
            .port();
        let session = Arc::new(Self {
            id: format!(
                "pm-live-{}-{}",
                std::process::id(),
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .context("system clock is before unix epoch")?
                    .as_nanos()
            ),
            host: "127.0.0.1".to_string(),
            port,
            state: Mutex::new(LiveSessionState::default()),
        });

        session.publish(LiveSessionMessage::SessionStarted {
            session_id: session.id.clone(),
        })?;
        spawn_accept_loop(Arc::clone(&session), listener);
        Ok(session)
    }

    fn info(&self) -> List {
        list!(
            session_id = self.id.clone(),
            host = self.host.clone(),
            port = i32::from(self.port)
        )
    }

    #[cfg(test)]
    pub(crate) fn id(&self) -> &str {
        &self.id
    }

    #[cfg(test)]
    pub(crate) fn port(&self) -> u16 {
        self.port
    }

    #[cfg(test)]
    pub(crate) fn backlog_snapshot(&self) -> Vec<String> {
        self.state
            .lock()
            .expect("live session state lock poisoned")
            .backlog
            .clone()
    }

    fn publish(&self, message: LiveSessionMessage) -> AnyResult<()> {
        let payload = serde_json::to_string(&message)
            .context("failed to serialize live session message")?;
        let mut state = self.state.lock().expect("live session state lock poisoned");
        state.backlog.push(payload.clone());

        if let Some(writer) = state.client_writer.as_mut() {
            if writeln!(writer, "{}", payload).is_err() || writer.flush().is_err() {
                state.connected = false;
                state.client_writer = None;
            }
        }

        Ok(())
    }

    pub(crate) fn publish_progress(&self, event: FitProgress) -> AnyResult<()> {
        self.publish(LiveSessionMessage::Progress { event })
    }

    pub(crate) fn publish_fit_failed(&self, message: &str) -> AnyResult<()> {
        self.publish(LiveSessionMessage::FitFailed {
            message: message.to_string(),
        })
    }

    pub(crate) fn publish_final_report_ready(
        &self,
        result_payload: &str,
        report_generated_at: &str,
    ) -> AnyResult<()> {
        self.publish(LiveSessionMessage::FinalReportReady {
            result_payload: result_payload.to_string(),
            report_generated_at: report_generated_at.to_string(),
        })
    }

    pub(crate) fn publish_report_failed(&self, message: &str) -> AnyResult<()> {
        self.publish(LiveSessionMessage::ReportFailed {
            message: message.to_string(),
        })
    }

    pub(crate) fn next_control(&self) -> AnyResult<Option<FitControl>> {
        let mut state = self.state.lock().expect("live session state lock poisoned");
        Ok(state.command_queue.pop_front())
    }

    fn wait_connected(&self, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        loop {
            {
                let state = self.state.lock().expect("live session state lock poisoned");
                if state.connected {
                    return true;
                }
                if state.closed {
                    return false;
                }
            }

            if Instant::now() >= deadline {
                return false;
            }

            thread::sleep(Duration::from_millis(25));
        }
    }

    fn close(&self) -> AnyResult<()> {
        self.publish(LiveSessionMessage::SessionClosed)?;
        let writer = {
            let mut state = self.state.lock().expect("live session state lock poisoned");
            state.closed = true;
            state.client_writer.take()
        };
        if let Some(writer) = writer {
            let _ = writer.shutdown(Shutdown::Both);
        }
        Ok(())
    }
}

fn spawn_accept_loop(session: Arc<LiveSession>, listener: TcpListener) {
    thread::spawn(move || loop {
        {
            let state = session.state.lock().expect("live session state lock poisoned");
            if state.closed {
                break;
            }
        }

        match listener.accept() {
            Ok((stream, _)) => {
                if let Err(error) = attach_client(Arc::clone(&session), stream) {
                    tracing::warn!("live session client attach failed: {}", error);
                }
            }
            Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                thread::sleep(Duration::from_millis(25));
            }
            Err(error) => {
                tracing::warn!("live session accept failed: {}", error);
                break;
            }
        }
    });
}

fn attach_client(session: Arc<LiveSession>, stream: TcpStream) -> AnyResult<()> {
    stream
        .set_nonblocking(false)
        .context("failed to configure live session client stream")?;
    stream
        .set_nodelay(true)
        .context("failed to enable TCP_NODELAY for live session")?;
    let mut writer_stream = stream
        .try_clone()
        .context("failed to clone live session stream")?;
    let backlog = {
        let state = session.state.lock().expect("live session state lock poisoned");
        state.backlog.clone()
    };

    for message in backlog {
        writeln!(writer_stream, "{}", message)
            .context("failed to write live session backlog")?;
        writer_stream
            .flush()
            .context("failed to flush live session backlog")?;
    }

    {
        let mut state = session.state.lock().expect("live session state lock poisoned");
        state.connected = true;
        state.client_writer = Some(writer_stream);
    }

    let reader_session = Arc::clone(&session);
    thread::spawn(move || {
        let mut reader = BufReader::new(stream);
        loop {
            let mut line = String::new();
            match reader.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    match parse_command(line) {
                        Ok(command) => {
                            let mut state = reader_session
                                .state
                                .lock()
                                .expect("live session state lock poisoned");
                            state.command_queue.push_back(command);
                        }
                        Err(error) => {
                            tracing::warn!("ignoring invalid live session command '{}': {}", line, error);
                        }
                    }
                }
                Err(error)
                    if matches!(
                        error.kind(),
                        ErrorKind::WouldBlock | ErrorKind::Interrupted | ErrorKind::TimedOut
                    ) =>
                {
                    thread::sleep(Duration::from_millis(25));
                }
                Err(error) => {
                    let closed = {
                        let state = reader_session
                            .state
                            .lock()
                            .expect("live session state lock poisoned");
                        state.closed
                    };
                    if !closed {
                        tracing::warn!("live session read failed: {}", error);
                    }
                    break;
                }
            }
        }
    });

    Ok(())
}

fn parse_command(line: &str) -> AnyResult<FitControl> {
    if let Ok(command) = serde_json::from_str::<LiveCommandEnvelope>(line) {
        return fit_control_from_kind(&command.kind);
    }

    fit_control_from_kind(line)
}

fn fit_control_from_kind(kind: &str) -> AnyResult<FitControl> {
    match kind.trim().to_ascii_lowercase().as_str() {
        "pause_after_cycle" => Ok(FitControl::PauseAfterCycle),
        "resume" => Ok(FitControl::Resume),
        "stop_after_cycle" => Ok(FitControl::StopAfterCycle),
        "ping" => Ok(FitControl::Ping),
        other => Err(anyhow!("unsupported live session command '{}'", other)),
    }
}

pub(crate) fn session(session_id: &str) -> AnyResult<Arc<LiveSession>> {
    registry()
        .lock()
        .expect("live session registry lock poisoned")
        .get(session_id)
        .cloned()
        .ok_or_else(|| anyhow!("unknown live session '{}'", session_id))
}

pub(crate) fn create_live_session() -> AnyResult<Arc<LiveSession>> {
    let listener = TcpListener::bind("127.0.0.1:0").context("failed to bind live session")?;
    let session = LiveSession::new(listener)?;
    registry()
        .lock()
        .expect("live session registry lock poisoned")
        .insert(session.id.clone(), Arc::clone(&session));
    Ok(session)
}

pub(crate) fn start_live_session() -> AnyResult<List> {
    let session = create_live_session()?;
    let info = session.info();
    Ok(info)
}

pub(crate) fn wait_live_session_connected(session_id: &str, timeout_ms: i32) -> AnyResult<bool> {
    let timeout_ms = timeout_ms.max(0) as u64;
    Ok(session(session_id)?.wait_connected(Duration::from_millis(timeout_ms)))
}

pub(crate) fn publish_live_report_result(
    session_id: &str,
    result_payload: &str,
    report_generated_at: &str,
) -> AnyResult<()> {
    session(session_id)?.publish_final_report_ready(result_payload, report_generated_at)
}

pub(crate) fn publish_live_report_failed(session_id: &str, message: &str) -> AnyResult<()> {
    session(session_id)?.publish_report_failed(message)
}

pub(crate) fn close_live_session(session_id: &str) -> AnyResult<()> {
    if let Some(session) = registry()
        .lock()
        .expect("live session registry lock poisoned")
        .remove(session_id)
    {
        session.close()?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pmcore::api::FitProgress;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpStream;

    fn connect(session: &LiveSession) -> AnyResult<TcpStream> {
        let stream = TcpStream::connect(("127.0.0.1", session.port()))
            .context("failed to connect to live session")?;
        stream
            .set_read_timeout(Some(Duration::from_secs(2)))
            .context("failed to set live session read timeout")?;
        Ok(stream)
    }

    #[test]
    fn live_session_accepts_client_and_replays_backlog() -> AnyResult<()> {
        let session = create_live_session()?;
        let stream = connect(&session)?;

        let mut reader = BufReader::new(stream);
        let mut first_line = String::new();
        reader
            .read_line(&mut first_line)
            .context("failed to read live session backlog")?;

        let message: serde_json::Value = serde_json::from_str(first_line.trim())
            .context("failed to parse live session backlog")?;
        assert_eq!(message["kind"], "session_started");
        assert_eq!(message["session_id"], session.id());

        close_live_session(session.id())?;
        Ok(())
    }

    #[test]
    fn live_session_publishes_progress_and_reads_commands() -> AnyResult<()> {
        let session = create_live_session()?;
        let mut stream = connect(&session)?;

        stream
            .write_all(b"{\"kind\":\"stop_after_cycle\"}\n")
            .context("failed to write live session command")?;
        stream.flush().context("failed to flush live session command")?;

        let started = Instant::now();
        let mut received = None;
        while started.elapsed() < Duration::from_secs(2) {
            if let Some(command) = session.next_control()? {
                received = Some(command);
                break;
            }
            thread::sleep(Duration::from_millis(25));
        }

        assert!(matches!(received, Some(FitControl::StopAfterCycle)));

        session.publish_progress(FitProgress::FitStarted)?;
        let state = session.state.lock().expect("live session state lock poisoned");
        assert!(state.backlog.iter().any(|line| line.contains("\"fit_started\"")));
        drop(state);

        close_live_session(session.id())?;
        Ok(())
    }

    #[test]
    fn live_session_reads_commands_after_idle_delay() -> AnyResult<()> {
        let session = create_live_session()?;
        let mut stream = connect(&session)?;

        thread::sleep(Duration::from_millis(150));

        stream
            .write_all(b"{\"kind\":\"ping\"}\n")
            .context("failed to write delayed live session command")?;
        stream
            .flush()
            .context("failed to flush delayed live session command")?;

        let started = Instant::now();
        let mut received = None;
        while started.elapsed() < Duration::from_secs(2) {
            if let Some(command) = session.next_control()? {
                received = Some(command);
                break;
            }
            thread::sleep(Duration::from_millis(25));
        }

        assert!(matches!(received, Some(FitControl::Ping)));

        close_live_session(session.id())?;
        Ok(())
    }

    #[test]
    fn live_session_publishes_final_report_messages() -> AnyResult<()> {
        let session = create_live_session()?;

        session.publish_final_report_ready("encoded-result", "2026-05-25T00:00:00Z")?;
        session.publish_report_failed("final report handoff failed")?;

        let backlog = session.backlog_snapshot();
        assert!(backlog
            .iter()
            .any(|line| line.contains("\"kind\":\"final_report_ready\"")));
        assert!(backlog
            .iter()
            .any(|line| line.contains("\"kind\":\"report_failed\"")));

        close_live_session(session.id())?;
        Ok(())
    }
}