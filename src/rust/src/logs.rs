use std::fmt;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Instant;

use anyhow::{anyhow, Result};
use extendr_api::prelude::*;
use extendr_api::rprintln;
use pmcore::api::LoggingLevel;
use tracing::Level;
use tracing_subscriber::fmt::time::FormatTime;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Layer;

// Global timer that can be reset
static GLOBAL_TIMER: OnceLock<Arc<Mutex<Instant>>> = OnceLock::new();
static GLOBAL_LOG_CONFIG: OnceLock<Arc<Mutex<LogConfig>>> = OnceLock::new();

#[derive(Debug)]
struct LogConfig {
    level: LoggingLevel,
    stdout: bool,
    file: Option<File>,
}

impl Default for LogConfig {
    fn default() -> Self {
        Self {
            level: LoggingLevel::Info,
            stdout: true,
            file: None,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LogControls {
    pub level: LoggingLevel,
    pub stdout: bool,
    pub write: bool,
    pub output_path: Option<PathBuf>,
}

impl Default for LogControls {
    fn default() -> Self {
        Self {
            level: LoggingLevel::Info,
            stdout: true,
            write: false,
            output_path: None,
        }
    }
}

fn global_log_config() -> Arc<Mutex<LogConfig>> {
    GLOBAL_LOG_CONFIG
        .get_or_init(|| Arc::new(Mutex::new(LogConfig::default())))
        .clone()
}

pub(crate) fn configure_logs(controls: &LogControls) -> Result<()> {
    let file = if controls.write {
        let output_path = controls
            .output_path
            .as_ref()
            .ok_or_else(|| anyhow!("log output path is required when write_logs = TRUE"))?;
        fs::create_dir_all(output_path)?;
        Some(File::create(output_path.join("log.txt"))?)
    } else {
        None
    };

    let config_handle = global_log_config();
    let mut config = config_handle.lock().unwrap();
    config.level = controls.level;
    config.stdout = controls.stdout;
    config.file = file;
    Ok(())
}

pub struct RFormatLayer {
    pub timer: CompactTimestamp,
}

impl RFormatLayer {
    /// Create a new RFormatLayer with a fresh timer
    pub fn new() -> Self {
        let start = GLOBAL_TIMER.get_or_init(|| Arc::new(Mutex::new(Instant::now())));
        Self {
            timer: CompactTimestamp {
                start: start.clone(),
            },
        }
    }

    /// Reset the global timer to the current instant
    pub fn reset_global_timer() {
        if let Some(timer) = GLOBAL_TIMER.get() {
            let mut start = timer.lock().unwrap();
            *start = Instant::now();
        }
    }
}

/// Storage attached to each span containing its formatted name + fields,
/// e.g. `cycle{cycle=3}` or just `cycle` if there are no fields.
struct SpanFields(String);

impl<S> Layer<S> for RFormatLayer
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(
        &self,
        attrs: &tracing::span::Attributes<'_>,
        id: &tracing::span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        let Some(span) = ctx.span(id) else { return };
        let mut visitor = FieldVisitor::default();
        attrs.record(&mut visitor);

        let formatted = format_span(span.name(), &visitor.fields);
        span.extensions_mut().insert(SpanFields(formatted));
    }

    fn on_record(
        &self,
        id: &tracing::span::Id,
        values: &tracing::span::Record<'_>,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        let Some(span) = ctx.span(id) else { return };
        let mut visitor = FieldVisitor::default();
        values.record(&mut visitor);
        if visitor.fields.is_empty() {
            return;
        }
        let formatted = format_span(span.name(), &visitor.fields);
        let mut ext = span.extensions_mut();
        if let Some(existing) = ext.get_mut::<SpanFields>() {
            existing.0 = formatted;
        } else {
            ext.insert(SpanFields(formatted));
        }
    }

    fn on_event(&self, event: &tracing::Event<'_>, ctx: tracing_subscriber::layer::Context<'_, S>) {
        // Format the event
        let metadata = event.metadata();
        let level = metadata.level();

        let (stdout_enabled, file_enabled) = {
            let config_handle = global_log_config();
            let config = config_handle.lock().unwrap();
            if !level_enabled(config.level, *level) {
                return;
            }
            (config.stdout, config.file.is_some())
        };

        if !stdout_enabled && !file_enabled {
            return;
        }

        // Create a visitor to extract field values
        let mut visitor = FieldVisitor::default();
        event.record(&mut visitor);

        // Format timestamp
        let mut time_buffer = String::new();
        {
            let mut writer = tracing_subscriber::fmt::format::Writer::new(&mut time_buffer);
            let _ = self.timer.format_time(&mut writer);
        }

        // Format the log message with color codes based on level
        let level_str = match *level {
            Level::ERROR => "\x1b[31mERROR\x1b[0m", // Red
            Level::WARN => "\x1b[33mWARN\x1b[0m",   // Yellow
            Level::INFO => "\x1b[32mINFO\x1b[0m",   // Green
            Level::DEBUG => "\x1b[36mDEBUG\x1b[0m", // Cyan
            Level::TRACE => "\x1b[35mTRACE\x1b[0m", // Magenta
        };

        // Walk the span scope from root -> current and collect formatted spans.
        // Each non-empty span is rendered as its own bracketed group, e.g. [Cycle 6].
        let mut span_str = String::new();
        let mut plain_span_str = String::new();
        if let Some(scope) = ctx.event_scope(event) {
            for span in scope.from_root() {
                let label = span
                    .extensions()
                    .get::<SpanFields>()
                    .map(|s| s.0.clone())
                    .unwrap_or_else(|| span.name().to_string());
                if label.is_empty() {
                    continue;
                }
                // Cyan color so the span context stands out from the message
                span_str.push_str(&format!(" \x1b[36m[{}]\x1b[0m", label));
                plain_span_str.push_str(&format!(" [{}]", label));
            }
        }

        let message = if visitor.fields.is_empty() {
            format!("[{}] [{}]{} (no fields)", time_buffer, level_str, span_str)
        } else {
            format!(
                "[{}] [{}]{} {}",
                time_buffer, level_str, span_str, visitor.fields
            )
        };

        let plain_level = match *level {
            Level::ERROR => "ERROR",
            Level::WARN => "WARN",
            Level::INFO => "INFO",
            Level::DEBUG => "DEBUG",
            Level::TRACE => "TRACE",
        };

        let plain_message = if visitor.fields.is_empty() {
            format!(
                "[{}] [{}]{} (no fields)",
                time_buffer, plain_level, plain_span_str
            )
        } else {
            format!(
                "[{}] [{}]{} {}",
                time_buffer, plain_level, plain_span_str, visitor.fields
            )
        };

        if file_enabled {
            let config_handle = global_log_config();
            let mut config = config_handle.lock().unwrap();
            if let Some(file) = config.file.as_mut() {
                let _ = writeln!(file, "{}", plain_message);
                let _ = file.flush();
            }
        }

        // Use rprintln to print to R console
        if stdout_enabled {
            rprintln!("{}", message);
        }
    }
}

fn level_enabled(configured: LoggingLevel, event: Level) -> bool {
    match configured {
        LoggingLevel::Error => matches!(event, Level::ERROR),
        LoggingLevel::Warn => matches!(event, Level::ERROR | Level::WARN),
        LoggingLevel::Info => matches!(event, Level::ERROR | Level::WARN | Level::INFO),
        LoggingLevel::Debug => matches!(
            event,
            Level::ERROR | Level::WARN | Level::INFO | Level::DEBUG
        ),
        LoggingLevel::Trace => true,
    }
}

/// Build a pretty span label from its name + recorded fields.
/// - Empty span name: use the fields verbatim (handles `info_span!("", "Cycle {}", n)`).
/// - No fields: just the span name.
/// - Both: `name{fields}`.
fn format_span(name: &str, fields: &str) -> String {
    match (name.is_empty(), fields.is_empty()) {
        (true, true) => String::new(),
        (true, false) => fields.to_string(),
        (false, true) => name.to_string(),
        (false, false) => format!("{}{{{}}}", name, fields),
    }
}

/// Visitor to extract field values from tracing events
#[derive(Default)]
pub struct FieldVisitor {
    fields: String,
}

impl tracing::field::Visit for FieldVisitor {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn fmt::Debug) {
        // Special handling for "message" field - just show the value
        if field.name() == "message" {
            if !self.fields.is_empty() {
                self.fields.push_str(" ");
            }
            self.fields.push_str(&format!("{:?}", value));
        } else {
            // For other fields, include the field name
            if !self.fields.is_empty() {
                self.fields.push_str(", ");
            }
            self.fields
                .push_str(&format!("{}={:?}", field.name(), value));
        }
    }
}

#[derive(Clone)]
pub struct CompactTimestamp {
    pub start: Arc<Mutex<Instant>>,
}

impl FormatTime for CompactTimestamp {
    fn format_time(
        &self,
        w: &mut tracing_subscriber::fmt::format::Writer<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        let start = self.start.lock().unwrap();
        let elapsed = start.elapsed();
        let hours = elapsed.as_secs() / 3600;
        let minutes = (elapsed.as_secs() % 3600) / 60;
        let seconds = elapsed.as_secs() % 60;

        write!(w, "{:02}h {:02}m {:02}s", hours, minutes, seconds)
    }
}
