use extendr_api::Error as RExtendrError;
use serde::Serialize;
use serde_json::{Map, Value};
use std::fmt;

pub(crate) const BRIDGE_ERROR_PREFIX: &str = "PMETRICS_BRIDGE_ERROR=";
const BRIDGE_ERROR_SCHEMA_VERSION: u32 = 1;

pub(crate) type BridgeResult<T> = Result<T, BridgeError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum BridgeStage {
    Compile,
    Data,
    Settings,
    Runtime,
    Handoff,
}

#[derive(Debug, Clone, Serialize)]
struct BridgeErrorEnvelope {
    kind: &'static str,
    schema_version: u32,
    stage: BridgeStage,
    code: String,
    message: String,
    diagnostic: Option<String>,
    details: Map<String, Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct BridgeError {
    envelope: BridgeErrorEnvelope,
}

impl BridgeError {
    pub(crate) fn new(
        stage: BridgeStage,
        code: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            envelope: BridgeErrorEnvelope {
                kind: "error",
                schema_version: BRIDGE_ERROR_SCHEMA_VERSION,
                stage,
                code: code.into(),
                message: message.into(),
                diagnostic: None,
                details: Map::new(),
            },
        }
    }

    pub(crate) fn from_anyhow(
        stage: BridgeStage,
        code: impl Into<String>,
        error: impl Into<anyhow::Error>,
    ) -> Self {
        let error = error.into();
        Self::new(stage, code, error.to_string())
    }

    pub(crate) fn with_diagnostic(mut self, diagnostic: impl Into<String>) -> Self {
        self.envelope.diagnostic = Some(diagnostic.into());
        self
    }

    pub(crate) fn with_detail(mut self, key: &str, value: impl Serialize) -> Self {
        if let Ok(value) = serde_json::to_value(value) {
            self.envelope.details.insert(key.to_string(), value);
        }
        self
    }

    pub(crate) fn stage(&self) -> BridgeStage {
        self.envelope.stage
    }

    pub(crate) fn code(&self) -> &str {
        &self.envelope.code
    }

    pub(crate) fn message(&self) -> &str {
        &self.envelope.message
    }

    pub(crate) fn compatibility_string(&self) -> String {
        let mut rendered = self.envelope.message.clone();
        if let Some(diagnostic) = &self.envelope.diagnostic {
            rendered.push('\n');
            rendered.push_str(diagnostic);
        }

        let envelope_json = serde_json::to_string(&self.envelope).unwrap_or_else(|_| {
            format!(
                "{{\"kind\":\"error\",\"schema_version\":{},\"stage\":\"handoff\",\"code\":\"bridge_error_serialize_failed\",\"message\":\"Failed to serialize bridge error envelope\",\"diagnostic\":null,\"details\":{{}}}}",
                BRIDGE_ERROR_SCHEMA_VERSION
            )
        });

        rendered.push_str("\n\n");
        rendered.push_str(BRIDGE_ERROR_PREFIX);
        rendered.push_str(&envelope_json);
        rendered
    }
}

impl fmt::Display for BridgeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.envelope.message)?;
        if let Some(diagnostic) = &self.envelope.diagnostic {
            write!(f, "\n{}", diagnostic)?;
        }
        Ok(())
    }
}

impl std::error::Error for BridgeError {}

impl From<BridgeError> for RExtendrError {
    fn from(error: BridgeError) -> Self {
        RExtendrError::Other(error.compatibility_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bridge_error_serializes_stage_and_details() {
        let error = BridgeError::new(BridgeStage::Runtime, "fit_failed", "fit failed")
            .with_detail("subject_count", 12)
            .with_detail("algorithm", "npag");

        let rendered = error.compatibility_string();
        let payload = rendered
            .split_once(BRIDGE_ERROR_PREFIX)
            .expect("bridge error prefix should be present")
            .1;
        let json: Value = serde_json::from_str(payload).expect("bridge envelope json should parse");

        assert_eq!(json["kind"], "error");
        assert_eq!(json["schema_version"], BRIDGE_ERROR_SCHEMA_VERSION);
        assert_eq!(json["stage"], "runtime");
        assert_eq!(json["code"], "fit_failed");
        assert_eq!(json["details"]["subject_count"], 12);
        assert_eq!(json["details"]["algorithm"], "npag");
    }

    #[test]
    fn bridge_error_compatibility_string_keeps_human_message_first() {
        let error = BridgeError::new(BridgeStage::Compile, "compile_failed", "compile failed")
            .with_diagnostic("line 1: unexpected token");

        let rendered = error.compatibility_string();

        assert!(rendered.starts_with("compile failed\nline 1: unexpected token"));
        assert!(rendered.contains(BRIDGE_ERROR_PREFIX));
    }
}