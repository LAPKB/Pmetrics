use std::fmt;

use extendr_api::prelude::*;
use extendr_api::rprintln;
use tracing::Level;
use tracing_subscriber::Layer;

pub struct RFormatLayer;

impl<S> Layer<S> for RFormatLayer
where
    S: tracing::Subscriber,
{
    fn on_event(
        &self,
        event: &tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        // Format the event
        let metadata = event.metadata();
        let level = metadata.level();
        let target = metadata.target();

        // Create a visitor to extract field values
        let mut visitor = FieldVisitor::default();
        event.record(&mut visitor);

        // Format the log message with color codes based on level
        let level_str = match *level {
            Level::ERROR => "\x1b[31mERROR\x1b[0m", // Red
            Level::WARN => "\x1b[33mWARN\x1b[0m",   // Yellow
            Level::INFO => "\x1b[32mINFO\x1b[0m",   // Green
            Level::DEBUG => "\x1b[36mDEBUG\x1b[0m", // Cyan
            Level::TRACE => "\x1b[35mTRACE\x1b[0m", // Magenta
        };

        let message = if visitor.fields.is_empty() {
            format!("[{}] {}: (no fields)", level_str, target)
        } else {
            format!("[{}] {}: {}", level_str, target, visitor.fields)
        };

        // Use rprintln to print to R console
        rprintln!("{}", message);
    }
}

/// Visitor to extract field values from tracing events
#[derive(Default)]
pub struct FieldVisitor {
    fields: String,
}

impl tracing::field::Visit for FieldVisitor {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn fmt::Debug) {
        if !self.fields.is_empty() {
            self.fields.push_str(", ");
        }
        self.fields
            .push_str(&format!("{}={:?}", field.name(), value));
    }
}
