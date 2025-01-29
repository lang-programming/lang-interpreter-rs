use crate::terminal_io::{Level, Logger};

/// This [Logger] prints to the web browser console
#[derive(Debug)]
pub struct JsConsoleLogger;

impl JsConsoleLogger {
    pub fn new() -> Self {
        Self
    }
}

impl Default for JsConsoleLogger {
    fn default() -> Self {
        Self::new()
    }
}

impl Logger for JsConsoleLogger {
    fn log(&mut self, lvl: Level, time_label: &str, text: &str, tag: &str) {
        let log = format!(
            "[{}][{}][{}]: {}",
            lvl.name(),
            time_label,
            tag,
            text,
        );

        match lvl {
            Level::NotSet | Level::User | Level::Config => web_sys::console::log_1(&log.as_str().into()),
            Level::Debug => web_sys::console::debug_1(&log.as_str().into()),
            Level::Info => web_sys::console::info_1(&log.as_str().into()),
            Level::Warning => web_sys::console::warn_1(&log.as_str().into()),
            Level::Error | Level::Critical => web_sys::console::error_1(&log.as_str().into()),
        }
    }
}
