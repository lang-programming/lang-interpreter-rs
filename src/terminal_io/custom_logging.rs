#[cfg(feature = "wasm")]
#[doc(hidden)]
pub mod wasm;

use std::fmt::Debug;
use crate::terminal_io::{Level, TerminalIO};

/// This trait is used to abstract logging functionality
///
/// This trait is only available if the `custom-logging` feature is enabled
pub trait Logger: Debug {
    fn log(&mut self, lvl: Level, time_label: &str, text: &str, tag: &str);
}

/// This [Logger] prints to standard out
///
/// This struct is only available if the `custom-logging` feature is enabled
#[derive(Debug)]
pub struct DefaultLogger;

impl DefaultLogger {
    pub fn new() -> Self {
        Self
    }
}

impl Default for DefaultLogger {
    fn default() -> Self {
        Self::new()
    }
}

impl Logger for DefaultLogger {
    fn log(&mut self, lvl: Level, time_label: &str, text: &str, tag: &str) {
        println!(
            "[{}][{}][{}]: {}",
            lvl.name(),
            time_label,
            tag,
            text,
        );
    }
}
