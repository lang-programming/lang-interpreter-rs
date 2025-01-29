#[cfg(feature = "custom-logging")]
#[doc(hidden)]
pub mod custom_logging;
#[cfg(feature = "custom-logging")]
#[doc(inline)]
pub use custom_logging::{Logger, DefaultLogger};

#[cfg(all(
    feature = "custom-logging",
    feature = "wasm",
))]
#[doc(inline)]
pub use custom_logging::wasm::JsConsoleLogger;

use std::error::Error;
use std::ffi::OsString;
use std::io;
use std::fs::File;
use std::io::Write;
use chrono::Local;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
#[repr(i8)]
pub enum Level {
    #[default]
    NotSet = -1,
    User = 0,
    Debug = 1,
    Config = 2,
    Info = 3,
    Warning = 4,
    Error = 5,
    Critical = 6,
}

impl Level {
    pub(crate) const VALUES: [Self; 8] = [
        Self::NotSet,
        Self::User,
        Self::Debug,
        Self::Config,
        Self::Info,
        Self::Warning,
        Self::Error,
        Self::Critical,
    ];

    pub fn level(&self) -> i8 {
        *self as i8
    }

    pub fn name(&self) -> String {
        match self {
            Level::NotSet => "Not set",
            Level::User => "User",
            Level::Debug => "Debug",
            Level::Config => "Config",
            Level::Info => "Info",
            Level::Warning => "Warning",
            Level::Error => "Error",
            Level::Critical => "Critical",
        }.to_string()
    }

    pub fn should_log(&self, log_level: Level) -> bool {
        log_level == Level::User || *self as i8 >= log_level as i8
    }
}

#[derive(Debug)]
pub struct TerminalIO {
    file: Option<File>,
    log_level: Level,

    #[cfg(feature = "custom-logging")]
    logger: Box<dyn Logger>,
}

impl TerminalIO {
    const TIME_FORMAT: &'static str = "%d.%m.%Y|%H:%M:%S";

    pub fn new(log_file: Option<OsString>) -> Result<Self, io::Error> {
        let file = if let Some(log_file) = log_file {
            Some(File::create(log_file)?)
        }else {
            None
        };

        #[cfg(not(feature = "custom-logging"))]
        {
            Ok(Self {
                file,
                log_level: Level::NotSet,
            })
        }
        #[cfg(feature = "custom-logging")]
        {
            Ok(Self {
                file,
                log_level: Level::NotSet,
                logger: Box::new(DefaultLogger),
            })
        }
    }

    #[cfg(feature = "custom-logging")]
    pub fn with_custom_logger(log_file: Option<OsString>, logger: Box<dyn Logger>) -> Result<Self, io::Error> {
        let file = if let Some(log_file) = log_file {
            Some(File::create(log_file)?)
        }else {
            None
        };

        Ok(Self {
            file,
            log_level: Level::NotSet,
            logger,
        })
    }

    pub fn set_level(&mut self, level: Level) {
        self.log_level = level;
    }

    fn log_internal(&mut self, lvl: Level, txt: &str, tag: &str) {
        if !lvl.should_log(self.log_level) {
            return;
        }

        let current_time = Local::now().format(Self::TIME_FORMAT).to_string();

        let log = format!(
            "[{}][{}][{}]: {}",
            lvl.name(),
            current_time,
            tag,
            txt,
        );

        #[cfg(not(feature = "custom-logging"))]
        {
            #[cfg(not(feature = "wasm"))]
            {
                println!("{log}");
            }
            #[cfg(feature = "wasm")]
            {
                match lvl {
                    Level::NotSet | Level::User | Level::Config => web_sys::console::log_1(&log.as_str().into()),
                    Level::Debug => web_sys::console::debug_1(&log.as_str().into()),
                    Level::Info => web_sys::console::info_1(&log.as_str().into()),
                    Level::Warning => web_sys::console::warn_1(&log.as_str().into()),
                    Level::Error | Level::Critical => web_sys::console::error_1(&log.as_str().into()),
                }
            }
        }
        #[cfg(feature = "custom-logging")]
        self.logger.log(lvl, &current_time, txt, tag);

        if let Some(file) = &mut self.file {
            let err = write!(file, "{log}");
            if let Err(e) = err {
                //Do not use the log_stack_trace method to avoid a stack overflow
                #[cfg(not(feature = "wasm"))]
                {
                    eprintln!("{e}");
                }
                #[cfg(feature = "wasm")]
                {
                    web_sys::console::error_1(&e.to_string().into());
                }
            }
        }
    }
    
    pub fn log(&mut self, lvl: Level, txt: impl Into<String>, tag: impl Into<String>) {
        self.log_internal(lvl, &txt.into(), &tag.into());
    }
    
    pub fn log_stack_trace(&mut self, error: Box<dyn Error>, tag: impl Into<String>) {
        self.log(Level::Error, error.to_string(), tag);
    }
}
