use std::error::Error;
use std::ffi::OsString;
use std::{fmt, io};
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
}

impl TerminalIO {
    const TIME_FORMAT: &'static str = "%d.%m.%Y|%H:%M:%S";

    pub fn new(log_file: Option<OsString>) -> Result<Self, io::Error> {
        let file = if let Some(log_file) = log_file {
            Some(File::create(log_file)?)
        }else {
            None
        };

        Ok(Self {
            file,
            log_level: Level::NotSet,
        })
    }

    pub fn set_level(&mut self, level: Level) {
        self.log_level = level;
    }

    fn log_internal(&mut self, lvl: Level, txt: impl Into<String>, tag: impl Into<String>, new_line: bool) {
        if !lvl.should_log(self.log_level) {
            return;
        }

        let current_time = Local::now().format(Self::TIME_FORMAT);

        let mut log = format!(
            "[{}][{}][{}]: {}",
            lvl.name(),
            current_time,
            tag.into(),
            txt.into(),
        );

        if new_line {
            log += "\n";
        }

        print!("{log}");
        
        if let Some(file) = &mut self.file {
            let err = write!(file, "{log}");
            if let Err(e) = err {
                //Doesn't use the log_stack_trace method to avoid a stack overflow
                eprintln!("{e}");
            }
        }
    }
    
    pub fn log(&mut self, lvl: Level, txt: impl Into<String>, tag: impl Into<String>) {
        self.log_internal(lvl, txt, tag, false);
    }
    
    pub fn logf(&mut self, lvl: Level, fmt: fmt::Arguments<'_>, tag: impl Into<String>) {
        self.log(lvl, fmt.to_string(), tag);
    }
    
    pub fn logln(&mut self, lvl: Level, txt: impl Into<String>, tag: impl Into<String>) {
        self.log_internal(lvl, txt, tag, true);
    }
    
    pub fn log_stack_trace(&mut self, error: Box<dyn Error>, tag: impl Into<String>) {
        self.logln(Level::Error, error.to_string(), tag);
    }
}
