use std::error;
use std::fmt::{Display, Formatter};
use regex::{Error, Regex};

pub fn matches(text: &str, regex: &str) -> Result<bool, InvalidPatternSyntaxError> {
    let regex = Regex::new(regex)?;

    let match_result = regex.find(text);

    Ok(match_result.is_some_and(|match_result| {
        match_result.start() == 0 && match_result.end() == text.len()
    }))
}

pub fn split(text: &str, regex: &str, limit: Option<usize>) -> Result<Vec<String>, InvalidPatternSyntaxError> {
    let regex = Regex::new(regex)?;

    let limit = limit.unwrap_or(0);
    if limit == 0 {
        let match_result = regex.split(text).map(|str| str.to_string());

        Ok(match_result.collect::<Vec<_>>())
    }else {
        let match_result = regex.splitn(text, limit).map(|str| str.to_string());

        Ok(match_result.collect::<Vec<_>>())
    }
}

pub fn replace(text: &str, regex: &str, replacement: String) -> Result<String, InvalidPatternSyntaxError> {
    let regex = Regex::new(regex)?;

    let match_result = regex.replace_all(text, replacement);

    Ok(match_result.to_string())
}

#[derive(Debug)]
pub struct InvalidPatternSyntaxError {
    message: String
}

impl InvalidPatternSyntaxError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Display for InvalidPatternSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl error::Error for InvalidPatternSyntaxError {}

impl From<Error> for InvalidPatternSyntaxError {
    fn from(value: Error) -> Self {
        Self {
            message: value.to_string(),
        }
    }
}
