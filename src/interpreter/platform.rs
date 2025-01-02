use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read, Write};
use std::path::{Path, PathBuf};
use crate::interpreter::data::function::native::NativeError;

pub trait PlatformAPI: Debug {
    /**
     * @param langPath Path to the folder
     * @return Return all files inside the folder located at langPath
     */
    fn get_lang_files(&self, lang_path: &Path) -> Result<Vec<PathBuf>, Error>;

    /**
     * @param langFile Path to the file
     * @return Return the canonical path of the file located at langFile
     */
    fn get_lang_path(&self, lang_file: &Path) -> Result<PathBuf, Error>;

    /**
     * @param langFile Path to the file
     * @return Return the file name of the file located at langFile
     */
    fn get_lang_file_name(&self, lang_file: &Path) -> Option<OsString>;

    /**
     * @param langFile Path to the file
     * @return Return a reader for the file
     */
    fn get_lang_buffered_reader(&self, lang_file: &Path) -> Result<Box<dyn BufRead>, Error>;

    /**
     * @param langFile Path to the file
     * @return Return an input stream for the file
     */
    fn get_lang_reader(&self, lang_file: &Path) -> Result<Box<dyn Read>, Error>;

    /**
     * @param langFile Path to the file
     * @param translationMap The Map of all translations
     * @return Return true if successful else false (Return false if not implemented)
     */
    fn write_lang_file(&self, lang_file: &Path, translation_map: HashMap<String, String>) -> Result<(), Error>;

    /**
     * @param text The text prompt to be shown to the user
     * @return Return the value inputed by the user
     * @throws Exception Throw any exception if not implemented or if any other error occurred
     */
    fn show_input_dialog(&self, text: &str) -> Result<String, NativeError>;
}

#[derive(Debug)]
pub struct DefaultPlatformAPI;

impl DefaultPlatformAPI {
    pub fn new() -> Self {
        Self
    }
}

impl Default for DefaultPlatformAPI {
    fn default() -> Self {
        Self::new()
    }
}

impl PlatformAPI for DefaultPlatformAPI {
    fn get_lang_files(&self, lang_path: &Path) -> Result<Vec<PathBuf>, Error> {
        let files = fs::read_dir(lang_path)?;

        let mut lang_files = Vec::new();

        for file in files {
            let file = file?;
            let file_type = file.file_type()?;
            if file_type.is_file() {
                let file_name = file.file_name();
                if let Some(file_name) = file_name.to_str() {
                    if file_name.to_ascii_lowercase().ends_with(".lang") {
                        lang_files.push(file.path());
                    }
                }
            }
        }

        Ok(lang_files)
    }

    fn get_lang_path(&self, lang_file: &Path) -> Result<PathBuf, Error> {
        let path = lang_file;
        let mut path = path.parent().unwrap_or(Path::new(""));
        if path == Path::new("") {
            path = Path::new("./");
        }

        let canonical_path = path.canonicalize();
        if let Ok(canonical_path) = canonical_path {
            Ok(canonical_path)
        }else {
            std::path::absolute(path)
        }
    }

    fn get_lang_file_name(&self, lang_file: &Path) -> Option<OsString> {
        let path = lang_file;

        path.file_name().map(|str| str.to_os_string())
    }

    fn get_lang_buffered_reader(&self, lang_file: &Path) -> Result<Box<dyn BufRead>, Error> {
        let path = lang_file;

        let file = File::open(path)?;
        let reader = BufReader::new(file);

        Ok(Box::new(reader))
    }

    fn get_lang_reader(&self, lang_file: &Path) -> Result<Box<dyn Read>, Error> {
        let path = lang_file;

        let file = File::open(path)?;

        Ok(Box::new(file))
    }

    fn write_lang_file(&self, lang_file: &Path, translation_map: HashMap<String, String>) -> Result<(), Error> {
        let path = lang_file;

        let mut file = File::create(path)?;

        for (translation_key, translation_value) in translation_map {
            //For multiline
            let translation_value = translation_value.replace("\n", "\\n");

            writeln!(file, "{translation_key} = {translation_value}")?;
        }

        Ok(())
    }

    /**
     * This method is not implemented
     */
    fn show_input_dialog(&self, _text: &str) -> Result<String, NativeError> {
        Err(NativeError::new(
            "Not Implemented",
            None,
        ))
    }
}
