#[cfg(feature = "wasm-platform-api")]
pub mod wasm;
#[cfg(feature = "wasm-platform-api")]
pub use wasm::WASMPlatformAPI;

use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs;
use std::fs::File;
use std::io::{Error, Read, Write};
use std::path::{Path, PathBuf};
use crate::interpreter::data::function::native::NativeError;

/// This trait is used to abstract some io functionality
pub trait PlatformAPI: Debug {
    /// Return all files inside the folder located at `lang_path`
    ///
    /// # Arguments
    ///
    /// * `lang_path` - Path to the folder
    fn get_lang_files(&self, lang_path: &Path) -> Result<Vec<PathBuf>, Error>;

    /// Return the canonical path of the file located at `lang_file`
    ///
    /// # Arguments
    ///
    /// * `lang_file` - Path to the file
    fn get_lang_path(&self, lang_file: &Path) -> Result<PathBuf, Error>;

    /// Return the file name of the file located at `lang_file`
    ///
    /// # Arguments
    ///
    /// * `lang_file` - Path to the file
    fn get_lang_file_name(&self, lang_file: &Path) -> Option<OsString>;

    /// Return a Box<u8> for the file located at `lang_file`
    ///
    /// # Arguments
    ///
    /// * `lang_file` - Path to the file
    fn get_lang_reader(&self, lang_file: &Path) -> Result<Box<[u8]>, Error>;

    /// Writes a translation file
    ///
    /// # Arguments
    ///
    /// * `lang_file` - Path to the file
    /// * `translation_map` - The map of all translations
    fn write_lang_file(&self, lang_file: &Path, translation_map: HashMap<String, String>) -> Result<(), Error>;

    /// Return the value inputted by the user
    ///
    /// # Arguments
    ///
    /// * `text`: The text prompt to be shown to the user
    fn show_input_dialog(&self, text: &str) -> Result<String, NativeError>;
}

/// This used standard io operations.
///
/// The [show_input_dialog](DefaultPlatformAPI::show_input_dialog) method is not implemented and will always return an [Err],
/// because showing a dialog requires using a platform-dependent GUI API.
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
            if !file_type.is_dir() {
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

    fn get_lang_reader(&self, lang_file: &Path) -> Result<Box<[u8]>, Error> {
        let path = lang_file;

        let mut file = File::open(path)?;

        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;

        Ok(bytes.into_boxed_slice())
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

    /// This method is not implemented
    ///
    /// Trait doc: [show_input_dialog](PlatformAPI::show_input_dialog)
    fn show_input_dialog(&self, _text: &str) -> Result<String, NativeError> {
        Err(NativeError::new(
            "Not Implemented",
            None,
        ))
    }
}
