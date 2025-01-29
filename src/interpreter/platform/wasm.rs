use std::collections::HashMap;
use std::ffi::OsString;
use std::io::{Error, ErrorKind};
use std::path::{Path, PathBuf};
use web_sys::{js_sys, Url, WorkerLocation, XmlHttpRequest, XmlHttpRequestResponseType};
use web_sys::js_sys::{ArrayBuffer, Uint8Array};
use web_sys::wasm_bindgen::JsValue;
use crate::interpreter::data::function::native::NativeError;
use crate::interpreter::platform::PlatformAPI;


/// This used fetch for io operations.
///
/// <div class="warning">
///
/// [WASMPlatformAPI] can only be used if the [Interpreter](crate::interpreter::Interpreter) is used within a [Web Worker].
///
/// </div>
///
/// The following functions are not implemented and will always return an [Err]:
///
/// * [get_lang_files](PlatformAPI::get_lang_files)
/// * [write_lang_file](PlatformAPI::write_lang_file)
/// * [show_input_dialog](PlatformAPI::show_input_dialog)
///
/// [Web Worker]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API
#[derive(Debug)]
pub struct WASMPlatformAPI;

impl WASMPlatformAPI {
    pub fn new() -> Self {
        Self
    }
}

impl Default for WASMPlatformAPI {
    fn default() -> Self {
        Self::new()
    }
}

impl PlatformAPI for WASMPlatformAPI {
    /// This method is not implemented
    ///
    /// Trait doc: [get_lang_files](PlatformAPI::get_lang_files)
    fn get_lang_files(&self, _: &Path) -> Result<Vec<PathBuf>, Error> {
        Err(Error::new(
            ErrorKind::Unsupported,
            "Not Implemented",
        ))
    }

    fn get_lang_path(&self, lang_file: &Path) -> Result<PathBuf, Error> {
        let path = lang_file;
        let mut path = path.parent().unwrap_or(Path::new(""));
        if path == Path::new("") {
            path = Path::new("./");
        }

        let path = &*path.to_string_lossy();

        let location = js_sys::Reflect::get(js_sys::global().as_ref(), &"location".into()).map_err(|err| Error::new(
            ErrorKind::Other,
            err.as_string().as_deref().unwrap_or("Error"),
        ))?;

        let location = WorkerLocation::from(location);

        //TODO fix url creation (Double slash: "//")
        let location = Url::new_with_base(path, &location.href()).map_err(|err| Error::new(
            ErrorKind::Other,
            err.as_string().as_deref().unwrap_or("Error"),
        ))?;

        Ok(PathBuf::from(location.pathname()))
    }

    fn get_lang_file_name(&self, lang_file: &Path) -> Option<OsString> {
        let path = lang_file;

        path.file_name().map(|str| str.to_os_string())
    }

    fn get_lang_reader(&self, lang_file: &Path) -> Result<Box<[u8]>, Error> {
        #[inline(always)]
        fn internal(lang_file: &Path) -> Result<Box<[u8]>, JsValue> {
            let request = XmlHttpRequest::new()?;
            request.set_response_type(XmlHttpRequestResponseType::Arraybuffer);
            request.open_with_async("GET", &lang_file.to_string_lossy(), false)?;
            request.send()?;

            let status_code = request.status().unwrap_or(500);
            if status_code != 200 {
                return Err(format!("Invalid response code: {status_code}").into());
            }

            let ret = request.response()?;
            let ret = ArrayBuffer::from(ret);
            let ret = Uint8Array::new(&ret);

            let mut bytes = vec![0; ret.length() as usize];
            ret.copy_to(&mut bytes);

            Ok(bytes.into_boxed_slice())
        }

        internal(lang_file).map_err(|err| Error::new(
            ErrorKind::Other,
            err.as_string().as_deref().unwrap_or("Error"),
        ))
    }

    /// This method is not implemented
    ///
    /// Trait doc: [write_lang_file](PlatformAPI::write_lang_file)
    fn write_lang_file(&self, _: &Path, _: HashMap<String, String>) -> Result<(), Error> {
        Err(Error::new(
            ErrorKind::Unsupported,
            "Not Implemented",
        ))
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

    fn print(&mut self, text: &str) {
        //TODO cache and do the printing in println only
        self.println(text);
    }

    fn println(&mut self, text: &str) {
        //TODO cache and print in println

        if text.is_empty() {
            web_sys::console::log_0();
        }else {
            web_sys::console::log_1(&text.into());
        }
    }

    fn print_error(&mut self, text: &str) {
        //TODO cache and do the printing in println_error only
        self.println_error(text);
    }

    fn println_error(&mut self, text: &str) {
        //TODO cache and print in println

        if text.is_empty() {
            web_sys::console::error_0();
        }else {
            web_sys::console::error_1(&text.into());
        }
    }
}
