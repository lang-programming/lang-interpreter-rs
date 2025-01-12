use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use ahash::AHashMap;
use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
use crate::interpreter::Interpreter;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ModuleType {
    Lang,
    Native,
}

#[derive(Debug, Clone)]
pub struct LangModuleConfiguration {
    name: Box<str>,
    description: Option<Box<str>>,
    version: Option<Box<str>>,

    min_supported_version: Option<Box<str>>,
    max_supported_version: Option<Box<str>>,

    supported_implementations: Option<Box<[Box<str>]>>,

    module_type: ModuleType,

    native_entry_point: Option<Box<str>>,
}

impl LangModuleConfiguration {
    pub fn parse_lmc(lmc: &str) -> Result<Self, InvalidModuleConfigurationException> {
        let mut data = AHashMap::new();

        for mut line in lmc.split("\n") {
            if let Some(index) = line.find("#") {
                line = &line[..index];
            }

            line = line.trim();

            if line.is_empty() {
                continue;
            }

            let tokens = line.splitn(2, " = ").
                    collect::<Vec<_>>();
            if tokens.len() != 2 {
                return Err(InvalidModuleConfigurationException::new(format!(
                    "Invalid configuration line (\" = \" is missing): {line}",
                )));
            }

            data.insert(tokens[0], tokens[1]);
        }

        let name = data.get("name");
        let Some(name) = name else {
            return Err(InvalidModuleConfigurationException::new(
                "Mandatory configuration of \"name\" is missing",
            ));
        };
        for c in name.bytes() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                return Err(InvalidModuleConfigurationException::new(
                    "The module name may only contain alphanumeric characters and underscores (_)",
                ));
            }
        }

        let supported_implementations = data.get("supportedImplementations");
        let supported_implementations = supported_implementations.map(|supported_implementations| {
            supported_implementations.split(",").
                    map(|ele| Box::from(ele.trim())).
                    collect::<Vec<Box<str>>>().
                    into_boxed_slice()
        });

        let module_type = data.get("moduleType");
        let Some(module_type) = module_type else {
            return Err(InvalidModuleConfigurationException::new(
                "Mandatory configuration of \"moduleType\" is missing",
            ));
        };

        let module_type = match *module_type {
            "lang" => ModuleType::Lang,
            "native" => ModuleType::Native,
            module_type => {
                return Err(InvalidModuleConfigurationException::new(format!(
                    "Invalid configuration for \"moduleType\" (Must be one of \"lang\", \"native\"): {module_type}",
                )));
            },
        };

        Ok(Self::new(
            Box::from(*name),
            data.get("description").map(|ele| Box::from(*ele)),
            data.get("version").map(|ele| Box::from(*ele)),

            data.get("minSupportedVersion").map(|ele| Box::from(*ele)),
            data.get("maxSupportedVersion").map(|ele| Box::from(*ele)),

            supported_implementations,

            module_type,

            data.get("nativeEntryPoint").map(|ele| Box::from(*ele)),
        ))
    }

    #[expect(clippy::too_many_arguments)]
    pub fn new(
        name: Box<str>,
        description: Option<Box<str>>,
        version: Option<Box<str>>,

        min_supported_version: Option<Box<str>>,
        max_supported_version: Option<Box<str>>,

        supported_implementations: Option<Box<[Box<str>]>>,

        module_type: ModuleType,

        native_entry_point: Option<Box<str>>,
    ) -> Self {
        Self {
            name,
            description,
            version,

            min_supported_version,
            max_supported_version,

            supported_implementations,

            module_type,

            native_entry_point,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    pub fn version(&self) -> Option<&str> {
        self.version.as_deref()
    }

    pub fn min_supported_version(&self) -> Option<&str> {
        self.min_supported_version.as_deref()
    }

    pub fn max_supported_version(&self) -> Option<&str> {
        self.max_supported_version.as_deref()
    }

    pub fn supported_implementations(&self) -> Option<&[Box<str>]> {
        self.supported_implementations.as_deref()
    }

    pub fn module_type(&self) -> ModuleType {
        self.module_type
    }

    pub fn native_entry_point(&self) -> Option<&str> {
        self.native_entry_point.as_deref()
    }
}

#[derive(Debug)]
pub struct ZipEntry {
    is_directory: bool,
}

impl ZipEntry {
    pub(crate) fn new(is_directory: bool) -> Self {
        Self { is_directory }
    }

    pub fn is_directory(&self) -> bool {
        self.is_directory
    }
}

#[derive(Debug)]
pub struct Module {
    file: Box<str>,

    load: RefCell<bool>,

    zip_entries: AHashMap<Box<str>, ZipEntry>,
    zip_data: AHashMap<Box<str>, Box<[u8]>>,

    lmc: LangModuleConfiguration,

    exported_functions: RefCell<Vec<Box<str>>>,
    exported_variables: RefCell<AHashMap<Rc<str>, DataObjectRef>>,

    loaded_native_modules: RefCell<AHashMap<Box<str>, Box<dyn NativeModule>>>,
}

impl Module {
    pub fn new(
        file: Box<str>,

        load: bool,

        zip_entries: AHashMap<Box<str>, ZipEntry>,
        zip_data: AHashMap<Box<str>, Box<[u8]>>,

        lmc: LangModuleConfiguration,
    ) -> Self {
        Self {
            file,

            load: RefCell::new(load),

            zip_entries,
            zip_data,

            lmc,

            exported_functions: Default::default(),
            exported_variables: Default::default(),

            loaded_native_modules: Default::default(),
        }
    }

    pub(crate) fn exported_functions(&self) -> &RefCell<Vec<Box<str>>> {
        &self.exported_functions
    }

    pub(crate) fn exported_variables(&self) -> &RefCell<AHashMap<Rc<str>, DataObjectRef>> {
        &self.exported_variables
    }

    #[expect(dead_code)]
    pub(crate) fn loaded_native_modules(&self) -> &RefCell<AHashMap<Box<str>, Box<dyn NativeModule>>> {
        &self.loaded_native_modules
    }

    pub fn file(&self) -> &str {
        &self.file
    }

    pub fn is_load(&self) -> bool {
        *self.load.borrow()
    }

    pub fn zip_entries(&self) -> &AHashMap<Box<str>, ZipEntry> {
        &self.zip_entries
    }

    pub fn zip_data(&self) -> &AHashMap<Box<str>, Box<[u8]>> {
        &self.zip_data
    }

    pub fn lang_module_configuration(&self) -> &LangModuleConfiguration {
        &self.lmc
    }

    pub(crate) fn set_load(&self, load: bool) {
        *self.load.borrow_mut() = load;
    }
}

pub trait NativeModule {
    /**
     * Will be called if the module is loaded
     *
     * @param args provided to "func.loadModule()" [modulePath included] separated with ARGUMENT_SEPARATORs
     * @return Will be returned for the "linker.loadModule()" call
     */
    fn load(&self, module: Rc<Module>, interpreter: &mut Interpreter, args: &[DataObjectRef]) -> OptionDataObjectRef;

    /**
     * Will be called if the module is unloaded
     *
     * @param args provided to "func.unloadModule()" [modulePath included] separated with ARGUMENT_SEPARATORs
     * @return Will be returned for the "linker.unloadModule()" call
     */
    fn unload(&self, module: Rc<Module>, interpreter: &mut Interpreter, args: &[DataObjectRef]) -> OptionDataObjectRef;
}

impl Debug for dyn NativeModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Native module at {:p}", self)
    }
}

#[derive(Debug)]
pub struct InvalidModuleConfigurationException {
    message: String
}

impl InvalidModuleConfigurationException {
    pub fn new(message: impl Into<String>) -> Self {
        Self { message: message.into() }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Display for InvalidModuleConfigurationException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl Error for InvalidModuleConfigurationException {}
