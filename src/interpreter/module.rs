use std::rc::Rc;
use ahash::AHashMap;
use crate::interpreter::data::DataObjectRef;

#[derive(Debug)]
pub struct Module {
    //TODO
}

impl Module {
    pub fn new() -> Self {
        #![expect(clippy::new_without_default)]

        Self {
            //TODO
        }
    }

    pub fn is_load(&self) -> bool {
        todo!()
    }

    pub fn exported_variables(&self) -> &AHashMap<Rc<str>, DataObjectRef> {
        todo!()
    }
}

#[derive(Debug)]
pub struct ModuleManager {
    //TODO
}

impl ModuleManager {
    pub fn new() -> Self {
        Self {
            //TODO
        }
    }
}

impl Default for ModuleManager {
    fn default() -> Self {
        Self::new()
    }
}
