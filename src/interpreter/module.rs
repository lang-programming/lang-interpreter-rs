use std::collections::HashMap;
use std::rc::Rc;
use crate::interpreter::data::DataObjectRef;

#[derive(Debug)]
pub struct Module {
    //TODO
}

impl Module {
    pub fn new() -> Self {
        Self {
            //TODO
        }
    }

    pub fn is_load(&self) -> bool {
        todo!()
    }

    pub fn exported_variables(&self) -> &HashMap<Rc<str>, DataObjectRef> {
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