use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::ptr;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{LazyLock, Mutex};
use gc::{Finalize, Gc, GcCell, Trace};
use crate::interpreter::data::{DataObject, DataObjectRef, DataTypeConstraint, DataTypeConstraintError, FunctionPointerObjectRef, LangObjectRef, OptionLangObjectRef, Visibility};
use crate::interpreter::data::function::{FunctionPointerObject, InternalFunction};
use crate::interpreter::Interpreter;
use crate::utils;

static NEXT_CLASS_ID: AtomicI64 = AtomicI64::new(0);
pub(super) static CLASS_ID_TO_SUPER_CLASS_IDS: LazyLock<Mutex<HashMap<i64, Vec<i64>>>> = LazyLock::new(Default::default);

#[derive(Debug, Trace, Finalize)]
pub struct LangObjectObject {
    super_level: usize,

    members: Vec<DataObjectRef>,

    class_base_definition: LangObjectRef,

    /**
     * false for classes and uninitialized objects, true for objects where post construct was called
     */
    initialized: bool,
}

impl LangObjectObject {
    pub fn new(
        super_level: usize,
        members: Vec<DataObjectRef>,
        class_base_definition: LangObjectRef,
        initialized: bool,
    ) -> Self {
        Self { super_level, members, class_base_definition, initialized }
    }

    pub fn super_level(&self) -> usize {
        self.super_level
    }

    pub fn members(&self) -> &[DataObjectRef] {
        &self.members
    }

    pub fn class_base_definition(&self) -> &LangObjectRef {
        &self.class_base_definition
    }

    pub fn initialized(&self) -> bool {
        self.initialized
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct MemberDefinition {
    name: Box<str>,

    //SAFETY: There are no GC reference inside DataTypeConstraint
    #[unsafe_ignore_trace]
    type_constraint: Option<Box<DataTypeConstraint>>,

    final_flag: bool,

    //SAFETY: There are no GC reference inside Visibility
    #[unsafe_ignore_trace]
    member_of_visibility: Visibility,
    member_of_class: LangObjectRef,
}

impl MemberDefinition {
    pub fn new(
        name: &str,
        type_constraint: Option<Box<DataTypeConstraint>>,
        final_flag: bool,
        member_of_visibility: Visibility,
        member_of_class: LangObjectRef,
    ) -> Self {
        Self {
            name: Box::from(name),
            type_constraint,
            final_flag,
            member_of_visibility,
            member_of_class,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_constraint(&self) -> Option<&DataTypeConstraint> {
        self.type_constraint.as_deref()
    }

    pub fn final_flag(&self) -> bool {
        self.final_flag
    }

    pub fn member_of_visibility(&self) -> Visibility {
        self.member_of_visibility
    }

    pub fn member_of_class(&self) -> &LangObjectRef {
        &self.member_of_class
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct LangObjectClass {
    members: Vec<MemberDefinition>,

    /**
     * If size = 0: This is the base object<br>
     * If size > 0: This is a normal object
     */
    parent_classes: Vec<LangObjectRef>,
}

impl LangObjectClass {
    pub fn new(members: Vec<MemberDefinition>, parent_classes: Vec<LangObjectRef>) -> Self {
        Self { members, parent_classes }
    }

    pub fn members(&self) -> &[MemberDefinition] {
        &self.members
    }

    pub fn parent_classes(&self) -> &[LangObjectRef] {
        &self.parent_classes
    }
}

#[derive(Debug, Trace, Finalize)]
pub enum LangObjectData {
    Class(LangObjectClass),
    Object(LangObjectObject),
}

#[derive(Debug, Trace, Finalize)]
pub struct LangObject {
    pub(super) class_id: i64,

    class_name: Option<Box<str>>,

    static_members: Vec<DataObjectRef>,

    methods: HashMap<Box<str>, FunctionPointerObjectRef>,
    constructors: FunctionPointerObjectRef,

    data: LangObjectData,
}

impl LangObject {
    //Thread local is ok for now, because data objects are neither Send nor Sync
    thread_local! {
        static DUMMY_CLASS_DEFINITION_CLASS: LangObjectRef = {
            let constructor = |_interpreter: &mut Interpreter, _: LangObjectRef| {};
            let constructor = FunctionPointerObject::from(crate::lang_func!(
                constructor,
                crate::lang_func_metadata!(
                    name="construct",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                ),
            ));

            let constructor_visibility = vec![Visibility::Public];

            LangObject::new_class(
                Some("<class-definition>"), Vec::new(), Vec::new(),
                HashMap::new(), HashMap::new(), HashMap::new(), constructor, constructor_visibility,
                Vec::new(),
            ).unwrap()
        };

        static OBJECT_CLASS: LangObjectRef = {
            let mut methods = HashMap::new();
            let mut method_override_flags = HashMap::new();
            let mut method_visibility = HashMap::new();

            let get_class_method = |_interpreter: &mut Interpreter, this: LangObjectRef| -> DataObjectRef {
                let mut class_object = DataObject::new();
                class_object.set_object(this.borrow().base_definition().unwrap()).unwrap();

                DataObjectRef::new(class_object)
            };
            let get_class_method = FunctionPointerObject::from(crate::lang_func!(
                get_class_method,
                crate::lang_func_metadata!(
                    name="mp.getClass",
                    return_type_constraint(
                        allowed=["OBJECT"],
                    ),
                ),
            ));

            methods.insert(Box::from("mp.getClass"), get_class_method);
            method_override_flags.insert(Box::from("mp.getClass"), vec![false]);
            method_visibility.insert(Box::from("mp.getClass"), vec![Visibility::Public]);

            let constructor = |_interpreter: &mut Interpreter, _: LangObjectRef| {};
            let constructor = FunctionPointerObject::from(crate::lang_func!(
                constructor,
                crate::lang_func_metadata!(
                    name="construct",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                ),
            ));

            let constructor_visibility = vec![Visibility::Public];

            LangObject::new_class_internal(
                true, Some("&Object"), Vec::new(), Vec::new(),
                methods, method_override_flags, method_visibility, constructor, constructor_visibility,
                Vec::new(),
            ).unwrap()
        };
    }

    /// Returns the "<class-definition>" lang initialization class
    pub(crate) fn dummy_class_definition_class() -> LangObjectRef {
        Self::DUMMY_CLASS_DEFINITION_CLASS.with(Clone::clone)
    }

    /// Returns the "&Object" lang object base class
    pub fn object_class() -> LangObjectRef {
        Self::OBJECT_CLASS.with(Clone::clone)
    }

    #[expect(clippy::too_many_arguments)]
    pub fn new_class(
        class_name: Option<&str>,

        static_members: Vec<DataObject>,

        members: Vec<MemberDefinition>,

        methods: HashMap<Box<str>, FunctionPointerObject>,
        method_override_flags: HashMap<Box<str>, Vec<bool>>,
        method_visibility: HashMap<Box<str>, Vec<Visibility>>,

        constructors: FunctionPointerObject,
        constructor_visibility: Vec<Visibility>,

        parent_classes: Vec<LangObjectRef>,
    ) -> Result<LangObjectRef, DataTypeConstraintError> {
        Self::new_class_internal(
            false,
            class_name,
            static_members,
            members,
            methods,
            method_override_flags,
            method_visibility,
            constructors,
            constructor_visibility,
            parent_classes,
        )
    }

    #[expect(clippy::too_many_arguments)]
    fn new_class_internal(
        is_base_object: bool,
        class_name: Option<&str>,

        static_members: Vec<DataObject>,

        members: Vec<MemberDefinition>,

        methods: HashMap<Box<str>, FunctionPointerObject>,
        method_override_flags: HashMap<Box<str>, Vec<bool>>,
        method_visibility: HashMap<Box<str>, Vec<Visibility>>,

        constructors: FunctionPointerObject,
        constructor_visibility: Vec<Visibility>,

        parent_classes: Vec<LangObjectRef>,
    ) -> Result<LangObjectRef, DataTypeConstraintError> {
        let parent_classes = if is_base_object {
            Vec::new()
        }else if parent_classes.is_empty() {
            vec![Self::object_class()]
        }else {
            for parent_class in parent_classes.iter() {
                if !parent_class.borrow().is_class() {
                    return Err(DataTypeConstraintError::with_message(
                        "Parent classes must not be an object",
                    ));
                }
            }

            if parent_classes.len() == 1 {
                parent_classes
            }else {
                return Err(DataTypeConstraintError::with_message(
                    "Multi-inheritance is not allowed",
                ));
            }
        };

        for static_member in static_members.iter() {
            let Some(static_member_name) = static_member.variable_name() else {
                return Err(DataTypeConstraintError::with_message(
                    "Variable name must not be None for static member",
                ));
            };

            for parent_class in parent_classes.iter() {
                let static_members = &parent_class.borrow().static_members;

                for super_static_member in static_members {
                    if let Some(super_static_member_name) = &super_static_member.borrow().variable_name {
                        if **super_static_member_name == *static_member_name {
                            return Err(DataTypeConstraintError::with_message(format!(
                                "Super class static member must not be shadowed (For static member \"{static_member_name}\")",
                            )));
                        }
                    }
                }
            }
        }

        let class_id = NEXT_CLASS_ID.fetch_add(1, Ordering::Relaxed);
        {
            let mut class_id_to_super_class_ids = CLASS_ID_TO_SUPER_CLASS_IDS.lock().unwrap();
            
            let mut super_class_ids = HashSet::new();
            super_class_ids.insert(class_id);
            for parent_class in &parent_classes {
                let parent_class_id = parent_class.borrow().class_id;
                
                for class_id in &class_id_to_super_class_ids[&parent_class_id] {
                    super_class_ids.insert(*class_id);
                }
            }
            
            class_id_to_super_class_ids.insert(class_id, Vec::from_iter(super_class_ids));
        }

        let new_value = Gc::new(GcCell::new(Self {
            class_id,

            class_name: class_name.map(Box::from),

            static_members: Vec::new(),

            methods: HashMap::new(),
            constructors: Gc::new(FunctionPointerObject::new_dummy_definition()),

            data: LangObjectData::Class(LangObjectClass::new(
                Vec::new(),
                parent_classes.clone(),
            )),
        }));

        let mut static_members = static_members;
        for static_member in static_members.iter_mut() {
            static_member.set_member_of_class(Some(new_value.clone()));

            if let Some(function_pointer) = static_member.function_pointer_value() {
                let func = function_pointer.copy_with_mapped_functions(|function| {
                    if let Some(member_of_class) = function.member_of_class() {
                        if ptr::eq(member_of_class.borrow().deref(), Self::dummy_class_definition_class().borrow().deref()) {
                            return InternalFunction::copy_with_class_member_attributes(
                                function,
                                new_value.clone(),
                                Visibility::Public,
                            );
                        }
                    }

                    function.clone()
                });

                static_member.set_function_pointer(Gc::new(func))?;
            }
        }

        let mut static_members = static_members.into_iter().
                map(DataObjectRef::new).
                collect::<Vec<_>>();

        //TODO allow multi-inheritance (Check if a static member is in both super classes)
        for parent_class in parent_classes.iter() {
            for super_static_member in &parent_class.borrow().static_members {
                //No copies (static members must be the same reference across all uses)
                static_members.push(super_static_member.clone());
            }
        }

        for member in members.iter() {
            for parent_class in parent_classes.iter() {
                let static_members = &parent_class.borrow().static_members;
                for super_static_member in static_members {
                    if let Some(super_static_member_name) = &super_static_member.borrow().variable_name {
                        if *super_static_member_name == member.name {
                            return Err(DataTypeConstraintError::with_message(format!(
                                "Super class static member must not be shadowed (For member \"{}\")",
                                member.name,
                            )));
                        }
                    }
                }

                let parent_class = parent_class.borrow();
                let members = parent_class.member_definitions().unwrap();
                for super_member in members {
                    let super_member_name = &super_member.name;
                    if *super_member_name == member.name {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "Super class member must not be shadowed (For member \"{}\")",
                            member.name,
                        )));
                    }
                }
            }

            for static_member in static_members.iter() {
                if let Some(static_member_name) = &static_member.borrow().variable_name {
                    if *static_member_name == member.name {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "Static members must not be shadowed (For member \"{}\")",
                            member.name,
                        )));
                    }
                }
            }
        }

        //TODO allow multi-inheritance (Check if a member is in both super classes)
        let mut members_new = Vec::new();
        for member in members.iter() {
            members_new.push(MemberDefinition::new(
                &member.name,
                member.type_constraint.clone(),
                member.final_flag,
                member.member_of_visibility,
                new_value.clone(),
            ));
        }

        for parent_class in parent_classes.iter() {
            for member in parent_class.borrow().member_definitions().unwrap() {
                members_new.push(MemberDefinition::new(
                    &member.name,
                    member.type_constraint.clone(),
                    member.final_flag,
                    member.member_of_visibility,
                    parent_class.clone(),
                ));
            }
        }

        let mut methods_new = methods;
        for (method_name, overloaded_functions) in methods_new.iter_mut() {
            if overloaded_functions.get_overloaded_function_count() == 0 {
                return Err(DataTypeConstraintError::with_message(format!(
                    "No method present for method (For method \"{method_name}\")",
                )));
            }

            let Some(overloaded_method_override_flags) = method_override_flags.get(method_name) else {
                return Err(DataTypeConstraintError::with_message(format!(
                    "Missing method override flags (For method \"{method_name}\")",
                )));
            };

            if overloaded_method_override_flags.len() != overloaded_functions.get_overloaded_function_count() {
                return Err(DataTypeConstraintError::with_message(format!(
                    "Count of method override flags does not match overloaded function count (For method \"{method_name}\")",
                )));
            }

            let Some(overloaded_method_visibility) = method_visibility.get(method_name) else {
                return Err(DataTypeConstraintError::with_message(format!(
                    "Missing method visibility (For method \"{method_name}\")",
                )));
            };

            if overloaded_method_visibility.len() != overloaded_functions.get_overloaded_function_count() {
                return Err(DataTypeConstraintError::with_message(format!(
                    "Count of method visibility does not match overloaded function count (For method \"{method_name}\")",
                )));
            }

            //Set visibility
            let functions = overloaded_functions.functions_mut();
            for (i, function) in functions.iter_mut().enumerate() {
                *function = function.copy_with_class_member_attributes(
                    new_value.clone(),
                    overloaded_method_visibility[i],
                ).copy_with_super_level(0);
            }

            //Check override flag
            let function_signatures = functions.iter().
                    map(|internal_function| internal_function.function()).
                    collect::<Vec<_>>();

            for func_a in function_signatures.iter() {
                for func_b in function_signatures.iter() {
                    //Do not compare to same function signature
                    if ptr::eq(*func_a, *func_b) {
                        continue;
                    }

                    if utils::are_function_signatures_equals(func_a, func_b) {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "Duplicated function signatures: \"{method_name}{}\" and \"{method_name}{}\"",
                            func_a.to_function_signature_string(),
                            func_b.to_function_signature_string(),
                        )));
                    }
                }
            }
        }

        for method_name in methods_new.keys() {
            let function_var_name = "fp.".to_string() + &method_name[3..];

            for parent_class in parent_classes.iter() {
                let static_members = &parent_class.borrow().static_members;
                for super_static_member in static_members {
                    if let Some(super_static_member_name) = &super_static_member.borrow().variable_name {
                        if **super_static_member_name == *function_var_name {
                            return Err(DataTypeConstraintError::with_message(format!(
                                "\"fp.\" static members of a super class must not be shadowed by method (For method \"{}\" and member \"{}\")",
                                method_name, function_var_name,
                            )));
                        }
                    }
                }

                let parent_class = parent_class.borrow();
                let members = parent_class.member_definitions().unwrap();
                for super_member in members {
                    let super_member_name = &super_member.name;
                    if **super_member_name == function_var_name {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "\"fp.\" members of a super class not be shadowed by method (For method \"{}\" and member \"{}\")",
                            method_name, function_var_name,
                        )));
                    }
                }
            }

            for static_member in static_members.iter() {
                if let Some(static_member_name) = &static_member.borrow().variable_name {
                    if **static_member_name == function_var_name {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "\"fp.\" static members must not be shadowed by method (For method \"{}\" and member \"{}\")",
                            method_name, function_var_name,
                        )));
                    }
                }
            }

            for member in members.iter() {
                let super_static_member_name = &member.name;
                if **super_static_member_name == function_var_name {
                    return Err(DataTypeConstraintError::with_message(format!(
                        "\"fp.\" members must not be shadowed by method (For method \"{}\" and member \"{}\")",
                        method_name, function_var_name,
                    )));
                }
            }
        }

        //TODO allow multi-inheritance (Check if a method with the same function signature is in both super classes)
        {
            for parent_class in parent_classes.iter() {
                for (method_name, super_overloaded_methods) in &parent_class.borrow().methods {
                    let overloaded_methods = methods_new.get_mut(method_name);
                    let Some(overloaded_methods) = overloaded_methods else {
                        methods_new.insert(
                            method_name.clone(),
                            super_overloaded_methods.copy_with_mapped_functions(|internal_function|
                                    internal_function.copy_with_super_level(internal_function.super_level().unwrap() + 1)),
                        );

                        continue;
                    };

                    //Override check
                    let override_flags = &method_override_flags[method_name];
                    let function_signatures = overloaded_methods.functions().iter().
                            map(|internal_function| internal_function.function()).
                            collect::<Vec<_>>();

                    let super_function_signatures = super_overloaded_methods.functions().iter().
                            map(|internal_function| internal_function.function()).
                            collect::<Vec<_>>();

                    for (i, func_a) in function_signatures.into_iter().enumerate() {
                        let mut is_any_equals = false;
                        for func_b in &super_function_signatures {
                            if utils::are_function_signatures_equals(func_a, func_b) {
                                is_any_equals = true;
                                break;
                            }
                        }

                        if override_flags[i] {
                            if !is_any_equals {
                                return Err(DataTypeConstraintError::with_message(format!(
                                    "No method for override was found for function signature: \"{method_name}{}\"",
                                    func_a.to_function_signature_string(),
                                )));
                            }
                        }else if is_any_equals {
                            return Err(DataTypeConstraintError::with_message(format!(
                                "Method was not declared as override for function signature: \"{method_name}{}\"",
                                func_a.to_function_signature_string(),
                            )));
                        }
                    }

                    *overloaded_methods = overloaded_methods.copy_with_added_functions(
                        &super_overloaded_methods.copy_with_mapped_functions(
                            |internal_function| internal_function.
                                    copy_with_super_level(internal_function.super_level().unwrap() + 1)
                        ),
                    );
                }
            }
        }

        if constructors.get_overloaded_function_count() == 0 {
            return Err(DataTypeConstraintError::with_message(
                "There must be at least one constructor",
            ));
        }

        if constructor_visibility.len() != constructors.get_overloaded_function_count() {
            return Err(DataTypeConstraintError::with_message(
                "Count of constructor visibility does not match overloaded constructor count",
            ));
        }

        let mut constructors = constructors.
                copy_with_function_name("construct").
                copy_with_mapped_functions(|internal_functions| internal_functions.
                        copy_with_super_level(0));
        for (i, constructor) in constructors.functions_mut().iter_mut().
                enumerate() {
            *constructor = constructor.copy_with_class_member_attributes(
                new_value.clone(),
                constructor_visibility[i],
            );
        }

        {
            let function_signatures = constructors.functions().iter().
                    map(|internal_function| internal_function.function()).
                    collect::<Vec<_>>();

            for func_a in function_signatures.iter() {
                for func_b in function_signatures.iter() {
                    //Do not compare to same function signature
                    if ptr::eq(*func_a, *func_b) {
                        continue;
                    }

                    if utils::are_function_signatures_equals(func_a, func_b) {
                        return Err(DataTypeConstraintError::with_message(format!(
                            "Duplicated function signatures: \"construct{}\" and \"construct{}\"",
                            func_a.to_function_signature_string(),
                            func_b.to_function_signature_string(),
                        )));
                    }
                }
            }
        }

        //TODO allow super constructor call from constructor [Dynamically bind super constructors to this]

        {
            let mut new_value_ref = new_value.borrow_mut();

            new_value_ref.static_members = static_members;
            let LangObjectData::Class(new_value_data) = &mut new_value_ref.data else {
                panic!();
            };

            new_value_data.members = members_new;
            new_value_ref.methods = HashMap::from_iter(methods_new.into_iter().
                    map(|(key, value)| (key, Gc::new(value))));
            new_value_ref.constructors = Gc::new(constructors);
        }

        Ok(new_value)
    }

    /**
     * The constructor must be called separately, afterward postConstructor must be called
     */
    pub fn new_object(
        class_base_definition: &LangObjectRef,
    ) -> Result<LangObjectRef, DataTypeConstraintError> {
        if !class_base_definition.borrow().is_class() {
            return Err(DataTypeConstraintError::with_message(
                "Class base definition must be a class",
            ));
        }

        let new_value = Gc::new(GcCell::new(Self {
            class_id: class_base_definition.borrow().class_id,

            class_name: class_base_definition.borrow().class_name.clone(),

            //No copies, because static members should be the same across all objects
            static_members: class_base_definition.borrow().static_members.clone(),

            methods: HashMap::new(),
            constructors: Gc::new(FunctionPointerObject::new_dummy_definition()),

            data: LangObjectData::Object(LangObjectObject::new(
                0,
                Vec::new(),
                class_base_definition.clone(),
                false,
            )),
        }));

        let mut members = Vec::with_capacity(class_base_definition.borrow().member_definitions().unwrap().len());
        for member in class_base_definition.borrow().member_definitions().unwrap() {
            let mut new_member = DataObject::new();

            new_member.set_member_visibility(Some(member.member_of_visibility));
            new_member.set_member_of_class(Some(new_value.clone()));
            new_member.set_variable_name(Some(&member.name))?;

            members.push(DataObjectRef::new(new_member));
        }

        let mut methods: HashMap<Box<str>, FunctionPointerObjectRef> = HashMap::with_capacity(class_base_definition.borrow().methods.len());
        for (method_name, method) in class_base_definition.borrow().methods.iter() {
            methods.insert(method_name.clone(), Gc::new(FunctionPointerObject::copy_with_this_object(method, new_value.clone())?));
        }

        let constructors = FunctionPointerObject::copy_with_this_object(
            &class_base_definition.borrow().constructors,
            new_value.clone(),
        )?;

        {
            let mut new_value_ref = new_value.borrow_mut();
            let LangObjectData::Object(new_value_data) = &mut new_value_ref.data else {
                panic!();
            };

            new_value_data.members = members;
            new_value_ref.methods = methods;
            new_value_ref.constructors = Gc::new(constructors);
        }

        Ok(new_value)
    }

    pub fn super_level(&self) -> Option<usize> {
        match &self.data {
            LangObjectData::Object(object) => Some(object.super_level),

            LangObjectData::Class(..) => None,
        }
    }

    pub fn set_super_level(&mut self, super_level: usize) -> Result<(), DataTypeConstraintError> {
        match &mut self.data {
            LangObjectData::Object(object) => {
                object.super_level = super_level;

                Ok(())
            },

            LangObjectData::Class(..) => Err(DataTypeConstraintError::with_message(
                "Super level can only be modified on objects",
            )),
        }
    }

    pub fn post_construct(&mut self) -> Result<(), DataTypeConstraintError> {
        let LangObjectData::Object(object) = &mut self.data else {
            return Err(DataTypeConstraintError::with_message(
                "Post construct must be called on an object",
            ));
        };

        if object.initialized {
            return Err(DataTypeConstraintError::with_message(
                "Object is already initialized",
            ));
        }

        for (i, member) in object.members.iter_mut().
                enumerate() {
            if let Some(member_definition) = object.class_base_definition.borrow().
                    member_definitions().
                    and_then(|member_definitions| member_definitions.get(i)) {
                let mut member = member.borrow_mut();
                if let Some(type_constraint) = &member_definition.type_constraint {
                    member.set_type_constraint(type_constraint.clone())?;
                }

                if member_definition.final_flag {
                    member.set_final_data(true);
                }
            };
        }

        object.initialized = true;

        Ok(())
    }

    pub fn is_initialized(&self) -> Option<bool> {
        match &self.data {
            LangObjectData::Object(object) => Some(object.initialized),

            LangObjectData::Class(..) => None,
        }
    }

    pub fn is_class(&self) -> bool {
        match &self.data {
            LangObjectData::Object(..) => false,
            LangObjectData::Class(..) => true,
        }
    }

    pub fn class_name(&self) -> Option<&str> {
        self.class_name.as_deref()
    }

    pub fn static_members(&self) -> &[DataObjectRef] {
        &self.static_members
    }

    pub fn index_of_static_member(&self, member_name: &str) -> Option<usize> {
        for (i, static_member) in self.static_members.iter().enumerate() {
            if static_member.borrow().variable_name.as_ref().
                    is_some_and(|variable_name| **variable_name == *member_name) {
                return Some(i);
            }
        }

        None
    }

    pub fn static_member(&self, member_name: &str) -> Result<&DataObjectRef, DataTypeConstraintError> {
        let index = self.index_of_static_member(member_name).
                ok_or_else(|| DataTypeConstraintError::with_message(format!(
                    "The static member \"{member_name}\" is not part of this class or object",
                )))?;

        Ok(&self.static_members[index])
    }

    pub fn member_definitions(&self) -> Option<&[MemberDefinition]> {
        match &self.data {
            LangObjectData::Class(class) => Some(&class.members),
            LangObjectData::Object(_) => None,
        }
    }

    pub fn members(&self) -> Option<&[DataObjectRef]> {
        match &self.data {
            LangObjectData::Object(object) => Some(&object.members),
            LangObjectData::Class(_) => None,
        }
    }

    pub fn index_of_member(&self, member_name: &str) -> Result<Option<usize>, DataTypeConstraintError> {
        let LangObjectData::Object(object) = &self.data else {
            return Err(DataTypeConstraintError::with_message(
                "Members can only be read in objects",
            ));
        };

        for (i, static_member) in object.members.iter().enumerate() {
            if static_member.borrow().variable_name.as_ref().
                    is_some_and(|variable_name| **variable_name == *member_name) {
                return Ok(Some(i));
            }
        }

        Ok(None)
    }

    pub fn member(&self, member_name: &str) -> Result<&DataObjectRef, DataTypeConstraintError> {
        let index = self.index_of_member(member_name)?.
                ok_or_else(|| DataTypeConstraintError::with_message(format!(
                    "The member \"{member_name}\" is not part of this class",
                )))?;

        let LangObjectData::Object(object) = &self.data else {
            return Err(DataTypeConstraintError::with_message(
                "Members can only be read in objects",
            ));
        };

        Ok(&object.members[index])
    }

    pub fn methods(&self) -> &HashMap<Box<str>, Gc<FunctionPointerObject>> {
        &self.methods
    }

    fn super_methods_internal(&self, super_level: usize) -> HashMap<Box<str>, FunctionPointerObjectRef> {
        let base_class_definition = if let LangObjectData::Object(object) = &self.data {
            Ok(object.class_base_definition.borrow())
        }else {
            Err(self)
        };

        let mut super_methods: HashMap<Box<str>, FunctionPointerObjectRef> = HashMap::new();

        let base_class_definition_ref = base_class_definition.
                as_deref().unwrap_or_else(|lang_object| *lang_object);
        for parent_class in base_class_definition_ref.parent_classes().unwrap() {
            if super_level > 0 {
                let super_super_methods = parent_class.borrow().
                        super_methods_internal(super_level - 1);

                for (method_name, method) in super_super_methods {
                    if let Some(super_method) = super_methods.get_mut(&method_name) {
                        *super_method = Gc::new(super_method.copy_with_added_functions(&method));
                    }else {
                        super_methods.insert(method_name, method.clone());
                    }
                }
            }else {
                for (method_name, method) in parent_class.borrow().methods.iter() {
                    if let Some(super_method) = super_methods.get_mut(method_name) {
                        *super_method = Gc::new(super_method.copy_with_added_functions(method));
                    }else {
                        super_methods.insert(method_name.clone(), method.clone());
                    }
                }
            }
        }

        super_methods
    }

    pub fn super_methods(&self) -> HashMap<Box<str>, FunctionPointerObjectRef> {
        let super_level = if let LangObjectData::Object(object) = &self.data {
            object.super_level
        }else {
            0
        };

        self.super_methods_internal(super_level)
    }

    pub fn constructors(&self) -> FunctionPointerObjectRef {
        self.constructors.clone()
    }

    /**
     * @return Returns all constructors for the current super level without going to super
     * [0 is current, 1 is parent, 2 is grandparent]
     */
    pub fn constructors_for_current_super_level(&self) -> FunctionPointerObjectRef {
        let super_level = if let LangObjectData::Object(object) = &self.data {
            object.super_level
        }else {
            0
        };

        if super_level == 0 {
            return self.constructors.clone();
        }

        self.super_constructors_internal(super_level - 1)
    }

    fn super_constructors_internal(&self, super_level: usize) -> FunctionPointerObjectRef {
        let base_class_definition = if let LangObjectData::Object(object) = &self.data {
            Ok(object.class_base_definition.borrow())
        }else {
            Err(self)
        };

        let mut super_constructors: Option<FunctionPointerObjectRef> = None;

        let base_class_definition_ref = base_class_definition.
                as_deref().unwrap_or_else(|lang_object| *lang_object);
        for parent_class in base_class_definition_ref.parent_classes().unwrap() {
            if super_level > 0 {
                let super_super_constructors = parent_class.borrow().
                        super_constructors_internal(super_level - 1);

                if let Some(super_constructors) = &mut super_constructors {
                    *super_constructors = Gc::new(super_constructors.copy_with_added_functions(&super_super_constructors));
                }else {
                    super_constructors = Some(super_super_constructors);
                }
            }else {
                let parent_class = parent_class.borrow();
                let super_super_constructors = parent_class.constructors();

                if let Some(super_constructors) = &mut super_constructors {
                    *super_constructors = Gc::new(super_constructors.copy_with_added_functions(&super_super_constructors));
                }else {
                    super_constructors = Some(super_super_constructors.clone());
                }
            }
        }

        super_constructors.expect("Internal error: No constructor in base class found")
    }

    pub fn super_constructors(&self) -> FunctionPointerObjectRef {
        let super_level = if let LangObjectData::Object(object) = &self.data {
            object.super_level
        }else {
            0
        };

        self.super_constructors_internal(super_level)
    }

    pub fn parent_classes(&self) -> Option<&[LangObjectRef]> {
        match &self.data {
            LangObjectData::Class(class) => Some(&class.parent_classes),
            LangObjectData::Object(_) => None,
        }
    }

    pub fn base_definition(&self) -> OptionLangObjectRef {
        match &self.data {
            LangObjectData::Object(object) => Some(object.class_base_definition.clone()),

            LangObjectData::Class(..) => None,
        }
    }

    pub fn is_instance_of(&self, lang_object: &LangObject) -> bool {
        if !lang_object.is_class() {
            return false;
        }

        if let LangObjectData::Object(object) = &self.data {
            return object.class_base_definition.borrow().is_instance_of(lang_object);
        }

        if ptr::eq(self, lang_object) {
            return true;
        }

        if let LangObjectData::Class(class) = &self.data {
            for parent_class in &class.parent_classes {
                if parent_class.borrow().is_instance_of(lang_object) {
                    return true;
                }
            }
        }

        false
    }
}

impl Display for LangObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_class() {
            write!(f, "<Class>")
        }else {
            write!(f, "<Object>")
        }
    }
}
