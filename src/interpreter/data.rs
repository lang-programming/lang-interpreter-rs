pub mod function;
pub mod object;

use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write as _};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::LazyLock;
use gc::{Finalize, Gc, GcCell, Trace};
use crate::interpreter::data::function::FunctionPointerObject;
use crate::interpreter::data::object::LangObject;
use crate::interpreter::InterpretingError;

pub type OptionDataObjectRef = Option<DataObjectRef>;

pub type LangObjectRef = Gc<GcCell<LangObject>>;
pub type OptionLangObjectRef = Option<LangObjectRef>;

pub type FunctionPointerObjectRef = Gc<FunctionPointerObject>;

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct DataType(u8);

impl DataType {
    pub const TEXT: DataType = Self(0);
    pub const CHAR: DataType = Self(1);
    pub const INT: DataType = Self(2);
    pub const LONG: DataType = Self(3);
    pub const FLOAT: DataType = Self(4);
    pub const DOUBLE: DataType = Self(5);
    pub const BYTE_BUFFER: DataType = Self(6);
    pub const ARRAY: DataType = Self(7);
    pub const LIST: DataType = Self(8);
    pub const VAR_POINTER: DataType = Self(9);
    pub const FUNCTION_POINTER: DataType = Self(10);
    pub const STRUCT: DataType = Self(11);
    pub const OBJECT: DataType = Self(12);
    pub const ERROR: DataType = Self(13);
    pub const NULL: DataType = Self(14);
    pub const VOID: DataType = Self(15);
    pub const ARGUMENT_SEPARATOR: DataType = Self(16);
    pub const TYPE: DataType = Self(17);

    pub(crate) const VALUES: [DataType; 18] = [
        Self::TEXT, Self::CHAR, Self::INT, Self::LONG, Self::FLOAT, Self::DOUBLE, Self::BYTE_BUFFER,
        Self::ARRAY, Self::LIST, Self::VAR_POINTER, Self::FUNCTION_POINTER, Self::STRUCT, Self::OBJECT,
        Self::ERROR, Self::NULL, Self::VOID, Self::ARGUMENT_SEPARATOR, Self::TYPE,
    ];

    pub fn from_string(str: &str) -> Option<Self> {
        match str {
            "TEXT" => Some(Self::TEXT),
            "CHAR" => Some(Self::CHAR),
            "INT" => Some(Self::INT),
            "LONG" => Some(Self::LONG),
            "FLOAT" => Some(Self::FLOAT),
            "DOUBLE" => Some(Self::DOUBLE),
            "BYTE_BUFFER" => Some(Self::BYTE_BUFFER),
            "ARRAY" => Some(Self::ARRAY),
            "LIST" => Some(Self::LIST),
            "VAR_POINTER" => Some(Self::VAR_POINTER),
            "FUNCTION_POINTER" => Some(Self::FUNCTION_POINTER),
            "STRUCT" => Some(Self::STRUCT),
            "OBJECT" => Some(Self::OBJECT),
            "ERROR" => Some(Self::ERROR),
            "NULL" => Some(Self::NULL),
            "VOID" => Some(Self::VOID),
            "ARGUMENT_SEPARATOR" => Some(Self::ARGUMENT_SEPARATOR),
            "TYPE" => Some(Self::TYPE),

            _ => None,
        }
    }

    fn type_name(&self) -> &str {
        match *self {
            Self::TEXT => "TEXT",
            Self::CHAR => "CHAR",
            Self::INT => "INT",
            Self::LONG => "LONG",
            Self::FLOAT => "FLOAT",
            Self::DOUBLE => "DOUBLE",
            Self::BYTE_BUFFER => "BYTE_BUFFER",
            Self::ARRAY => "ARRAY",
            Self::LIST => "LIST",
            Self::VAR_POINTER => "VAR_POINTER",
            Self::FUNCTION_POINTER => "FUNCTION_POINTER",
            Self::STRUCT => "STRUCT",
            Self::OBJECT => "OBJECT",
            Self::ERROR => "ERROR",
            Self::NULL => "NULL",
            Self::VOID => "VOID",
            Self::ARGUMENT_SEPARATOR => "ARGUMENT_SEPARATOR",
            Self::TYPE => "TYPE",

            _ => "invalid"
        }
    }

    pub const fn lang_type_id(&self) -> u8 {
        self.0
    }
}

impl Debug for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.type_name())
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.type_name())
    }
}

#[derive(Debug, Clone, Eq)]
pub struct DataTypeConstraint {
    types: HashSet<DataType>,
    allowed: bool,
}

impl DataTypeConstraint {
    pub fn from_allowed_types(allowed_types: &[DataType]) -> Self {
        Self {
            types: HashSet::from_iter(allowed_types.iter().copied()),
            allowed: true,
        }
    }

    pub fn from_not_allowed_types(not_allowed_types: &[DataType]) -> Self {
        Self {
            types: HashSet::from_iter(not_allowed_types.iter().copied()),
            allowed: false,
        }
    }

    pub fn from_single_allowed_type(allowed_type: DataType) -> Self {
        Self {
            types: HashSet::from([allowed_type]),
            allowed: true,
        }
    }

    pub fn is_type_allowed(&self, data_type: DataType) -> bool {
        self.types.contains(&data_type) == self.allowed
    }

    pub fn allowed_types(&self) -> Vec<DataType> {
        if self.allowed {
            Vec::from_iter(self.types.iter().copied())
        }else {
            Vec::from_iter(
                DataType::VALUES.iter().
                        filter(|data_type| !self.types.contains(data_type)).
                        copied(),
            )
        }
    }

    pub fn not_allowed_types(&self) -> Vec<DataType> {
        if self.allowed {
            Vec::from_iter(
                DataType::VALUES.iter().
                        filter(|data_type| !self.types.contains(data_type)).
                        copied(),
            )
        }else {
            Vec::from_iter(self.types.iter().copied())
        }
    }

    pub fn to_type_constraint_syntax(&self) -> String {
        let mut builder = String::new();
        builder += "{";

        //Invert "!" if no types are set and print all types
        let inverted = !self.allowed ^ self.types.is_empty();
        if inverted {
            builder += "!";
        }

        let mut types = if self.types.is_empty() {
            HashSet::from(DataType::VALUES)
        }else {
            self.types.clone()
        };

        if !inverted && types.len() > 1 && types.contains(&DataType::NULL) {
            types.remove(&DataType::NULL);

            builder += "?";
        }

        for data_type in types {
            let _ = write!(builder, "{data_type}|");
        }

        builder = builder[..builder.len() - 1].to_string();

        builder += "}";
        builder
    }
}

impl PartialEq for DataTypeConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.allowed_types() == other.allowed_types()
    }
}

impl Display for DataTypeConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}[{}]",
            if self.allowed {
                "= "
            }else {
                "! "
            },
            self.types.iter().
                    map(|ele| ele.to_string()).
                    collect::<Vec<_>>().
                    join(", "),
        )
    }
}

pub static CONSTRAINT_NORMAL: LazyLock<DataTypeConstraint, fn() -> DataTypeConstraint> =
    LazyLock::new(|| DataTypeConstraint::from_not_allowed_types(&[]));

pub static CONSTRAINT_COMPOSITE: LazyLock<DataTypeConstraint, fn() -> DataTypeConstraint> =
    LazyLock::new(|| DataTypeConstraint::from_allowed_types(&[
        DataType::ARRAY, DataType::LIST, DataType::STRUCT, DataType::OBJECT, DataType::NULL,
    ]));

pub static CONSTRAINT_FUNCTION_POINTER: LazyLock<DataTypeConstraint, fn() -> DataTypeConstraint> =
    LazyLock::new(|| DataTypeConstraint::from_allowed_types(&[
        DataType::FUNCTION_POINTER, DataType::NULL,
    ]));

#[repr(u8)]
#[derive(Debug, Clone, Trace, Finalize)]
pub enum DataValue {
    Text(Rc<str>),
    Char(char),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    ByteBuffer(Gc<GcCell<Box<[u8]>>>),
    Array(Gc<GcCell<Box<[DataObjectRef]>>>),
    List(Gc<GcCell<VecDeque<DataObjectRef>>>),
    VarPointer(DataObjectRef),
    FunctionPointer(FunctionPointerObjectRef),
    Struct(Gc<StructObject>),
    Object(LangObjectRef),
    Error(Gc<ErrorObject>),
    Null,
    Void,
    ArgumentSeparator(Rc<str>),
    //SAFETY: There are no GC reference inside DataType
    Type(#[unsafe_ignore_trace] DataType),
}

impl DataValue {
    pub const fn data_type(&self) -> DataType {
        match self {
            DataValue::Text(..) => DataType::TEXT,
            DataValue::Char(..) => DataType::CHAR,
            DataValue::Int(..) => DataType::INT,
            DataValue::Long(..) => DataType::LONG,
            DataValue::Float(..) => DataType::FLOAT,
            DataValue::Double(..) => DataType::DOUBLE,
            DataValue::ByteBuffer(..) => DataType::BYTE_BUFFER,
            DataValue::Array(..) => DataType::ARRAY,
            DataValue::List(..) => DataType::LIST,
            DataValue::VarPointer(..) => DataType::VAR_POINTER,
            DataValue::FunctionPointer(..) => DataType::FUNCTION_POINTER,
            DataValue::Struct(..) => DataType::STRUCT,
            DataValue::Object(..) => DataType::OBJECT,
            DataValue::Error(..) => DataType::ERROR,
            DataValue::Null => DataType::NULL,
            DataValue::Void => DataType::VOID,
            DataValue::ArgumentSeparator(..) => DataType::ARGUMENT_SEPARATOR,
            DataValue::Type(..) => DataType::TYPE,
        }
    }

    pub const fn is_of_type(&self, data_type: DataType) -> bool {
        self.data_type().0 == data_type.0
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct DataObject {
    value: DataValue,

    //SAFETY: There are no GC reference inside DataTypeConstraint
    #[unsafe_ignore_trace]
    type_constraint: Box<DataTypeConstraint>,

    variable_name: Option<Box<str>>,
    flags: u8,
    member_of_class_id: i64,
    //SAFETY: There are no GC reference inside Visibility
    #[unsafe_ignore_trace]
    member_visibility: Option<Visibility>,
}

impl DataObject {
    const FLAG_FINAL: u8 = 1;
    const FLAG_STATIC: u8 = 2;
    const FLAG_COPY_STATIC_AND_FINAL: u8 = 4;
    const FLAG_LANG_VAR: u8 = 8;

    pub fn get_type_constraint_for(variable_name: Option<&str>) -> &DataTypeConstraint {
        if let Some(variable_name) = variable_name {
            if variable_name.starts_with("&") {
                &CONSTRAINT_COMPOSITE
            }else if variable_name.starts_with("mp.") | variable_name.starts_with("fp.") |
                    variable_name.starts_with("func.") | variable_name.starts_with("fn.") |
                    variable_name.starts_with("linker.") | variable_name.starts_with("ln.") {
                &CONSTRAINT_FUNCTION_POINTER
            }else {
                &CONSTRAINT_NORMAL
            }
        }else {
            &CONSTRAINT_NORMAL
        }
    }

    /**
     * Creates a new data object of type NULL
     */
    pub fn new() -> Self {
        Self {
            value: DataValue::Null,

            type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

            variable_name: None,
            flags: 0,
            member_of_class_id: -1,
            member_visibility: None,
        }
    }

    pub fn new_void() -> Self {
        Self {
            value: DataValue::Void,

            type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

            variable_name: None,
            flags: 0,
            member_of_class_id: -1,
            member_visibility: None,
        }
    }

    pub fn new_text(str: impl Into<Rc<str>>) -> Self {
        Self {
            value: DataValue::Text(str.into()),

            type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

            variable_name: None,
            flags: 0,
            member_of_class_id: -1,
            member_visibility: None,
        }
    }

    pub fn new_number(value: impl Into<Number>) -> Self {
        Self {
            value: match value.into() {
                Number::Int(value) => DataValue::Int(value),
                Number::Long(value) => DataValue::Long(value),
                Number::Float(value) => DataValue::Float(value),
                Number::Double(value) => DataValue::Double(value),
            },

            type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

            variable_name: None,
            flags: 0,
            member_of_class_id: -1,
            member_visibility: None,
        }
    }

    pub fn new_optional_text(str: Option<impl Into<Rc<str>>>) -> Self {
        if let Some(str) = str {
            Self::new_text(str)
        }else {
            Self {
                value: DataValue::Null,

                type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

                variable_name: None,
                flags: 0,
                member_of_class_id: -1,
                member_visibility: None,
            }
        }
    }

    pub fn new_final_text(str: impl Into<Rc<str>>) -> Self {
        Self {
            value: DataValue::Text(str.into()),

            type_constraint: Box::new(CONSTRAINT_NORMAL.clone()),

            variable_name: None,
            flags: Self::FLAG_FINAL,
            member_of_class_id: -1,
            member_visibility: None,
        }
    }

    /// This method allows the use of the rust "?" operator for data object creation
    #[inline(always)]
    pub fn with_update(func: impl FnOnce(&mut Self) -> Result<&mut DataObject, DataTypeConstraintError>) -> Result<DataObject, DataTypeConstraintError> {
        let mut data_object = DataObject::new();

        data_object.update(func)?;

        Ok(data_object)
    }

    /// This method allows the use of the rust "?" operator for data object creation and sets final data to true
    #[inline(always)]
    pub fn with_update_final(func: impl FnOnce(&mut Self) -> Result<&mut DataObject, DataTypeConstraintError>) -> Result<DataObject, DataTypeConstraintError> {
        let mut data_object = DataObject::new();

        data_object.update(func)?;
        data_object.set_final_data(true);

        Ok(data_object)
    }

    /// This method allows the use of the rust "?" operator for data object updates
    #[inline(always)]
    pub fn update(&mut self, func: impl FnOnce(&mut Self) -> Result<&mut DataObject, DataTypeConstraintError>) -> Result<(), DataTypeConstraintError> {
        func(self)?;

        Ok(())
    }

    /// This method allows the use of the rust "?" operator for data object updates and sets final data to true
    #[inline(always)]
    pub fn update_final(&mut self, func: impl FnOnce(&mut Self) -> Result<&mut DataObject, DataTypeConstraintError>) -> Result<(), DataTypeConstraintError> {
        func(self)?;
        self.set_final_data(true);

        Ok(())
    }

    /**
     * This method <b>ignores</b> the final and static state of the data object<br>
     * This method will not modify variableName<br>
     * This method will also not modify finalData nor staticData (<b>Except</b>: copyStaticAndFinalModifiers flag is set)
     */
    pub fn set_data(&mut self, data_object: &DataObject) -> Result<&mut Self, DataTypeConstraintError> {
        self.check_type(data_object.value.data_type())?;

        //Set function name for better debugging experience
        if let DataValue::FunctionPointer(function_pointer) = &data_object.value {
            if let Some(variable_name) = &self.variable_name {
                if self.variable_name().is_some() && function_pointer.function_name().is_none() {
                    self.value = DataValue::FunctionPointer(Gc::new(function_pointer.
                            copy_with_function_name(variable_name)));
                }else {
                    self.value = data_object.value.clone();
                }
            }else {
                self.value = data_object.value.clone();
            }
        }else {
            self.value = data_object.value.clone();
        }

        if data_object.is_copy_static_and_final_modifiers() {
            if data_object.is_final_data() {
                self.flags |= Self::FLAG_FINAL;
            }

            if data_object.is_static_data() {
                self.flags |= Self::FLAG_STATIC;
            }
        }

        Ok(self)
    }

    pub(crate) fn data_value(&self) -> &DataValue {
        &self.value
    }

    pub(crate) fn set_argument_separator(&mut self, value: &str) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::ARGUMENT_SEPARATOR)?;
        self.value = DataValue::ArgumentSeparator(value.into());

        Ok(self)
    }

    pub fn set_text(&mut self, value: impl Into<Rc<str>>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::TEXT)?;
        self.value = DataValue::Text(value.into());

        Ok(self)
    }

    pub fn text_value(&self) -> Option<&str> {
        match &self.value {
            DataValue::Text(value) | DataValue::ArgumentSeparator(value) => Some(value.as_ref()),

            _ => None,
        }
    }

    pub fn set_byte_buffer(&mut self, value: Box<[u8]>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::BYTE_BUFFER)?;
        self.value = DataValue::ByteBuffer(Gc::new(GcCell::new(value)));

        Ok(self)
    }

    pub fn byte_buffer_value(&self) -> Option<Gc<GcCell<Box<[u8]>>>> {
        match &self.value {
            DataValue::ByteBuffer(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_array(&mut self, value: Box<[DataObjectRef]>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::ARRAY)?;
        self.value = DataValue::Array(Gc::new(GcCell::new(value)));

        Ok(self)
    }

    pub fn array_value(&self) -> Option<Gc<GcCell<Box<[DataObjectRef]>>>> {
        match &self.value {
            DataValue::Array(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_list(&mut self, value: VecDeque<DataObjectRef>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::LIST)?;
        self.value = DataValue::List(Gc::new(GcCell::new(value)));

        Ok(self)
    }

    pub fn list_value(&self) -> Option<Gc<GcCell<VecDeque<DataObjectRef>>>> {
        match &self.value {
            DataValue::List(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_var_pointer(&mut self, value: DataObjectRef) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::VAR_POINTER)?;
        self.value = DataValue::VarPointer(value);

        Ok(self)
    }

    pub fn var_pointer_value(&self) -> Option<DataObjectRef> {
        match &self.value {
            DataValue::VarPointer(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_function_pointer(&mut self, value: FunctionPointerObjectRef) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::FUNCTION_POINTER)?;

        if let Some(variable_name) = &self.variable_name {
            if self.variable_name().is_some() && value.function_name().is_none() {
                self.value = DataValue::FunctionPointer(Gc::new(value.
                        copy_with_function_name(variable_name)));
            }else {
                self.value = DataValue::FunctionPointer(value);
            }
        }else {
            self.value = DataValue::FunctionPointer(value);
        }

        Ok(self)
    }

    pub fn function_pointer_value(&self) -> Option<FunctionPointerObjectRef> {
        match &self.value {
            DataValue::FunctionPointer(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_struct(&mut self, value: Gc<StructObject>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::STRUCT)?;
        self.value = DataValue::Struct(value);

        Ok(self)
    }

    pub fn struct_value(&self) -> Option<Gc<StructObject>> {
        match &self.value {
            DataValue::Struct(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_object(&mut self, value: LangObjectRef) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::OBJECT)?;
        self.value = DataValue::Object(value);

        Ok(self)
    }

    pub fn object_value(&self) -> Option<LangObjectRef> {
        match &self.value {
            DataValue::Object(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_null(&mut self) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::NULL)?;
        self.value = DataValue::Null;

        Ok(self)
    }

    pub fn set_void(&mut self) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::VOID)?;
        self.value = DataValue::Void;

        Ok(self)
    }

    pub fn set_number(&mut self, value: Number) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        match value {
            Number::Int(value) => {
                self.check_type(DataType::INT)?;
                self.value = DataValue::Int(value);
            },

            Number::Long(value) => {
                self.check_type(DataType::LONG)?;
                self.value = DataValue::Long(value);
            },

            Number::Float(value) => {
                self.check_type(DataType::FLOAT)?;
                self.value = DataValue::Float(value);
            },

            Number::Double(value) => {
                self.check_type(DataType::DOUBLE)?;
                self.value = DataValue::Double(value);
            },
        }

        Ok(self)
    }

    pub fn set_int(&mut self, value: i32) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::INT)?;
        self.value = DataValue::Int(value);

        Ok(self)
    }

    pub fn int_value(&self) -> Option<i32> {
        match self.value {
            DataValue::Int(value) => Some(value),

            _ => None,
        }
    }

    /**
     * Sets data to INT = 1 if boolean value is true else INT = 0
     */
    pub fn set_bool(&mut self, value: bool) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::INT)?;
        self.value = DataValue::Int(value as i32);

        Ok(self)
    }

    pub fn set_long(&mut self, value: i64) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::LONG)?;
        self.value = DataValue::Long(value);

        Ok(self)
    }

    pub fn long_value(&self) -> Option<i64> {
        match self.value {
            DataValue::Long(value) => Some(value),

            _ => None,
        }
    }

    pub fn set_float(&mut self, value: f32) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::FLOAT)?;
        self.value = DataValue::Float(value);

        Ok(self)
    }

    pub fn float_value(&self) -> Option<f32> {
        match self.value {
            DataValue::Float(value) => Some(value),

            _ => None,
        }
    }

    pub fn set_double(&mut self, value: f64) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::DOUBLE)?;
        self.value = DataValue::Double(value);

        Ok(self)
    }

    pub fn double_value(&self) -> Option<f64> {
        match self.value {
            DataValue::Double(value) => Some(value),

            _ => None,
        }
    }

    pub fn set_char(&mut self, value: char) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::CHAR)?;
        self.value = DataValue::Char(value);

        Ok(self)
    }

    pub fn char_value(&self) -> Option<char> {
        match self.value {
            DataValue::Char(value) => Some(value),

            _ => None,
        }
    }

    pub fn set_error(&mut self, value: Gc<ErrorObject>) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::ERROR)?;
        self.value = DataValue::Error(value);

        Ok(self)
    }

    pub fn error_value(&self) -> Option<Gc<ErrorObject>> {
        match &self.value {
            DataValue::Error(value) => Some(value.clone()),

            _ => None,
        }
    }

    pub fn set_type(&mut self, value: DataType) -> Result<&mut Self, DataTypeConstraintError> {
        if self.is_final_data() {
            return Ok(self);
        }

        self.check_type(DataType::TYPE)?;
        self.value = DataValue::Type(value);

        Ok(self)
    }

    pub fn type_value(&self) -> Option<DataType> {
        match self.value {
            DataValue::Type(value) => Some(value),

            _ => None,
        }
    }

    /// This method returns Some for INT, LONG, FLOAT, and DOUBLE values and should only be used
    /// for native function parameters with the "number" parameter type
    ///
    /// This method does not convert the data object into a number if it is none.
    /// A data object can be converted with [conversions::to_number](crate::interpreter::conversions::to_number)
    pub fn number_value(&self) -> Option<Number> {
        match self.value {
            DataValue::Int(value) => Some(value.into()),
            DataValue::Long(value) => Some(value.into()),
            DataValue::Float(value) => Some(value.into()),
            DataValue::Double(value) => Some(value.into()),

            _ => None,
        }
    }

    /// This method returns Some for INT values and should only be used
    /// for native function parameters with the "boolean" parameter type
    ///
    /// This method does not convert the data object into a boolean if it is none.
    /// A data object can be converted with [conversions::to_bool](crate::interpreter::conversions::to_bool)
    pub fn bool_value(&self) -> Option<bool> {
        match self.value {
            DataValue::Int(value) => Some(value != 0),

            _ => None,
        }
    }

    pub fn set_variable_name(&mut self, variable_name: Option<&str>) -> Result<&mut Self, DataTypeConstraintError> {
        let new_type_requirement = Self::get_type_constraint_for(variable_name);
        if !new_type_requirement.is_type_allowed(self.value.data_type()) {
            return Err(DataTypeConstraintError::new());
        }

        self.type_constraint = Box::new(new_type_requirement.clone());
        self.variable_name = variable_name.map(Box::from);

        Ok(self)
    }

    pub fn variable_name(&self) -> Option<&str> {
        self.variable_name.as_deref()
    }

    pub fn set_final_data(&mut self, final_data: bool) -> &mut Self {
        self.flags = (self.flags | Self::FLAG_FINAL) ^ if final_data {
            0
        }else {
            Self::FLAG_FINAL
        };

        self
    }

    pub fn is_final_data(&self) -> bool {
        (self.flags & DataObject::FLAG_FINAL) != 0
    }

    pub fn set_static_data(&mut self, final_data: bool) -> &mut Self {
        self.flags = (self.flags | Self::FLAG_STATIC) ^ if final_data {
            0
        }else {
            Self::FLAG_STATIC
        };

        self
    }

    pub fn is_static_data(&self) -> bool {
        (self.flags & DataObject::FLAG_STATIC) != 0
    }

    pub(crate) fn set_copy_static_and_final_modifiers(&mut self, copy_static_and_final_modifiers: bool) -> &mut Self {
        self.flags = (self.flags | Self::FLAG_COPY_STATIC_AND_FINAL) ^ if copy_static_and_final_modifiers {
            0
        }else {
            Self::FLAG_COPY_STATIC_AND_FINAL
        };

        self
    }

    pub fn is_copy_static_and_final_modifiers(&self) -> bool {
        (self.flags & DataObject::FLAG_COPY_STATIC_AND_FINAL) != 0
    }

    pub(crate) fn set_lang_var(&mut self) -> &mut Self {
        self.flags |= Self::FLAG_LANG_VAR;

        self
    }

    pub fn is_lang_var(&self) -> bool {
        (self.flags & DataObject::FLAG_LANG_VAR) != 0
    }

    pub fn data_type(&self) -> DataType {
        self.value.data_type()
    }

    pub(crate) fn set_type_constraint(&mut self, type_constraint: Box<DataTypeConstraint>) -> Result<&mut Self, DataTypeConstraintError> {
        for data_type in self.type_constraint.not_allowed_types() {
            if type_constraint.is_type_allowed(data_type) {
                return Err(DataTypeConstraintError::with_message("New type constraint must not allow types which were not allowed previously"));
            }
        }

        if !type_constraint.is_type_allowed(self.value.data_type()) {
            return Err(DataTypeConstraintError::new());
        }

        self.type_constraint = type_constraint;

        Ok(self)
    }

    pub fn type_constraint(&self) -> &DataTypeConstraint {
        &self.type_constraint
    }

    pub fn check_type(&self, data_type: DataType) -> Result<(), DataTypeConstraintError> {
        if !self.type_constraint.is_type_allowed(data_type) {
            return Err(DataTypeConstraintError::new());
        }

        Ok(())
    }

    pub(crate) fn set_member_visibility(&mut self, member_visibility: Option<Visibility>) -> &mut Self {
        self.member_visibility = member_visibility;

        self
    }

    pub fn member_visibility(&self) -> Option<Visibility> {
        self.member_visibility
    }

    pub(crate) fn set_member_of_class(&mut self, member_of_class: OptionLangObjectRef) -> &mut Self {
        self.member_of_class_id = member_of_class.map(|lang_object| {
            lang_object.borrow().class_id
        }).unwrap_or(-1);

        self
    }

    pub fn member_of_class(&self) -> i64 {
        self.member_of_class_id
    }

    pub fn is_accessible(&self, accessing_class: Option<&LangObjectRef>) -> bool {
        if self.member_of_class_id == -1 {
            return true;
        }

        let Some(member_visibility) = self.member_visibility else {
            return true;
        };

        if member_visibility == Visibility::Public {
            return true;
        }

        let Some(accessing_class) = accessing_class else {
            return false;
        };

        let accessing_class_id = accessing_class.borrow().class_id;

        accessing_class_id == self.member_of_class_id || (
            member_visibility == Visibility::Protected && object::CLASS_ID_TO_SUPER_CLASS_IDS.lock().
                    unwrap()[&accessing_class_id].contains(&self.member_of_class_id)
        )
    }
}

impl Default for DataObject {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for DataObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<DataObject>")
    }
}

#[derive(Debug, Clone, Default, Trace, Finalize)]
pub struct DataObjectRef(Gc<GcCell<DataObject>>);

impl DataObjectRef {
    pub fn new(data_object: DataObject) -> Self {
        Self(Gc::new(GcCell::new(data_object)))
    }

    pub(crate) fn data_value(&self) -> DataValue {
        self.0.borrow().data_value().clone()
    }

    pub fn text_value(&self) -> Option<Box<str>> {
        self.0.borrow().text_value().map(Box::from)
    }

    pub fn byte_buffer_value(&self) -> Option<Gc<GcCell<Box<[u8]>>>> {
        self.0.borrow().byte_buffer_value()
    }

    pub fn array_value(&self) -> Option<Gc<GcCell<Box<[DataObjectRef]>>>> {
        self.0.borrow().array_value()
    }

    pub fn list_value(&self) -> Option<Gc<GcCell<VecDeque<DataObjectRef>>>> {
        self.0.borrow().list_value()
    }

    pub fn var_pointer_value(&self) -> Option<DataObjectRef> {
        self.0.borrow().var_pointer_value()
    }

    pub fn function_pointer_value(&self) -> Option<FunctionPointerObjectRef> {
        self.0.borrow().function_pointer_value()
    }

    pub fn struct_value(&self) -> Option<Gc<StructObject>> {
        self.0.borrow().struct_value()
    }

    pub fn object_value(&self) -> Option<LangObjectRef> {
        self.0.borrow().object_value()
    }

    pub fn int_value(&self) -> Option<i32> {
        self.0.borrow().int_value()
    }

    pub fn long_value(&self) -> Option<i64> {
        self.0.borrow().long_value()
    }

    pub fn float_value(&self) -> Option<f32> {
        self.0.borrow().float_value()
    }

    pub fn double_value(&self) -> Option<f64> {
        self.0.borrow().double_value()
    }

    pub fn char_value(&self) -> Option<char> {
        self.0.borrow().char_value()
    }

    pub fn error_value(&self) -> Option<Gc<ErrorObject>> {
        self.0.borrow().error_value()
    }

    pub fn type_value(&self) -> Option<DataType> {
        self.0.borrow().type_value()
    }

    /// This method borrows the data object and copies the value from [DataObject::number_value]
    pub fn number_value(&self) -> Option<Number> {
        self.0.borrow().number_value()
    }

    /// This method borrows the data object and copies the value from [DataObject::bool_value]
    pub fn bool_value(&self) -> Option<bool> {
        self.0.borrow().bool_value()
    }

    pub fn variable_name(&self) -> Option<Box<str>> {
        self.0.borrow().variable_name().map(Box::from)
    }

    pub fn is_final_data(&self) -> bool {
        self.0.borrow().is_final_data()
    }

    pub fn is_static_data(&self) -> bool {
        self.0.borrow().is_static_data()
    }

    pub fn is_copy_static_and_final_modifiers(&self) -> bool {
        self.0.borrow().is_copy_static_and_final_modifiers()
    }

    pub fn is_lang_var(&self) -> bool {
        self.0.borrow().is_lang_var()
    }

    pub fn data_type(&self) -> DataType {
        self.0.borrow().data_type()
    }

    /// This functions clones data
    pub fn type_constraint(&self) -> DataTypeConstraint {
        self.0.borrow().type_constraint().clone()
    }

    pub fn member_visibility(&self) -> Option<Visibility> {
        self.0.borrow().member_visibility()
    }

    pub fn member_of_class(&self) -> i64 {
        self.0.borrow().member_of_class()
    }

    pub fn is_accessible(&self, accessing_class: Option<&LangObjectRef>) -> bool {
        self.0.borrow().is_accessible(accessing_class)
    }
}

impl Deref for DataObjectRef {
    type Target = GcCell<DataObject>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug, Trace, Finalize)]
enum StructData {
    //SAFETY: There are no GC reference inside DataTypeConstraint
    Definition(#[unsafe_ignore_trace] Vec<(Box<str>, Option<Box<DataTypeConstraint>>)>),
    Instance {
        members: Vec<(Box<str>, DataObjectRef)>,
        struct_base_definition: Gc<StructObject>,
    },
}

#[derive(Debug, Trace, Finalize)]
pub struct StructObject {
    data: StructData,
}

impl StructObject {
    pub fn new_definition(members: &[(&str, Option<DataTypeConstraint>)]) -> Self {
        let members = Vec::from_iter(members.iter().
                cloned().
                map(|(key, value)| (Box::from(key), value.
                        map(Box::new))));

        Self {
            data: StructData::Definition(members),
        }
    }

    pub fn new_instance(
        struct_base_definition: Gc<StructObject>,
        values: &[DataObjectRef],
    ) -> Result<Self, DataTypeConstraintError> {
        let mut members = Vec::with_capacity(values.len());

        {
            let StructData::Definition(struct_definition_members) = &struct_base_definition.data else {
                return Err(DataTypeConstraintError::with_message(
                    "No instance can be created of another struct instance",
                ));
            };

            if struct_definition_members.len() != values.len() {
                return Err(DataTypeConstraintError::with_message(
                    "The count of members must be equals to the count of values",
                ));
            }

            for (i, member_definition) in struct_definition_members.iter().
                    enumerate() {
                let member_name = member_definition.0.clone();

                let mut member = DataObject::new();
                member.set_variable_name(Some(&member_name))?;

                member.set_data(&values[i].borrow())?;

                if let Some(type_constraint) = member_definition.1.clone() {
                    member.set_type_constraint(type_constraint)?;
                }

                members.push((member_name, DataObjectRef::new(member)));
            }
        }

        Ok(Self {
            data: StructData::Instance {
                members,
                struct_base_definition
            },
        })
    }

    pub fn member_names(&self) -> Vec<Box<str>> {
        match &self.data {
            StructData::Definition(members) => {
                Vec::from_iter(members.iter().map(|(member_name, ..)| member_name.clone()))
            },

            StructData::Instance { struct_base_definition, .. } => {
                struct_base_definition.member_names()
            },
        }
    }

    pub fn type_constraints(&self) -> Vec<Option<Box<DataTypeConstraint>>> {
        match &self.data {
            StructData::Definition(members) => {
                Vec::from_iter(members.iter().map(|(.., type_constraint)| type_constraint.clone()))
            },

            StructData::Instance { struct_base_definition, .. } => {
                struct_base_definition.type_constraints()
            },
        }
    }

    pub fn base_definition(&self) -> Option<Gc<StructObject>> {
        match &self.data {
            StructData::Instance { struct_base_definition, .. } =>
                Some(struct_base_definition.clone()),

            StructData::Definition(..) => None,
        }
    }

    pub fn index_of(&self, member_name: &str) -> Option<usize> {
        match &self.data {
            StructData::Definition(members) => {
                for (i, (member, ..)) in members.iter().
                        enumerate() {
                    if **member == *member_name {
                        return Some(i);
                    }
                }
            },

            StructData::Instance { struct_base_definition, .. } =>
                return struct_base_definition.index_of(member_name),
        }

        None
    }

    pub fn type_constraint(&self, member_name: &str) -> Result<Option<Box<DataTypeConstraint>>, DataTypeConstraintError> {
        let Some(index) = self.index_of(member_name) else {
            return Err(DataTypeConstraintError::with_message(
                format!("The member \"{member_name}\" is not part of this struct"),
            ));
        };

        match &self.data {
            StructData::Definition(members) => Ok(members[index].1.clone()),

            StructData::Instance { struct_base_definition, .. } =>
                struct_base_definition.type_constraint(member_name),
        }
    }

    pub fn get_member(&self, member_name: &str) -> Result<DataObjectRef, DataTypeConstraintError> {
        let StructData::Instance { members, .. } = &self.data else {
            return Err(DataTypeConstraintError::with_message(
                "The struct definition is no struct instance and has no member values",
            ));
        };

        let Some(index) = self.index_of(member_name) else {
            return Err(DataTypeConstraintError::with_message(
                format!("The member \"{member_name}\" is not part of this struct"),
            ));
        };

        Ok(members[index].1.clone())
    }

    pub fn set_member(&self, member_name: &str, value: &DataObject) -> Result<(), DataTypeConstraintError> {
        let StructData::Instance { members, .. } = &self.data else {
            return Err(DataTypeConstraintError::with_message(
                "The struct definition is no struct instance and has no member values",
            ));
        };

        let Some(index) = self.index_of(member_name) else {
            return Err(DataTypeConstraintError::with_message(
                format!("The member \"{member_name}\" is not part of this struct"),
            ));
        };

        members[index].1.borrow_mut().set_data(value)?;

        Ok(())
    }

    pub fn is_definition(&self) -> bool {
        matches!(self.data, StructData::Definition(..))
    }
}

impl Display for StructObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_definition() {
            write!(f, "<Struct[Definition]>")
        }else {
            write!(f, "<Struct[Instance]>")
        }
    }
}

#[derive(Debug, Eq, Trace, Finalize)]
pub struct ErrorObject {
    //SAFETY: There are no GC reference inside InterpretingError
    #[unsafe_ignore_trace]
    err: InterpretingError,
    message: Option<Box<str>>,
}

impl ErrorObject {
    pub fn new(err: InterpretingError, message: Option<&str>) -> Self {
        Self {
            err,
            message: message.map(Box::from),
        }
    }

    pub fn err(&self) -> InterpretingError {
        self.err
    }

    pub fn message(&self) -> Option<&str> {
        self.message.as_deref()
    }
}

impl PartialEq for ErrorObject {
    fn eq(&self, other: &Self) -> bool {
        self.err == other.err
    }
}

impl Display for ErrorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error")
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

impl Visibility {
    pub fn symbol(&self) -> &'static str {
        match self {
            Visibility::Private => "-",
            Visibility::Protected => "~",
            Visibility::Public => "+",
        }
    }
}

impl From<crate::parser::ast::Visibility> for Visibility {
    fn from(value: crate::parser::ast::Visibility) -> Self {
        match value {
            crate::parser::ast::Visibility::Private => Visibility::Private,
            crate::parser::ast::Visibility::Protected => Visibility::Protected,
            crate::parser::ast::Visibility::Public => Visibility::Public,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Number {
    pub fn int_value(self) -> i32 {
        match self {
            Self::Int(value) => value,
            Self::Long(value) => value as i32,
            Self::Float(value) => value as i32,
            Self::Double(value) => value as i32,
        }
    }

    pub fn long_value(self) -> i64 {
        match self {
            Self::Int(value) => value as i64,
            Self::Long(value) => value,
            Self::Float(value) => value as i64,
            Self::Double(value) => value as i64,
        }
    }

    pub fn float_value(self) -> f32 {
        match self {
            Self::Int(value) => value as f32,
            Self::Long(value) => value as f32,
            Self::Float(value) => value,
            Self::Double(value) => value as f32,
        }
    }

    pub fn double_value(self) -> f64 {
        match self {
            Self::Int(value) => value as f64,
            Self::Long(value) => value as f64,
            Self::Float(value) => value as f64,
            Self::Double(value) => value,
        }
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self::Int(value)
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Long(value)
    }
}

impl From<f32> for Number {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Double(value)
    }
}

#[derive(Debug)]
pub struct DataTypeConstraintError {
    message: String
}

impl DataTypeConstraintError {
    pub fn new() -> Self {
        Self {
            message: "The data type would violate a type constraint".to_string(),
        }
    }

    pub fn with_message(message: impl Into<String>) -> Self {
        Self { message: message.into() }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Default for DataTypeConstraintError {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for DataTypeConstraintError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl Error for DataTypeConstraintError {}
