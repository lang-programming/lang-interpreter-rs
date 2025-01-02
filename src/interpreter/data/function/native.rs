use std::any::Any;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::interpreter::data::{DataObjectRef, DataTypeConstraintError, LangObjectRef, OptionDataObjectRef, OptionLangObjectRef};
use crate::interpreter::{operators, Interpreter};
use crate::lexer::CodePosition;

pub type Result<T> = std::result::Result<T, NativeError>;

type DynFnLangNativeFunction<Args> = dyn Fn(&mut Interpreter, Args) -> Result<OptionDataObjectRef>;

pub trait FromLangArgs: Sized {
    fn from_lang_args(
        this_object: OptionLangObjectRef,
        args: Vec<DataObjectRef>,
    ) -> Result<Self>;

    fn lang_parameter_count() -> usize;
    fn is_method() -> bool;
}

#[doc(hidden)]
mod tuple_from_lang_args {
    use std::mem;
    use super::*;

    crate::internal_tuple_from_lang_args! { 1 }
    crate::internal_tuple_from_lang_args! { 2 }
    crate::internal_tuple_from_lang_args! { 3 }
    crate::internal_tuple_from_lang_args! { 4 }
    crate::internal_tuple_from_lang_args! { 5 }
    crate::internal_tuple_from_lang_args! { 6 }
    crate::internal_tuple_from_lang_args! { 7 }
    crate::internal_tuple_from_lang_args! { 8 }
    crate::internal_tuple_from_lang_args! { 9 }

    //No args
    impl FromLangArgs for () {
        fn from_lang_args(
            this_object: OptionLangObjectRef,
            args: Vec<DataObjectRef>,
        ) -> Result<Self> {
            if !args.is_empty() {
                return Err(NativeError::new("Invalid argument count for native function", None));
            }

            if this_object.is_some() {
                return Err(NativeError::new("This object may not be set for native function without a this parameter", None));
            }

            Ok(())
        }

        fn lang_parameter_count() -> usize {
            0
        }

        fn is_method() -> bool {
            false
        }
    }

    //VarArgs only
    impl FromLangArgs for (Vec<DataObjectRef>,) {
        fn from_lang_args(
            this_object: OptionLangObjectRef,
            args: Vec<DataObjectRef>,
        ) -> Result<Self> {
            if this_object.is_some() {
                return Err(NativeError::new("This object may not be set for native function without a this parameter", None));
            }

            Ok((args,))
        }

        fn lang_parameter_count() -> usize {
            1
        }

        fn is_method() -> bool {
            false
        }
    }

    //This arg only
    impl FromLangArgs for (LangObjectRef,) {
        fn from_lang_args(
            this_object: OptionLangObjectRef,
            args: Vec<DataObjectRef>,
        ) -> Result<Self> {
            if !args.is_empty() {
                return Err(NativeError::new("Invalid argument count for native function", None));
            }

            let Some(this_object) = this_object.as_ref() else {
                return Err(NativeError::new("This object must be set for native function with a this parameter", None));
            };

            Ok((this_object.clone(),))
        }

        fn lang_parameter_count() -> usize {
            0
        }

        fn is_method() -> bool {
            true
        }
    }

    //This arg + VarArgs
    impl FromLangArgs for (LangObjectRef, Vec<DataObjectRef>) {
        fn from_lang_args(
            this_object: OptionLangObjectRef,
            args: Vec<DataObjectRef>,
        ) -> Result<Self> {
            let Some(this_object) = this_object.as_ref() else {
                return Err(NativeError::new("This object must be set for native function with a this parameter", None));
            };

            Ok((this_object.clone(), args))
        }

        fn lang_parameter_count() -> usize {
            1
        }

        fn is_method() -> bool {
            true
        }
    }
}

mod private {
    pub trait Sealed {}
}

pub trait NativeFunctionAdapter: private::Sealed {
    fn lang_call(
        &self,
        interpreter: &mut Interpreter,
        this_object: OptionLangObjectRef,
        args: Vec<DataObjectRef>,
    ) -> Result<OptionDataObjectRef>;

    fn lang_parameter_count(&self) -> usize;
    fn is_method(&self) -> bool;
}

impl<
    Args: FromLangArgs,
> NativeFunctionAdapter for Box<DynFnLangNativeFunction<Args>> {
    fn lang_call(
        &self,
        interpreter: &mut Interpreter,
        this_object: OptionLangObjectRef,
        args: Vec<DataObjectRef>,
    ) -> Result<OptionDataObjectRef> {
        self(interpreter, Args::from_lang_args(this_object, args)?)
    }

    fn lang_parameter_count(&self) -> usize {
        Args::lang_parameter_count()
    }

    fn is_method(&self) -> bool {
        Args::is_method()
    }
}

impl<
    Args: FromLangArgs,
> private::Sealed for Box<DynFnLangNativeFunction<Args>> {}

impl Debug for dyn NativeFunctionAdapter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Native function at {:p}", self)
    }
}

#[inline(always)]
fn func_trait<F, Args>(func: Box<F>) -> Box<DynFnLangNativeFunction<Args>> where
        Args: FromLangArgs,
        F: Fn(&mut Interpreter, Args) -> Result<OptionDataObjectRef> + 'static,
{
    func
}

#[inline(always)]
fn create_native_function_adapter_from_func_trait<'a, F, Args>(func: F) -> Box<dyn NativeFunctionAdapter + 'a> where
        Args: FromLangArgs,
        F: Fn(&mut Interpreter, Args) -> Result<OptionDataObjectRef> + NativeFunctionAdapter + 'a,
{
    Box::new(func)
}

#[inline(always)]
pub fn create_native_function_adapter<F, Args>(func: F) -> Box<dyn NativeFunctionAdapter + 'static> where
        Args: FromLangArgs + 'static,
        F: Fn(&mut Interpreter, Args) -> Result<OptionDataObjectRef> + 'static,
{
    create_native_function_adapter_from_func_trait(func_trait(Box::new(func)))
}

pub fn create_native_function(
    func: Box<dyn NativeFunctionAdapter>,
    func_id: NativeFuncId,
    value_dependencies: Vec<Box<dyn AnyWithEq>>,
) -> NativeFunction {
    NativeFunction::new(func, func_id, value_dependencies)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NativeFuncId(usize);

pub fn gen_next_native_func_id() -> NativeFuncId {
    static NEXT_NATIVE_FUNC_ID: AtomicUsize = AtomicUsize::new(0);

    let id = NEXT_NATIVE_FUNC_ID.fetch_add(1, Ordering::Relaxed);

    NativeFuncId(id)
}

#[derive(Debug)]
pub struct NativeFunction {
    function_body: Box<dyn NativeFunctionAdapter>,
    func_id: NativeFuncId,

    value_dependencies: Vec<Box<dyn AnyWithEq>>,
}

impl NativeFunction {
    fn new(function_body: Box<dyn NativeFunctionAdapter>, func_id: NativeFuncId, value_dependencies: Vec<Box<dyn AnyWithEq>>) -> Self {
        Self { function_body, func_id, value_dependencies }
    }

    pub fn is_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        self.func_id == other.func_id && self.value_dependencies.len() == other.value_dependencies.len() &&
                self.value_dependencies.iter().zip(other.value_dependencies.iter()).
                        all(|(s, o)|
                                s.is_equals(o.as_ref(), interpreter, pos))
    }

    pub fn is_strict_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        self.func_id == other.func_id && self.value_dependencies.len() == other.value_dependencies.len() &&
                self.value_dependencies.iter().zip(other.value_dependencies.iter()).
                        all(|(s, o)|
                                s.is_strict_equals(o.as_ref(), interpreter, pos))
    }

    pub fn function_body(&self) -> &dyn NativeFunctionAdapter {
        self.function_body.as_ref()
    }

    pub fn lang_parameter_count(&self) -> usize {
        self.function_body.lang_parameter_count()
    }

    pub fn is_method(&self) -> bool {
        self.function_body.is_method()
    }
}

#[allow(unused_variables)]
pub trait AnyWithEq: Debug {
    fn as_any(&self) -> &dyn Any;

    fn is_equals(&self, other: &dyn AnyWithEq, interpreter: &mut Interpreter, pos: CodePosition) -> bool;
    fn is_strict_equals(&self, other: &dyn AnyWithEq, interpreter: &mut Interpreter, pos: CodePosition) -> bool;
}

impl<T: 'static + PartialEq> AnyWithEq for T where T: Debug {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_equals(&self, other: &dyn AnyWithEq, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        other.as_any().downcast_ref::<T>().map_or(false, |o| self == o)
    }

    fn is_strict_equals(&self, other: &dyn AnyWithEq, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        other.as_any().downcast_ref::<T>().map_or(false, |o| self == o)
    }
}

impl AnyWithEq for DataObjectRef {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_equals(&self, other: &dyn AnyWithEq, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        other.as_any().downcast_ref::<Self>().
                map_or(false, |o| operators::is_equals(
                    interpreter,
                    &self.clone(),
                    &o.clone(),
                    pos,
                ))
    }

    fn is_strict_equals(&self, other: &dyn AnyWithEq, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        other.as_any().downcast_ref::<Self>().
                map_or(false, |o| operators::is_strict_equals(
                    interpreter,
                    &self.clone(),
                    &o.clone(),
                    pos,
                ))
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Native function (ID: {}) at {:p}", self.func_id.0, self.function_body.as_ref())
    }
}

#[derive(Debug)]
pub struct NativeError {
    message: String,
    cause: Option<Box<dyn Error>>,
}

impl NativeError {
    pub fn new(message: &str, cause: Option<Box<dyn Error>>) -> Self {
        Self {
            message: message.into(),
            cause,
        }
    }

    pub fn from_error<E>(cause: E) -> Self
    where E: Error + 'static {
        Self {
            message: "Native Error".to_string(),
            cause: Some(Box::new(cause)),
        }
    }

    pub fn from_error_with_message<E>(message: &str, cause: E) -> Self
    where E: Error + 'static {
        Self {
            message: message.into(),
            cause: Some(Box::new(cause)),
        }
    }

    pub fn apply<E>(cause: E) -> Self
    where E: Error + 'static {
        Self {
            message: "Native Error".to_string(),
            cause: Some(Box::new(cause)),
        }
    }

    pub fn apply_with_message<E>(message: &str) -> impl FnOnce(E) -> Self + 'static
    where E: Error + 'static {
        let message = message.into();
        
        move |cause: E| {
            Self {
                message,
                cause: Some(Box::new(cause)),
            }
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn cause(&self) -> Option<&dyn Error> {
        self.cause.as_deref()
    }
}

impl From<DataTypeConstraintError> for NativeError {
    fn from(value: DataTypeConstraintError) -> Self {
        Self::new("Data Type Constraint error", Some(value.into()))
    }
}

impl From<Box<dyn Error>> for NativeError {
    fn from(value: Box<dyn Error>) -> Self {
        Self::new("Native Error", Some(value))
    }
}

impl Display for NativeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(cause) = &self.cause {
            write!(f, "{}\nCaused by:\n{}", &self.message, cause)
        }else {
            write!(f, "{}", &self.message)
        }
    }
}

impl Error for NativeError {}
