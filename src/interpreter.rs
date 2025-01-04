mod lang_vars;
mod predefined_functions;

pub mod module;
pub mod regex;
pub mod data;
pub mod conversions;
pub mod operators;
pub mod platform;
pub mod lang_test;

use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use std::collections::hash_map::Entry;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::{ptr, str};
use std::rc::Rc;
use std::str::FromStr;
use std::time::Instant;
use include_dir::{include_dir, Dir};
use rand::rngs::SmallRng;
use rand::{thread_rng, SeedableRng};
use crate::interpreter::module::{Module, ModuleManager};
use crate::interpreter::platform::{PlatformAPI};
use crate::interpreter::data::{
    DataObject,
    DataObjectRef,
    DataType,
    DataTypeConstraint,
    ErrorObject,
    FunctionPointerObjectRef,
    LangObjectRef,
    OptionDataObjectRef,
    OptionLangObjectRef,
    StructObject,
    Visibility,
};
use crate::interpreter::data::function::{
    Function,
    FunctionData,
    FunctionMetadata,
    FunctionPointerObject,
    InternalFunction,
    NormalFunction,
    ParameterMetadata,
    ParameterType,
};
use crate::interpreter::data::function::native::NativeError;
use crate::interpreter::data::object::{LangObject, MemberDefinition};
use crate::interpreter::lang_test::{AssertResult, LangTest};
use crate::lexer::CodePosition;
use crate::terminal_io::{Level, TerminalIO};
use crate::parser::ast::{Node, NodeData, Operator, OperatorType, AST};
use crate::parser::{Parser, ParsingError};
use crate::utils;

#[derive(Debug)]
pub struct Interpreter {
    parser: Parser,

    module_manager: ModuleManager,
    modules: HashMap<Box<str>, Rc<Module>>,

    object_class: OptionLangObjectRef,
    dummy_class_definition_class: OptionLangObjectRef,

    is_initializing_lang_standard_implementation: bool,
    scope_id: isize,
    current_call_stack_element: StackElement,
    call_stack: Vec<StackElement>,

    term: Option<TerminalIO>,
    platform_api: Box<dyn PlatformAPI>,
    origin_time: Instant,
    ran: SmallRng,

    //Lang tests
    lang_test_store: LangTest,
    lang_test_expected_throw_value: Option<InterpretingError>,
    lang_test_expected_return_value: OptionDataObjectRef,
    lang_test_expected_no_return_value: bool,
    lang_test_message_for_last_test_result: Option<Box<str>>,
    lang_test_expected_return_value_scope_id: isize,

    //Fields for return/throw node, continue/break node, and force stopping execution
    execution_state: ExecutionState,
    execution_flags: ExecutionFlags,

    //DATA
    data: HashMap<usize, Rc<RefCell<Data>>>,

    //Lang Standard implementation data
    standard_types: HashMap<Box<str>, DataObjectRef>,

    //Predefined functions & linker functions (= Predefined functions)
    funcs: HashMap<Box<str>, FunctionPointerObjectRef>,
}

impl Interpreter {
    pub const LOG_TAG: &'static str = "LangInterpreter";

    pub const VERSION: &'static str = "v1.0.0";

    const RESOURCES_DIR: Dir<'static> = include_dir!("resources");

    pub fn new(
        lang_path: &str,
        lang_file: Option<&str>,
        term: Option<TerminalIO>,
        platform_api: Box<dyn PlatformAPI>,
        lang_args: Option<Vec<Box<str>>>,
    ) -> Self {
        let mut interpreter = Self {
            parser: Parser::new(),

            module_manager: ModuleManager::new(),
            modules: HashMap::new(),

            object_class: None,
            dummy_class_definition_class: None,

            is_initializing_lang_standard_implementation: true,
            scope_id: -1,
            current_call_stack_element: StackElement::new(
                lang_path,
                lang_file,
                None,
                None,
                None,
                None,
            ),
            call_stack: Vec::new(),

            term,
            platform_api,
            origin_time: Instant::now(),
            ran: SmallRng::from_rng(thread_rng()).unwrap(),

            lang_test_store: LangTest::new(),
            lang_test_expected_throw_value: None,
            lang_test_expected_return_value: None,
            lang_test_expected_no_return_value: false,
            lang_test_message_for_last_test_result: None,
            lang_test_expected_return_value_scope_id: -1,

            execution_state: ExecutionState::new(),
            execution_flags: ExecutionFlags::new(),

            data: HashMap::new(),

            standard_types: HashMap::new(),

            funcs: HashMap::new(),
        };

        interpreter.init_lang_standard();
        interpreter.enter_scope(lang_args);

        interpreter
    }

    pub fn parse_lines(&mut self, lines: impl Into<String>) -> Option<AST> {
        self.parser.parse_lines(lines)
    }

    pub fn interpret_lines(&mut self, lines: impl Into<String>) -> OptionDataObjectRef {
        let ast = self.parse_lines(lines)?;

        self.interpret_ast(&ast)
    }

    pub fn interpret_ast(&mut self, ast: &AST) -> OptionDataObjectRef {
        /*if self.execution_state.force_stop_execution_flag {
            //TODO stop
        }*/

        let mut ret = None;
        for node in ast.nodes() {
            if self.execution_state.stop_execution_flag {
                return None;
            }

            ret = self.interpret_node(None, node);
        }

        ret
    }

    pub fn get_object_class(&self) -> &LangObjectRef {
        self.object_class.as_ref().unwrap()
    }

    pub fn lang_test_store(&self) -> &LangTest {
        &self.lang_test_store
    }

    fn data(&self) -> &Rc<RefCell<Data>> {
        let scope_id = self.scope_id as usize;

        &self.data[&scope_id]
    }

    pub fn data_ref(&self) -> Ref<Data> {
        let scope_id = self.scope_id as usize;

        self.data[&scope_id].borrow()
    }

    pub fn data_mut(&mut self) -> RefMut<Data> {
        let scope_id = self.scope_id as usize;

        self.data[&scope_id].borrow_mut()
    }

    //TODO
    /*
    pub fn force_stop(&mut self) {
        self.execution_state.force_stop_execution_flag = true;
    }

    pub fn is_force_stop_execution_flag(&self) -> bool {
        self.execution_state.force_stop_execution_flag
    }
    */

    pub fn current_call_stack_element(&self) -> &StackElement {
        &self.current_call_stack_element
    }

    fn call_stack_elements(&self) -> &[StackElement] {
        &self.call_stack
    }

    fn push_stack_element(&mut self, stack_element: StackElement, parent_pos: CodePosition) {
        self.call_stack.push(self.current_call_stack_element.copy_with_pos(parent_pos));
        self.current_call_stack_element = stack_element;
    }

    fn pop_stack_element(&mut self) -> Option<&StackElement> {
        self.current_call_stack_element = self.call_stack.pop()?;
        self.current_call_stack_element.pos = CodePosition::EMPTY;

        Some(&self.current_call_stack_element)
    }

    fn print_stack_trace(&self, pos: CodePosition) -> String {
        let mut builder = String::new();

        builder += &self.current_call_stack_element.copy_with_pos(pos).to_string();

        for ele in self.call_stack.iter().rev() {
            builder += "\n";
            builder += &ele.to_string();
        }

        builder
    }

    fn parser_line_number(&self) -> usize {
        self.parser.line_number()
    }

    fn set_parser_line_number(&mut self, line_number: usize) {
        self.parser.set_line_number(line_number);
    }

    fn reset_parser_positional_vars(&mut self) {
        self.parser.reset_position_vars();
    }

    fn interpret_node(&mut self, composite_type: OptionDataObjectRef, node: &Node) -> OptionDataObjectRef {
        /*if self.execution_state.force_stop_execution_flag {
            //TODO stop
        }*/

        match node.node_data() {
            NodeData::UnprocessedVariableName(..) => {
                let node = self.process_unprocessed_variable_name_node(composite_type.clone(), node);

                self.interpret_node(composite_type, &node)
            },

            NodeData::FunctionCallPreviousNodeValue {..} => {
                let node = self.process_function_call_previous_node_value_node(node, None);

                self.interpret_node(composite_type, &node)
            },

            NodeData::List => {
                self.interpret_list_node(composite_type.clone(), node)
            },

            NodeData::CharValue(..) |
            NodeData::TextValue(..) |
            NodeData::IntValue(..) |
            NodeData::LongValue(..) |
            NodeData::FloatValue(..) |
            NodeData::DoubleValue(..) |
            NodeData::NullValue |
            NodeData::VoidValue => {
                Some(self.interpret_value_node(node))
            },

            NodeData::ParsingError {..} => {
                Some(self.interpret_parsing_error_node(node))
            },

            NodeData::IfStatement => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_if_statement_node(node))
                }).unwrap()))
            },

            NodeData::IfStatementPartIf {..} |
            NodeData::IfStatementPartElse(..) => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_if_statement_part_node(node))
                }).unwrap()))
            },

            NodeData::LoopStatement => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_loop_statement_node(node))
                }).unwrap()))
            },

            NodeData::LoopStatementPartWhile {..} |
            NodeData::LoopStatementPartUntil {..} |
            NodeData::LoopStatementPartRepeat {..} |
            NodeData::LoopStatementPartForEach {..} |
            NodeData::LoopStatementPartLoop(..) |
            NodeData::LoopStatementPartElse(..) => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_loop_statement_part_node(node))
                }).unwrap()))
            },

            NodeData::ContinueBreakStatement {..} => {
                self.interpret_loop_statement_continue_break(node);

                None
            },

            NodeData::TryStatement => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_try_statement_node(node))
                }).unwrap()))
            },

            NodeData::TryStatementPartTry {..} |
            NodeData::TryStatementPartSoftTry {..} |
            NodeData::TryStatementPartNonTry {..} |
            NodeData::TryStatementPartCatch {..} |
            NodeData::TryStatementPartElse(..) |
            NodeData::TryStatementPartFinally(..) => {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(self.interpret_try_statement_part_node(node))
                }).unwrap()))
            },

            NodeData::Operation(..) |
            NodeData::Math(..) |
            NodeData::Condition(..) => {
                self.interpret_operation_node(node)
            },

            NodeData::Return => {
                self.interpret_return_node(node);

                None
            },

            NodeData::Throw => {
                self.interpret_throw_node(node);

                None
            },

            NodeData::Assignment => {
                self.interpret_assigment_node(node)
            },

            NodeData::VariableName {..} => {
                self.interpret_variable_name_node(composite_type, node)
            },

            NodeData::EscapeSequence(..) => {
                self.interpret_escape_sequence_node(node)
            },

            NodeData::UnicodeEscapeSequence(..) => {
                Some(self.interpret_unicode_escape_sequence_node(node))
            },

            NodeData::ArgumentSeparator(..) => {
                Some(self.interpret_argument_separator_node(node))
            },

            NodeData::FunctionCall(..) => {
                self.interpret_function_call_node(composite_type, node)
            },

            NodeData::FunctionDefinition(..) => {
                self.interpret_function_definition_node(node)
            },

            NodeData::ArrayValue => {
                Some(self.interpret_array_node(node))
            },

            NodeData::StructDefinition(..) => {
                self.interpret_struct_definition_node(node)
            },

            NodeData::ClassDefinition(..) => {
                self.interpret_class_definition_node(node)
            },
        }
    }

    /**
     * @param variable_prefix_append_after_search If no part of the variable name matched an existing variable, the variable prefix will be added to the returned TextValueNode<br>
     *                                             (e.g. "func.abc" ("func." is not part of the variableNames in the set))
     * @param supports_pointer_dereferencing_and_referencing If true, this node will return pointer reference or a dereferenced pointers as VariableNameNode<br>
     *                                   (e.g. $[abc] is not in variableNames, but $abc is -> $[abc] will return a VariableNameNode)
     */
    fn convert_variable_name_to_variable_name_node_or_composition(
        &mut self,
        module_name: Option<Box<str>>,
        variable_name: String,
        variable_names: Box<[Rc<str>]>,
        variable_prefix_append_after_search: &str,
        supports_pointer_dereferencing_and_referencing: bool,
        pos: CodePosition,
    ) -> Node {
        let variable_names = if let Some(module_name) = &module_name {
            let module = self.modules.get(module_name);
            if let Some(module) = module {
                Box::from_iter(module.exported_variables().keys().cloned())
            }else {
                self.set_errno(InterpretingError::ModuleLoadUnloadErr, Some(&format!(
                    "The module \"{module_name}\" is not loaded!",
                )), pos);

                return Node::new_text_value_node(pos, format!(
                    "[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}",
                ));
            }
        }else {
            variable_names
        };

        //Sort keySet from large to small length (e.g.: $abcd and $abc and $ab)
        let returned_variable_name = variable_names.iter().
                filter(|name| variable_name.starts_with(&***name)).
                max_by_key(|name| name.len());

        if let Some(returned_variable_name) = returned_variable_name {
            if returned_variable_name.len() == variable_name.len() {
                return Node::new_variable_name_node(pos, if let Some(module_name) = &module_name {
                    format!("[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}")
                }else {
                    format!("{variable_prefix_append_after_search}{variable_name}")
                }, None);
            }

            //Variable composition
            return Node::new_list_node(vec![
                //Add matching part of variable as VariableNameNode
                Node::new_variable_name_node(pos, if let Some(module_name) = &module_name {
                    format!("[[{module_name}]]::{variable_prefix_append_after_search}{returned_variable_name}")
                }else {
                    format!("{variable_prefix_append_after_search}{returned_variable_name}")
                }, None),

                //Add composition part as TextValueNode
                Node::new_text_value_node(pos, &variable_name[returned_variable_name.len()..]),
            ]);
        }

        if supports_pointer_dereferencing_and_referencing {
            let mut dereferences = None;
            let mut start_index = None;
            let mut modified_variable_name = variable_name.clone();
            let mut returned_node = None;
            let mut text = None;

            if variable_name.contains("*") {
                start_index = variable_name.find("*");
                let end_index = variable_name.rfind("*").unwrap() + 1;
                if end_index >= variable_name.len() {
                    return Node::new_text_value_node(pos, if let Some(module_name) = &module_name {
                        format!("[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}")
                    }else {
                        format!("{variable_prefix_append_after_search}{variable_name}")
                    });
                }

                let start_index = start_index.unwrap();
                dereferences = Some(&variable_name[start_index..end_index]);
                modified_variable_name = String::new() + &variable_name[..start_index] +
                        &variable_name[end_index..];

                if !modified_variable_name.contains("[") && !modified_variable_name.contains("]") {
                    returned_node = Some(self.convert_variable_name_to_variable_name_node_or_composition(
                        module_name.clone(),
                        modified_variable_name.clone(),
                        variable_names.clone(),
                        "",
                        supports_pointer_dereferencing_and_referencing,
                        pos,
                    ));
                }
            }

            //Check dereferenced variable name
            if modified_variable_name.contains("[") && modified_variable_name.contains("]") {
                let index_opening_bracket = modified_variable_name.find("[").unwrap();
                let index_matching_bracket = utils::get_index_of_matching_bracket_str(
                    &modified_variable_name, index_opening_bracket,
                    usize::MAX, b'[', b']',
                );

                if let Some(index_matching_bracket) = index_matching_bracket {
                    //Remove all "[" "]" pairs
                    let mut current_index = index_opening_bracket;
                    let mut current_index_matching_bracket = index_matching_bracket;

                    //"&" both "++" and "--" must be executed
                    while modified_variable_name.as_bytes()[current_index] == b'[' &&
                            modified_variable_name.as_bytes()[current_index_matching_bracket] == b']' {
                        current_index += 1;
                        current_index_matching_bracket -= 1;
                    }

                    if index_matching_bracket != modified_variable_name.len() - 1 {
                        text = Some(modified_variable_name[index_matching_bracket + 1..].to_string());
                        modified_variable_name = modified_variable_name[..index_matching_bracket + 1].to_string();
                    }

                    if !modified_variable_name[current_index..].contains("[") {
                        returned_node = Some(self.convert_variable_name_to_variable_name_node_or_composition(
                            module_name.clone(),
                            String::new() + &modified_variable_name[..index_opening_bracket] +
                                    &modified_variable_name[current_index..current_index_matching_bracket + 1],
                            variable_names.clone(),
                            "",
                            supports_pointer_dereferencing_and_referencing,
                            pos,
                        ));
                    }
                }
            }

            if let Some(returned_node) = returned_node {
                if let Some(dereferences) = dereferences {
                    let start_index = start_index.unwrap();

                    modified_variable_name = String::new() + &modified_variable_name[..start_index] +
                            dereferences + &modified_variable_name[start_index..];
                }

                match returned_node.node_data() {
                    //Variable was found without additional text -> valid pointer reference
                    NodeData::VariableName { .. } => {
                        let Some(text) = text else {
                            return Node::new_variable_name_node(pos, if let Some(module_name) = &module_name {
                                format!("[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}")
                            }else {
                                format!("{variable_prefix_append_after_search}{variable_name}")
                            }, None);
                        };

                        //Variable composition
                        return Node::new_list_node(vec![
                            Node::new_variable_name_node(pos, if let Some(module_name) = &module_name {
                                format!("[[{module_name}]]::{variable_prefix_append_after_search}{modified_variable_name}")
                            }else {
                                format!("{variable_prefix_append_after_search}{modified_variable_name}")
                            }, None),
                            Node::new_text_value_node(pos, text),
                        ]);
                    },

                    NodeData::List |
                    NodeData::TextValue(..) => {
                        //List: Variable was found with additional text -> no valid pointer reference
                        //TextValue: Variable was not found

                        return Node::new_variable_name_node(pos, if let Some(module_name) = &module_name {
                            format!("[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}")
                        }else {
                            format!("{variable_prefix_append_after_search}{variable_name}")
                        }, None);
                    },

                    _ => panic!("Invalid node"),
                }
            }
        }

        Node::new_text_value_node(pos, if let Some(module_name) = &module_name {
            format!("[[{module_name}]]::{variable_prefix_append_after_search}{variable_name}")
        }else {
            format!("{variable_prefix_append_after_search}{variable_name}")
        })
    }

    fn process_unprocessed_variable_name_node(&mut self, composite_type: OptionDataObjectRef, node: &Node) -> Node {
        let NodeData::UnprocessedVariableName(variable_name) = node.node_data() else {
            panic!("Invalid AST node");
        };

        if self.execution_flags.raw_variable_names {
            return Node::new_variable_name_node(node.pos(), variable_name.clone(), None);
        }

        let mut variable_name = variable_name.to_string();

        let is_module_variable = variable_name.starts_with("[[");
        let mut module_name = None;
        if is_module_variable {
            let index_module_identifier_end = variable_name.find("]]::");
            let Some(index_module_identifier_end) = index_module_identifier_end else {
                self.set_errno(InterpretingError::InvalidAstNode, Some("Invalid variable name"), node.pos());

                return Node::new_text_value_node(node.pos(), variable_name);
            };

            let module_name_local = &variable_name[2..index_module_identifier_end];
            if !Self::is_alpha_numeric_with_underline(module_name_local) {
                self.set_errno(InterpretingError::InvalidAstNode, Some("Invalid module name"), node.pos());

                return Node::new_text_value_node(node.pos(), variable_name);
            }
            module_name = Some(Box::from(module_name_local));
            variable_name = variable_name[index_module_identifier_end + 4..].to_string();
        }

        if let Some(composite_type) = &composite_type {
            if let Some(object_data) = composite_type.object_value() {
                if variable_name.starts_with("mp.") && !object_data.borrow().is_class() {
                    let variable_names = object_data.borrow().methods().
                            keys().
                            filter(|key| key.starts_with("mp.")).
                            map(|key| Rc::from(&**key)).
                            collect::<Box<_>>();

                    return self.convert_variable_name_to_variable_name_node_or_composition(
                        module_name,
                        variable_name,
                        variable_names,
                        "",
                        false,
                        node.pos(),
                    );
                }

                if variable_name.starts_with("op:") && !object_data.borrow().is_class() {
                    let variable_names = object_data.borrow().methods().
                            keys().
                            filter(|key| key.starts_with("op:")).
                            map(|key| Rc::from(&**key)).
                            collect::<Box<_>>();

                    return self.convert_variable_name_to_variable_name_node_or_composition(
                        module_name,
                        variable_name,
                        variable_names,
                        "",
                        false,
                        node.pos(),
                    );
                }

                if variable_name.starts_with("to:") && !object_data.borrow().is_class() {
                    let variable_names = object_data.borrow().methods().
                            keys().
                            filter(|key| key.starts_with("to:")).
                            map(|key| Rc::from(&**key)).
                            collect::<Box<_>>();

                    return self.convert_variable_name_to_variable_name_node_or_composition(
                        module_name,
                        variable_name,
                        variable_names,
                        "",
                        false,
                        node.pos(),
                    );
                }
            }
        }

        if variable_name.starts_with("$") || variable_name.starts_with("&") ||
                variable_name.starts_with("fp.") {
            let variable_names = if let Some(composite_type) = &composite_type {
                if composite_type.error_value().is_some() {
                    Box::from([
                        Rc::from("$text"),
                        Rc::from("$code"),
                        Rc::from("$message"),
                    ])
                }else if let Some(struct_data) = composite_type.struct_value() {
                    Box::from_iter(struct_data.member_names().into_iter().
                            map(Rc::from))
                }else if let Some(object_data) = composite_type.object_value() {
                    let mut variable_names = object_data.borrow().static_members().iter().
                            filter_map(|data_object| data_object.variable_name()).
                            map(Rc::from).
                            collect::<Vec<_>>();

                    if !object_data.borrow().is_class() {
                        if let Some(members) = object_data.borrow().members() {
                            variable_names.extend(members.iter().
                                    filter_map(|data_object| data_object.
                                            variable_name().
                                            map(Rc::from)));
                        }
                    }

                    variable_names.into_boxed_slice()
                }else {
                    self.set_errno(InterpretingError::InvalidArguments, Some("Invalid composite type"), node.pos());

                    return Node::new_text_value_node(node.pos(), variable_name);
                }
            }else {
                self.data_ref().var.keys().
                        cloned().
                        collect::<Box<_>>()
            };

            let supports_pointer_dereferencing_rand_referencing = variable_name.starts_with("$");
            return self.convert_variable_name_to_variable_name_node_or_composition(
                module_name,
                variable_name,
                variable_names,
                "",
                supports_pointer_dereferencing_rand_referencing,
                node.pos(),
            );
        }

        if composite_type.is_some() {
            self.set_errno(InterpretingError::InvalidAstNode, Some(&format!(
                "Invalid composite type member name: \"{variable_name}\"",
            )), node.pos());

            return Node::new_text_value_node(node.pos(), variable_name);
        }

        let is_linker_function;
        let prefix;

        if !is_module_variable && variable_name.starts_with("func.") {
            is_linker_function = false;
            prefix = "func.";

            variable_name = variable_name[5..].to_string();
        }else if !is_module_variable && variable_name.starts_with("fn.") {
            is_linker_function = false;
            prefix = "fn.";

            variable_name = variable_name[3..].to_string();
        }else if !is_module_variable && variable_name.starts_with("linker.") {
            is_linker_function = true;
            prefix = "linker.";

            variable_name = variable_name[7..].to_string();
        }else if !is_module_variable && variable_name.starts_with("ln.") {
            is_linker_function = true;
            prefix = "ln.";

            variable_name = variable_name[3..].to_string();
        }else {
            self.set_errno(InterpretingError::InvalidAstNode, Some("Invalid variable name"), node.pos());

            return Node::new_text_value_node(node.pos(), variable_name);
        }

        let variable_names = self.funcs.iter().
                filter(|(_, func)| func.linker_function() == is_linker_function).
                map(|(func_name, _)| Rc::from(&**func_name)).
                collect::<Box<_>>();

        self.convert_variable_name_to_variable_name_node_or_composition(
            None,
            variable_name,
            variable_names,
            prefix,
            false,
            node.pos(),
        )
    }

    fn process_function_call_previous_node_value_node(&mut self, node: &Node, previous_value: OptionDataObjectRef) -> Node {
        let NodeData::FunctionCallPreviousNodeValue {
            leading_whitespace,
            trailing_whitespace,
        } = node.node_data() else {
            panic!("Invalid AST node");
        };

        if let Some(previous_value) = previous_value {
            if previous_value.function_pointer_value().is_some() || previous_value.type_value().is_some() {
                return node.clone();
            }

            if let Some(struct_value) = previous_value.struct_value() {
                if struct_value.is_definition() {
                    return node.clone();
                }
            }

            if let Some(object_value) = previous_value.object_value() {
                if object_value.borrow().is_class() || object_value.borrow().methods().contains_key("op:call") {
                    return node.clone();
                }
            }
        }

        //Previous node value wasn't a function -> return children of node in between "(" and ")" as ListNode
        let mut nodes = Vec::with_capacity(2 + node.child_nodes().len());
        nodes.push(Node::new_text_value_node(node.pos(), format!("({leading_whitespace}")));
        nodes.append(&mut node.child_nodes().to_vec());
        nodes.push(Node::new_text_value_node(node.pos(), format!("{trailing_whitespace})")));

        Node::new_list_node(nodes)
    }

    fn interpret_list_node(&mut self, mut composite_type: OptionDataObjectRef, node: &Node) -> OptionDataObjectRef {
        let NodeData::List = node.node_data() else {
            panic!("Invalid AST node");
        };

        let mut data_objects = Vec::with_capacity(node.child_nodes().len());
        let mut previous_data_object = None;

        for child_node in node.child_nodes() {
            if matches!(child_node.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) &&
                    previous_data_object.is_some() {
                let ret = self.process_function_call_previous_node_value_node(
                    child_node,
                    previous_data_object.clone(),
                );

                if matches!(ret.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) {
                    //Remove last data Object, because it is used as function pointer for a function call
                    data_objects.pop();
                    data_objects.push(self.interpret_function_call_previous_node(&ret, previous_data_object.unwrap()));
                }else {
                    data_objects.push(self.interpret_node(None, &ret).unwrap());
                }

                previous_data_object = Some(data_objects[data_objects.len() - 1].clone());

                continue;
            }

            let ret = self.interpret_node(composite_type.take(), child_node);
            if let Some(ret) = &ret {
                data_objects.push(ret.clone());
            }

            previous_data_object = ret.clone();
        }

        utils::combine_data_objects(
            &data_objects,
            self,
            node.pos(),
        )
    }

    fn interpret_value_node(&mut self, node: &Node) -> DataObjectRef {
        match node.node_data() {
            NodeData::CharValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_char(*value)
                }).unwrap())
            },

            NodeData::TextValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_text(&**value)
                }).unwrap())
            },

            NodeData::IntValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_int(*value)
                }).unwrap())
            },

            NodeData::LongValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_long(*value)
                }).unwrap())
            },

            NodeData::FloatValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_float(*value)
                }).unwrap())
            },

            NodeData::DoubleValue(value) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_double(*value)
                }).unwrap())
            },

            NodeData::NullValue => {
                DataObjectRef::new(DataObject::new())
            },

            NodeData::VoidValue => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_void()
                }).unwrap())
            },

            _ => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_error(Rc::new(ErrorObject::new(
                        InterpretingError::InvalidAstNode,
                        None,
                    )))
                }).unwrap())
            },
        }
    }

    fn interpret_parsing_error_node(&mut self, node: &Node) -> DataObjectRef {
        let NodeData::ParsingError {
            error,
            message,
        } = node.node_data() else {
            panic!("Invalid AST node");
        };

        let error = match error {
            ParsingError::BracketMismatch => InterpretingError::BracketMismatch,

            ParsingError::ContFlowArgMissing => InterpretingError::ContFlowArgMissing,

            ParsingError::Eof => InterpretingError::Eof,

            ParsingError::InvalidConPart => InterpretingError::InvalidConPart,

            ParsingError::InvalidAssignment => InterpretingError::InvalidAssignment,

            ParsingError::InvalidParameter |
            ParsingError::LexerError => InterpretingError::InvalidAstNode,
        };

        self.set_errno_error_object(
            error,
            Some(message),
            node.pos(),
        )
    }

    /**
     * @return Returns true if any condition was true and if any block was executed
     */
    fn interpret_if_statement_node(&mut self, node: &Node) -> bool {
        let NodeData::IfStatement = node.node_data() else {
            panic!("Invalid AST node");
        };

        let nodes = node.child_nodes();
        if nodes.is_empty() {
            self.set_errno(
                InterpretingError::InvalidAstNode,
                Some("Empty if statement"),
                node.pos(),
            );

            return false;
        }

        for node in nodes {
            if self.interpret_if_statement_part_node(node) {
                return true;
            }
        }

        false
    }

    /**
     * @return Returns true if condition was true and if block was executed
     */
    fn interpret_if_statement_part_node(&mut self, node: &Node) -> bool {
        match node.node_data() {
            NodeData::IfStatementPartIf {
                if_body,
                condition,
            } => {
                let condition = self.interpret_operation_node(
                    condition.node(),
                ).unwrap();

                let condition = conversions::to_bool(self, &condition, node.pos());
                if condition {
                    self.interpret_ast(if_body);
                }

                condition
            },

            NodeData::IfStatementPartElse(if_body) => {
                self.interpret_ast(if_body);

                true
            },

            _ => panic!("Invalid AST node"),
        }
    }

    /**
     * @return Returns true if at least one loop iteration was executed
     */
    fn interpret_loop_statement_node(&mut self, node: &Node) -> bool {
        let NodeData::LoopStatement = node.node_data() else {
            panic!("Invalid AST node");
        };

        let nodes = node.child_nodes();
        if nodes.is_empty() {
            self.set_errno(
                InterpretingError::InvalidAstNode,
                Some("Empty loop statement"),
                node.pos(),
            );

            return false;
        }

        for node in nodes {
            if self.interpret_loop_statement_part_node(node) {
                return true;
            }
        }

        false
    }

    /**
     * @return false if not break or continue with level <= 1<br>
     * true if break or continue with level > 1
     */
    fn should_break_current_loop_iteration(&mut self) -> bool {
        if self.execution_state.stop_execution_flag {
            if self.execution_state.break_continue_count == 0 {
                return true;
            }

            //Handle continue and break
            self.execution_state.break_continue_count -= 1;
            if self.execution_state.break_continue_count > 0 {
                return true;
            }

            self.execution_state.stop_execution_flag = false;

            return !self.execution_state.is_continue_statement;
        }

        false
    }

    /**
     * @return Returns true if at least one loop iteration was executed
     */
    fn interpret_loop_statement_part_node(&mut self, node: &Node) -> bool {
        let mut flag = false;

        let ret = 'error: {
            match node.node_data() {
                NodeData::LoopStatementPartLoop(loop_body) => {
                    loop {
                        self.interpret_ast(loop_body);
                        if self.should_break_current_loop_iteration() {
                            return true;
                        }
                    }
                },

                NodeData::LoopStatementPartWhile {
                    loop_body,
                    condition,
                } => {
                    loop {
                        let condition = self.interpret_operation_node(
                            condition.node(),
                        ).unwrap();

                        let condition = conversions::to_bool(self, &condition, node.pos());
                        if !condition {
                            break;
                        }

                        flag = true;

                        self.interpret_ast(loop_body);
                        if self.should_break_current_loop_iteration() {
                            return true;
                        }
                    }

                    Ok(())
                },

                NodeData::LoopStatementPartUntil {
                    loop_body,
                    condition,
                } => {
                    loop {
                        let condition = self.interpret_operation_node(
                            condition.node(),
                        ).unwrap();

                        let condition = conversions::to_bool(self, &condition, node.pos());
                        if condition {
                            break;
                        }

                        flag = true;

                        self.interpret_ast(loop_body);
                        if self.should_break_current_loop_iteration() {
                            return true;
                        }
                    }

                    Ok(())
                },

                NodeData::LoopStatementPartRepeat {
                    loop_body,
                    var_pointer_node,
                    repeat_count_node,
                } => {
                    let var_pointer = self.interpret_node(None, var_pointer_node).unwrap();
                    let var = {
                        if !matches!(var_pointer.data_type(), DataType::VAR_POINTER | DataType::NULL) {
                            self.set_errno(
                                InterpretingError::IncompatibleDataType,
                                Some("con.repeat needs a variablePointer or a null value for the current iteration variable"),
                                node.pos(),
                            );

                            return false;
                        }

                        var_pointer.var_pointer_value()
                    };

                    let iterations = self.interpret_node(None, repeat_count_node).
                            and_then(|data_object| {
                                conversions::to_number(self, &data_object, node.pos())
                            });
                    let Some(iterations) = iterations else {
                        self.set_errno(
                            InterpretingError::IncompatibleDataType,
                            Some("con.repeat needs a repeat count value"),
                            node.pos(),
                        );

                        return false;
                    };

                    let iterations = iterations.int_value();
                    if iterations < 0 {
                        self.set_errno(
                            InterpretingError::IncompatibleDataType,
                            Some("con.repeat repeat count can not be less than 0"),
                            node.pos(),
                        );

                        return false;
                    }

                    for i in 0..iterations {
                        flag = true;

                        if let Some(var) = &var {
                            if var.is_final_data() || var.is_lang_var() {
                                self.set_errno(
                                    InterpretingError::FinalVarChange,
                                    Some("con.repeat current iteration value can not be set"),
                                    node.pos(),
                                );
                            }else {
                                let mut var = var.borrow_mut();

                                let ret = var.set_int(i);
                                if ret.is_err() {
                                    self.set_errno(
                                        InterpretingError::IncompatibleDataType,
                                        Some("con.repeat current iteration value can not be set"),
                                        node.pos(),
                                    );
                                }
                            }
                        }

                        self.interpret_ast(loop_body);
                        if self.should_break_current_loop_iteration() {
                            return true;
                        }
                    }

                    Ok(())
                },

                NodeData::LoopStatementPartForEach {
                    loop_body,
                    var_pointer_node,
                    composite_or_text_node,
                } => {
                    let var_pointer = self.interpret_node(None, var_pointer_node).unwrap();
                    let var = {
                        if let Some(var_pointer_value) = var_pointer.var_pointer_value() {
                            var_pointer_value
                        }else {
                            self.set_errno(
                                InterpretingError::IncompatibleDataType,
                                Some("con.foreach needs a variablePointer for the current element variable"),
                                node.pos(),
                            );

                            return false;
                        }
                    };

                    let pos = composite_or_text_node.pos();
                    let composition_or_text = self.interpret_node(None, composite_or_text_node).unwrap();
                    let iterator = operators::op_iter(self, &composition_or_text, pos);
                    let Some(iterator) = iterator else {
                        self.set_errno(
                            InterpretingError::IncompatibleDataType,
                            Some("The provided value to con.foreach does not support iteration"),
                            node.pos(),
                        );

                        return false;
                    };

                    loop {
                        let has_next = operators::op_has_next(self, &iterator, pos);
                        let Some(has_next) = has_next else {
                            self.set_errno(
                                InterpretingError::IncompatibleDataType,
                                Some("Invalid iterator implementation for value provided to con.foreach"),
                                node.pos(),
                            );

                            return false;
                        };

                        if !conversions::to_bool(self, &has_next, pos) {
                            break;
                        }

                        let next = operators::op_next(self, &iterator, pos);
                        let Some(next) = next else {
                            self.set_errno(
                                InterpretingError::IncompatibleDataType,
                                Some("Invalid iterator implementation for value provided to con.foreach"),
                                node.pos(),
                            );

                            return false;
                        };

                        flag = true;

                        {
                            if var.is_final_data() || var.is_lang_var() {
                                self.set_errno(
                                    InterpretingError::FinalVarChange,
                                    Some("con.foreach current element value can not be set"),
                                    node.pos(),
                                );
                            }else {
                                let mut var = var.borrow_mut();

                                let ret = var.set_data(&next.borrow());
                                if ret.is_err() {
                                    break 'error ret.map(|_| ());
                                }
                            }
                        }

                        self.interpret_ast(loop_body);
                        if self.should_break_current_loop_iteration() {
                            return true;
                        }
                    }

                    Ok(())
                },

                NodeData::LoopStatementPartElse(loop_body) => {
                    flag = true;
                    self.interpret_ast(loop_body);

                    Ok(())
                },

                _ => panic!("Invalid AST node"),
            }
        };
        if let Err(e) = ret {
            self.set_errno(
                InterpretingError::IncompatibleDataType,
                Some(e.message()),
                node.pos(),
            );

            return false;
        }

        flag
    }

    fn interpret_loop_statement_continue_break(&mut self, node: &Node) {
        let NodeData::ContinueBreakStatement {
            continue_node,
            number_node,
        } = node.node_data() else {
            panic!("Invalid AST node");
        };

        if let Some(number_node) = number_node {
            let level = self.interpret_node(None, number_node).
                    and_then(|data_object| {
                        conversions::to_number(self, &data_object, node.pos())
                    });
            let Some(level) = level else {
                self.set_errno(
                    InterpretingError::IncompatibleDataType,
                    Some(&format!(
                        "con.{} needs either non value or a level number",
                        if *continue_node {
                            "continue"
                        }else {
                            "break"
                        }
                    )),
                    node.pos(),
                );

                return;
            };

            let level = level.int_value();
            if level < 1 {
                self.execution_state.break_continue_count = 0;

                self.set_errno(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "con.{} the level must be > 0",
                        if *continue_node {
                            "continue"
                        }else {
                            "break"
                        }
                    )),
                    node.pos(),
                );

                return;
            }

            self.execution_state.break_continue_count = level as u32;
        }else {
            self.execution_state.break_continue_count = 1;
        }

        self.execution_state.is_continue_statement = *continue_node;
        self.execution_state.stop_execution_flag = true;
    }

    fn save_execution_stop_state_to_var_and_reset(&mut self, saved_execution_state: &mut ExecutionState) {
        saved_execution_state.stop_execution_flag = self.execution_state.stop_execution_flag;
        saved_execution_state.returned_or_thrown_value = self.execution_state.returned_or_thrown_value.take();
        saved_execution_state.is_thrown_value = self.execution_state.is_thrown_value;
        saved_execution_state.return_or_throw_statement_pos = self.execution_state.return_or_throw_statement_pos;
        saved_execution_state.break_continue_count = self.execution_state.break_continue_count;
        saved_execution_state.is_continue_statement = self.execution_state.is_continue_statement;
        self.execution_state.stop_execution_flag = false;
        //self.execution_state.returned_or_thrown_value will be set to None because of ".take()"
        self.execution_state.is_thrown_value = false;
        self.execution_state.return_or_throw_statement_pos = CodePosition::EMPTY;
        self.execution_state.break_continue_count = 0;
        self.execution_state.is_continue_statement = false;
    }
    /**
     * @return Returns true if a catch or an else block was executed
     */
    fn interpret_try_statement_node(&mut self, node: &Node) -> bool {
        let NodeData::TryStatement = node.node_data() else {
            panic!("Invalid AST node");
        };

        let nodes = node.child_nodes();
        if nodes.is_empty() {
            self.set_errno(
                InterpretingError::InvalidAstNode,
                Some("Empty try statement"),
                node.pos(),
            );

            return false;
        }

        let mut saved_execution_state = ExecutionState::new();

        let try_part = &nodes[0];
        if !matches!(try_part.node_data(), NodeData::TryStatementPartTry(..) | NodeData::TryStatementPartSoftTry(..) |
            NodeData::TryStatementPartNonTry(..)) {
            self.set_errno(
                InterpretingError::InvalidAstNode,
                Some("First part of try statement was no try nor soft try nor non try part"),
                node.pos(),
            );

            return false;
        }
        self.interpret_try_statement_part_node(try_part);

        if self.execution_state.stop_execution_flag {
            self.save_execution_stop_state_to_var_and_reset(&mut saved_execution_state);
        }

        let mut flag = false;
        if saved_execution_state.stop_execution_flag && self.execution_state.try_thrown_error.is_some() {
            let catch_parts = nodes.iter().
                    skip(1).
                    take_while(|node| matches!(node.node_data(), NodeData::TryStatementPartCatch {..}));

            for node in catch_parts {
                flag = self.interpret_try_statement_part_node(node);
                if flag {
                    if self.execution_state.stop_execution_flag {
                        self.save_execution_stop_state_to_var_and_reset(&mut saved_execution_state);
                    }else {
                        //Reset saved execution state because the reason of the execution stop was handled by the catch block
                        saved_execution_state = ExecutionState::new();
                        self.execution_state.try_thrown_error = None;

                        //Error was handled and (the try statement is the most outer try statement or no other error was thrown): reset $LANG_ERRNO
                        self.get_and_clear_errno_error_object();
                    }

                    break;
                }
            }
        }

        let saved_stop_execution_flag_for_else_block = saved_execution_state.stop_execution_flag;

        //Cancel execution stop because of error if most outer try block is reached or if inside a nontry statement
        if saved_execution_state.stop_execution_flag && (saved_execution_state.try_thrown_error.is_none() || saved_execution_state.try_block_level == 0 ||
                (saved_execution_state.is_soft_try && saved_execution_state.try_body_scope_id != self.scope_id)) {
            saved_execution_state.stop_execution_flag = false;
        }

        if !flag && !saved_stop_execution_flag_for_else_block && nodes.len() > 1 {
            let mut node = None;

            if matches!(nodes[nodes.len() - 2].node_data(), NodeData::TryStatementPartElse(..)) {
                node = Some(&nodes[nodes.len() - 2]);
            }else if matches!(nodes[nodes.len() - 1].node_data(), NodeData::TryStatementPartElse(..)) {
                node = Some(&nodes[nodes.len() - 1]);
            }

            if let Some(node) = node {
                flag = self.interpret_try_statement_part_node(node);

                if self.execution_state.stop_execution_flag {
                    self.save_execution_stop_state_to_var_and_reset(&mut saved_execution_state);
                }
            }
        }

        if nodes.len() > 1 && matches!(nodes[nodes.len() - 1].node_data(), NodeData::TryStatementPartFinally(..)) {
            self.interpret_try_statement_part_node(&nodes[nodes.len() - 1]);
        }

        //Reset saved execution flag to stop execution if finally has not set the stop execution flag
        if !self.execution_state.stop_execution_flag {
            self.execution_state.stop_execution_flag = saved_execution_state.stop_execution_flag;
            self.execution_state.returned_or_thrown_value = saved_execution_state.returned_or_thrown_value;
            self.execution_state.is_thrown_value = saved_execution_state.is_thrown_value;
            self.execution_state.return_or_throw_statement_pos = saved_execution_state.return_or_throw_statement_pos;
            self.execution_state.break_continue_count = saved_execution_state.break_continue_count;
            self.execution_state.is_continue_statement = saved_execution_state.is_continue_statement;
        }

        flag
    }

    /**
     * @return Returns true if a catch or an else block was executed
     */
    fn interpret_try_statement_part_node(&mut self, node: &Node) -> bool {
        let mut flag = false;

        match node.node_data() {
            NodeData::TryStatementPartTry(try_body) |
            NodeData::TryStatementPartSoftTry(try_body) => {
                self.execution_state.try_thrown_error = None;
                self.execution_state.try_block_level += 1;
                let is_old_soft_try_old = self.execution_state.is_soft_try;
                self.execution_state.is_soft_try = matches!(node.node_data(), NodeData::TryStatementPartSoftTry(..));
                let old_try_block_scope_id = self.execution_state.try_body_scope_id;
                self.execution_state.try_body_scope_id = self.scope_id;

                self.interpret_ast(try_body);

                self.execution_state.try_block_level -= 1;
                self.execution_state.is_soft_try = is_old_soft_try_old;
                self.execution_state.try_body_scope_id = old_try_block_scope_id;
            },

            NodeData::TryStatementPartNonTry(try_body) => {
                self.execution_state.try_thrown_error = None;
                let old_try_block_level = self.execution_state.try_block_level;
                self.execution_state.try_block_level = 0;
                let is_old_soft_try_old = self.execution_state.is_soft_try;
                self.execution_state.is_soft_try = false;
                let old_try_block_scope_id = self.execution_state.try_body_scope_id;
                self.execution_state.try_body_scope_id = 0;

                self.interpret_ast(try_body);

                self.execution_state.try_block_level = old_try_block_level;
                self.execution_state.is_soft_try = is_old_soft_try_old;
                self.execution_state.try_body_scope_id = old_try_block_scope_id;
            },

            NodeData::TryStatementPartCatch {
                try_body,
                errors,
            } => {
                let Some(try_thrown_error) = self.execution_state.try_thrown_error else {
                    return false;
                };

                if let Some(errors) = errors {
                    if errors.is_empty() {
                        self.set_errno(
                            InterpretingError::InvalidAstNode,
                            Some(
                                "Empty catch part \"catch()\" is not allowed!\n\
                                For checking all warnings \"catch\" without \"()\" should be used",
                            ),
                            node.pos(),
                        );

                        return false;
                    }

                    let mut data_objects = Vec::new();
                    let mut found_error = false;
                    let mut previous_data_object = None;
                    for child_node in errors {
                        if matches!(child_node.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) &&
                                previous_data_object.is_some() {
                            let ret = self.process_function_call_previous_node_value_node(
                                child_node,
                                previous_data_object.clone(),
                            );

                            if matches!(ret.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) {
                                //Remove last data Object, because it is used as function pointer for a function call
                                data_objects.pop();
                                data_objects.push(self.interpret_function_call_previous_node(&ret, previous_data_object.unwrap()));
                            }else {
                                data_objects.push(self.interpret_node(None, &ret).unwrap());
                            }

                            previous_data_object = Some(data_objects[data_objects.len() - 1].clone());

                            continue;
                        }

                        let ret = self.interpret_node(None, child_node);
                        if let Some(ret) = &ret {
                            data_objects.push(ret.clone());
                        }

                        previous_data_object = ret;
                    }

                    let error_list = utils::combine_arguments_without_argument_separators(
                        &data_objects, self, node.pos(),
                    );

                    for data_object in error_list {
                        let Some(error_value) =  data_object.error_value() else {
                            self.set_errno(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "Variable with type other than {:?} in catch statement",
                                    DataType::ERROR,
                                )),
                                node.pos(),
                            );

                            continue;
                        };

                        if error_value.err() == try_thrown_error {
                            found_error = true;
                        }
                    }

                    if !found_error {
                        return false;
                    }
                }

                flag = true;

                self.interpret_ast(try_body);
            },

            NodeData::TryStatementPartElse(try_body) => {
                if self.execution_state.try_thrown_error.is_some() {
                    return false;
                }

                flag = true;

                self.interpret_ast(try_body);
            },

            NodeData::TryStatementPartFinally(try_body) => {
                self.interpret_ast(try_body);
            },

            _ => panic!("Invalid AST node"),
        }

        flag
    }

    fn interpret_operation_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let operation = match node.node_data() {
            NodeData::Operation(operation) |
            NodeData::Math(operation) |
            NodeData::Condition(operation) => operation,

            _ => panic!("Invalid AST node"),
        };

        let left_side_operand = if operation.operator().is_unary() &&
                operation.operator().lazy_evaluation() {
            None
        }else {
            self.interpret_node(None, operation.left_side_operand().unwrap())
        };

        let mut middle_operand = if !operation.operator().is_ternary() ||
                operation.operator().lazy_evaluation() {
            None
        }else {
            self.interpret_node(None, operation.middle_operand().unwrap())
        };

        let mut right_side_operand = if operation.operator().is_unary() ||
                operation.operator().lazy_evaluation() {
            None
        }else {
            self.interpret_node(None, operation.right_side_operand().unwrap())
        };

        //Forward Rust None values for NON operators
        if left_side_operand.is_none() && matches!(operation.operator(),
            Operator::Non | Operator::MathNon | Operator::ConditionalNon) {
            return None;
        }

        //Allow None values in slice and replace with Lang VOID values
        if matches!(operation.operator(), Operator::Slice | Operator::OptionalSlice) {
            if middle_operand.is_none() {
                middle_operand = Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_void()
                }).unwrap()));
            }

            if right_side_operand.is_none() {
                right_side_operand = Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_void()
                }).unwrap()));
            }
        }

        if (left_side_operand.is_none() && (!operation.operator().is_unary() || !operation.operator().lazy_evaluation())) ||
                (!operation.operator().lazy_evaluation() && ((!operation.operator().is_unary() && right_side_operand.is_none()) ||
                        (operation.operator().is_ternary() && middle_operand.is_none()))) {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Missing operand"),
                node.pos(),
            ));
        }

        match operation.operator_type() {
            OperatorType::All => {
                let output = match operation.operator() {
                    Operator::Comma => {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidAstNode,
                            Some("The COMMA operator is parser-only (If you meant the text value of \",\", you must escape the COMMA operator: \"\\,\")"),
                            node.pos(),
                        ));
                    },

                    Operator::OptionalGetItem => {
                        operators::op_optional_get_item(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::GetItem => {
                        operators::op_get_item(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::OptionalSlice => {
                        operators::op_optional_slice(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            middle_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Slice => {
                        operators::op_slice(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            middle_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::MemberAccessPointer => {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        let Some(left_side_operand) = left_side_operand.var_pointer_value() else {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "The left side operand of the member access pointer operator (\"{}\") must be a pointer",
                                    operation.operator().symbol(),
                                )),
                                node.pos(),
                            ));
                        };

                        if !utils::is_member_access_allowed(&left_side_operand) {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "The left side operand of the member access pointer operator (\"{}\") must be a pointer pointing to a composite type",
                                    operation.operator().symbol(),
                                )),
                                node.pos(),
                            ));
                        }

                        return self.interpret_node(Some(left_side_operand), operation.right_side_operand().unwrap());
                    },

                    Operator::MemberAccess => {
                        let left_side_operand = left_side_operand.unwrap();

                        let is_super_keyword = {
                            if let Some(text_value) = left_side_operand.text_value() {
                                matches!(operation.left_side_operand().unwrap().node_data(), NodeData::TextValue(..)) &&
                                        &*text_value == "super"
                            }else {
                                false
                            }
                        };

                        if !is_super_keyword && !utils::is_member_access_allowed(&left_side_operand) {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "The left side operand of the member access operator (\"{}\") must be a composite type",
                                    operation.operator().symbol(),
                                )),
                                node.pos(),
                            ));
                        }

                        return self.interpret_node(Some(left_side_operand), operation.right_side_operand().unwrap());
                    },

                    Operator::MemberAccessThis => {
                        let composite_type = self.data_ref().var.get("&this").cloned();

                        if composite_type.as_ref().is_none_or(|composite_type|
                                !matches!(composite_type.data_type(), DataType::STRUCT | DataType::OBJECT)) {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "\"&this\" is not present or invalid for the member access this operator (\"{}\")",
                                    operation.operator().symbol(),
                                )),
                                node.pos(),
                            ));
                        }

                        return self.interpret_node(composite_type, operation.left_side_operand().unwrap());
                    },

                    Operator::OptionalMemberAccess => {
                        let left_side_operand = left_side_operand.unwrap();
                        if matches!(left_side_operand.data_type(), DataType::NULL | DataType::VOID) {
                            return Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                                data_object.set_void()
                            }).unwrap()));
                        }

                        if !utils::is_member_access_allowed(&left_side_operand) {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "The left side operand of the optional member access operator (\"{}\") must be a composite type",
                                    operation.operator().symbol(),
                                )),
                                node.pos(),
                            ));
                        }

                        return self.interpret_node(Some(left_side_operand), operation.right_side_operand().unwrap());
                    },

                    _ => panic!("Invalid AST node"),
                };

                if output.is_none() {
                    let left_side_operand_data_type = {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        format!("{:?}", left_side_operand.data_type())
                    };

                    let middle_operand_data_type = {
                        if operation.operator().is_ternary() {
                            let middle_operand = middle_operand.as_ref().unwrap();

                            format!(", {:?}, ", middle_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    let right_side_operand_data_type = {
                        if !operation.operator().is_unary() {
                            let right_side_operand = right_side_operand.as_ref().unwrap();

                            format!(" and {:?}", right_side_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The \"{}\" operator is not defined for {}{}{}",
                            operation.operator().symbol(),
                            left_side_operand_data_type,
                            middle_operand_data_type,
                            right_side_operand_data_type,
                        )),
                        node.pos(),
                    ));
                }

                output
            },

            OperatorType::General => {
                let output = match operation.operator() {
                    //Unary
                    Operator::Non => {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        Some(left_side_operand.clone())
                    },

                    Operator::Len => {
                        operators::op_len(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::DeepCopy => {
                        operators::op_deep_copy(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    //Binary
                    Operator::Concat => {
                        operators::op_concat(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Spaceship => {
                        operators::op_spaceship(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Elvis => {
                        let left_side_operand = left_side_operand.unwrap();
                        if conversions::to_bool(self, &left_side_operand, node.pos()) {
                            return Some(left_side_operand);
                        }

                        let right_side_operand = self.interpret_node(
                            None, operation.right_side_operand().unwrap(),
                        );

                        if right_side_operand.is_none() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Missing operand"),
                                node.pos(),
                            ));
                        }

                        return right_side_operand;
                    },

                    Operator::NullCoalescing => {
                        let left_side_operand = left_side_operand.unwrap();
                        if !matches!(left_side_operand.data_type(), DataType::NULL | DataType::VOID) {
                            return Some(left_side_operand);
                        }

                        let right_side_operand = self.interpret_node(
                            None, operation.right_side_operand().unwrap(),
                        );

                        if right_side_operand.is_none() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Missing operand"),
                                node.pos(),
                            ));
                        }

                        return right_side_operand;
                    },

                    //Ternary
                    Operator::InlineIf => {
                        let left_side_operand = left_side_operand.unwrap();

                        let operand = if conversions::to_bool(self, &left_side_operand, node.pos()) {
                            self.interpret_node(None, operation.middle_operand().unwrap())
                        }else {
                            self.interpret_node(None, operation.right_side_operand().unwrap())
                        };

                        if operand.is_none() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Missing operand"),
                                node.pos(),
                            ));
                        }

                        return operand;
                    },

                    _ => panic!("Invalid AST node"),
                };

                if output.is_none() {
                    let left_side_operand_data_type = {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        format!("{:?}", left_side_operand.data_type())
                    };

                    let middle_operand_data_type = {
                        if operation.operator().is_ternary() {
                            let middle_operand = middle_operand.as_ref().unwrap();

                            format!(", {:?}, ", middle_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    let right_side_operand_data_type = {
                        if !operation.operator().is_unary() {
                            let right_side_operand = right_side_operand.as_ref().unwrap();

                            format!(" and {:?}", right_side_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The \"{}\" operator is not defined for {}{}{}",
                            operation.operator().symbol(),
                            left_side_operand_data_type,
                            middle_operand_data_type,
                            right_side_operand_data_type,
                        )),
                        node.pos(),
                    ));
                }

                output
            },

            OperatorType::Math => {
                let output = match operation.operator() {
                    //Unary
                    Operator::Non => {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        Some(left_side_operand.clone())
                    },

                    Operator::Pos => {
                        operators::op_pos(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Inv => {
                        operators::op_inv(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::BitwiseNot => {
                        operators::op_not(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Inc => {
                        operators::op_inc(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Dec => {
                        operators::op_dec(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    //Binary
                    Operator::Pow => {
                        operators::op_pow(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Mul => {
                        operators::op_mul(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Div => {
                        operators::op_div(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::TruncDiv => {
                        operators::op_trunc_div(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::FloorDiv => {
                        operators::op_floor_div(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::CeilDiv => {
                        operators::op_ceil_div(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Mod => {
                        operators::op_mod(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Add => {
                        operators::op_add(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Sub => {
                        operators::op_sub(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Lshift => {
                        operators::op_lshift(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Rshift => {
                        operators::op_rshift(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::Rzshift => {
                        operators::op_rzshift(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::BitwiseAnd => {
                        operators::op_and(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::BitwiseXor => {
                        operators::op_xor(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::BitwiseOr => {
                        operators::op_or(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    _ => panic!("Invalid AST node"),
                };

                if output.is_none() {
                    let left_side_operand_data_type = {
                        let left_side_operand = left_side_operand.as_ref().unwrap();

                        format!("{:?}", left_side_operand.data_type())
                    };

                    let middle_operand_data_type = {
                        if operation.operator().is_ternary() {
                            let middle_operand = middle_operand.as_ref().unwrap();

                            format!(", {:?}, ", middle_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    let right_side_operand_data_type = {
                        if !operation.operator().is_unary() {
                            let right_side_operand = right_side_operand.as_ref().unwrap();

                            format!(" and {:?}", right_side_operand.data_type())
                        }else {
                            String::new()
                        }
                    };

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The \"{}\" operator is not defined for {}{}{}",
                            operation.operator().symbol(),
                            left_side_operand_data_type,
                            middle_operand_data_type,
                            right_side_operand_data_type,
                        )),
                        node.pos(),
                    ));
                }

                output
            },

            OperatorType::Condition => {
                let condition_output = match operation.operator() {
                    //Unary (Logical operators)
                    Operator::ConditionalNon | Operator::Not => {
                        let condition_output = conversions::to_bool(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        );

                        //Invert if "Not"
                        condition_output ^ matches!(operation.operator(), Operator::Not)
                    },

                    //Binary (Logical operators)
                    Operator::And => {
                        let left_side_condition_output = conversions::to_bool(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        );

                        left_side_condition_output && {
                            let right_side_operand = self.interpret_node(None, operation.right_side_operand().unwrap());
                            let Some(right_side_operand) = right_side_operand else {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::InvalidAstNode,
                                    Some("Missing operand"),
                                    node.pos(),
                                ));
                            };

                            conversions::to_bool(
                                self,
                                &right_side_operand,
                                node.pos(),
                            )
                        }
                    },

                    Operator::Or => {
                        let left_side_condition_output = conversions::to_bool(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        );

                        left_side_condition_output || {
                            let right_side_operand = self.interpret_node(None, operation.right_side_operand().unwrap());
                            let Some(right_side_operand) = right_side_operand else {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::InvalidAstNode,
                                    Some("Missing operand"),
                                    node.pos(),
                                ));
                            };

                            conversions::to_bool(
                                self,
                                &right_side_operand,
                                node.pos(),
                            )
                        }
                    },

                    //Binary (Comparison operators)
                    Operator::InstanceOf => {
                        let left_side_operand = left_side_operand.as_ref().unwrap();
                        let right_side_operand = right_side_operand.as_ref().unwrap();

                        if let Some(type_value) = right_side_operand.type_value() {
                            left_side_operand.data_type() == type_value
                        }else if let Some(struct_definition) = right_side_operand.struct_value() {
                            if !struct_definition.is_definition() {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::InvalidArguments,
                                    Some(&format!(
                                        "The second operand of the \"{}\" operator must be a struct definition",
                                        operation.operator().symbol(),
                                    )),
                                    node.pos(),
                                ));
                            }

                            if let Some(struct_value) = left_side_operand.struct_value() {
                                !struct_value.is_definition() && ptr::eq(
                                    struct_value.base_definition().unwrap().deref(),
                                    struct_definition.deref(),
                                )
                            }else {
                                false
                            }
                        }else if let Some(class_value) = right_side_operand.object_value() {
                            if !class_value.borrow().is_class() {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::InvalidArguments,
                                    Some(&format!(
                                        "The second operand of the \"{}\" operator must be a class",
                                        operation.operator().symbol(),
                                    )),
                                    node.pos(),
                                ));
                            }

                            if let Some(object_value) = left_side_operand.object_value() {
                                !object_value.borrow().is_class() && object_value.borrow().is_instance_of(class_value.borrow().deref())
                            }else {
                                false
                            }
                        }else {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some(&format!(
                                    "The second operand of the \"{}\" operator must be of type {:?}, {:?}, or {:?}",
                                    operation.operator().symbol(),
                                    DataType::TYPE,
                                    DataType::STRUCT,
                                    DataType::OBJECT,
                                )),
                                node.pos(),
                            ));
                        }
                    },

                    Operator::Equals | Operator::NotEquals => {
                        //Invert if "NotEquals"
                        operators::is_equals(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        ) ^ matches!(operation.operator(), Operator::NotEquals)
                    },

                    Operator::Matches | Operator::NotMatches => {
                        let left_side_text = conversions::to_text(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            node.pos(),
                        );

                        let right_side_text = conversions::to_text(
                            self,
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        );

                        //Invert if "NotMatches"
                        let match_result = regex::matches(&left_side_text, &right_side_text);
                        match match_result {
                            Ok(is_match) => is_match ^ matches!(operation.operator(), Operator::NotMatches),

                            Err(error) => {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::InvalidRegexSyntax,
                                    Some(error.message()),
                                    node.pos(),
                                ));
                            },
                        }
                    },

                    Operator::StrictEquals | Operator::StrictNotEquals => {
                        //Invert if "StrictNotEquals"
                        operators::is_strict_equals(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        ) ^ matches!(operation.operator(), Operator::StrictNotEquals)
                    },

                    Operator::LessThan => {
                        operators::is_less_than(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::GreaterThan => {
                        operators::is_greater_than(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::LessThanOrEquals => {
                        operators::is_less_than_or_equals(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    Operator::GreaterThanOrEquals => {
                        operators::is_greater_than_or_equals(
                            self,
                            left_side_operand.as_ref().unwrap(),
                            right_side_operand.as_ref().unwrap(),
                            node.pos(),
                        )
                    },

                    _ => panic!("Invalid AST node"),
                };

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(condition_output)
                }).unwrap()))
            },
        }
    }

    fn interpret_return_node(&mut self, node: &Node) {
        let NodeData::Return = node.node_data() else {
            panic!("Invalid AST node");
        };

        let returned_value_node = node.child_nodes().first();
        let returned_value = returned_value_node.and_then(|node| self.interpret_node(None, node));

        self.execution_state.returned_or_thrown_value = returned_value;
        self.execution_state.return_or_throw_statement_pos = node.pos();
        self.execution_state.stop_execution_flag = true;
    }

    fn interpret_throw_node(&mut self, node: &Node) {
        let NodeData::Throw = node.node_data() else {
            panic!("Invalid AST node");
        };

        let thrown_node = &node.child_nodes()[0];
        let error_object = self.interpret_node(None, thrown_node);

        let error_type;
        let error_message;

        //TODO improve when if let chains become stable
        'skip_error: {
            if let Some(error_object) = &error_object {
                if let Some(error_value) = error_object.error_value() {
                    let message_node = node.child_nodes().get(1);
                    let message_object = message_node.
                            and_then(|node| self.interpret_node(None, node));

                    error_type = error_value.err();

                    if let Some(message_object) = message_object {
                        error_message = Some(conversions::to_text(self, &message_object, message_node.unwrap().pos()));

                        let error_message = error_message.as_deref();
                        self.execution_state.returned_or_thrown_value = Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_error(Rc::new(ErrorObject::new(error_value.err(), error_message)))
                        }).unwrap()));
                    }else {
                        error_message = None;

                        self.execution_state.returned_or_thrown_value = Some(error_object.clone());
                    }

                    break 'skip_error;
                }
            }

            error_type = InterpretingError::IncompatibleDataType;
            error_message = None;

            self.execution_state.returned_or_thrown_value = Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_error(Rc::new(ErrorObject::new(error_type, None)))
            }).unwrap()));
        }
        self.execution_state.is_thrown_value = error_type.error_code() > 0;
        self.execution_state.return_or_throw_statement_pos = node.pos();
        self.execution_state.stop_execution_flag = true;

        if self.execution_state.is_thrown_value && self.scope_id > -1 {
            self.set_errno(error_type, error_message.as_deref(), self.execution_state.return_or_throw_statement_pos);
        }

        if self.execution_state.is_thrown_value && self.execution_state.try_block_level > 0 &&
                (!self.execution_state.is_soft_try || self.execution_state.try_body_scope_id == self.scope_id) {
            self.execution_state.try_thrown_error = Some(error_type);
            self.execution_state.stop_execution_flag = true;
        }
    }

    fn interpret_lang_data_and_execution_flags(
        &mut self,
        lang_data_execution_flag: &str,
        value: &DataObjectRef,
        pos: CodePosition,
    ) {
        match lang_data_execution_flag {
            //Data
            "lang.version" => {
                let lang_ver = conversions::to_text(self, value, pos);
                let comp_ver = utils::compare_versions_str(Self::VERSION, &lang_ver);
                let Some(comp_ver) = comp_ver else {
                    self.set_errno(
                        InterpretingError::LangVerError,
                        Some("lang.version has an invalid format"),
                        pos,
                    );

                    return;
                };

                match comp_ver {
                    Ordering::Less => {
                        self.set_errno(
                            InterpretingError::LangVerWarning,
                            Some("Lang file's version is older than this version! The Lang file could not be executed correctly"),
                            pos,
                        );
                    },

                    Ordering::Greater => {
                        self.set_errno(
                            InterpretingError::LangVerError,
                            Some("Lang file's version is newer than this version! The Lang file will not be executed correctly!"),
                            pos,
                        );
                    },

                    _ => {},
                }
            },

            "lang.name" => {
                //Nothing to do
            },

            //Flags
            "lang.allowTermRedirect" => {
                let number = conversions::to_number(self, value, pos);
                let Some(number) = number else {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("Invalid Data Type for the lang.allowTermRedirect flag!"),
                        pos,
                    );

                    return;
                };

                self.execution_flags.allow_term_redirect = number.int_value() != 0;
            },

            "lang.errorOutput" => {
                let number = conversions::to_number(self, value, pos);
                let Some(number) = number else {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("Invalid Data Type for the lang.errorOutput flag!"),
                        pos,
                    );

                    return;
                };

                self.execution_flags.error_output = ErrorOutputFlag::get_error_flag_for(number.int_value());
            },

            "lang.test" => {
                let number = conversions::to_number(self, value, pos);
                let Some(number) = number else {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("Invalid Data Type for the lang.test flag!"),
                        pos,
                    );

                    return;
                };

                let lang_test_new_value = number.int_value() != 0;

                if self.execution_flags.lang_test && !lang_test_new_value {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("The lang.test flag can not be changed if it was once set to true!"),
                        pos,
                    );

                    return;
                }

                self.execution_flags.lang_test = lang_test_new_value;
            },

            "lang.rawVariableNames" => {
                let number = conversions::to_number(self, value, pos);
                let Some(number) = number else {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("Invalid Data Type for the lang.rawVariableNames flag!"),
                        pos,
                    );

                    return;
                };

                self.execution_flags.raw_variable_names = number.int_value() != 0;
            },

            "lang.nativeStackTraces" => {
                let number = conversions::to_number(self, value, pos);
                let Some(number) = number else {
                    self.set_errno(
                        InterpretingError::InvalidArguments,
                        Some("Invalid Data Type for the lang.nativeStackTraces flag!"),
                        pos,
                    );

                    return;
                };

                self.execution_flags.native_stack_traces = number.int_value() != 0;
            },

            _ => {
                self.set_errno(
                    InterpretingError::InvalidExecFlagData,
                    Some(&format!("\"{lang_data_execution_flag}\" is neither Lang data nor an execution flag")),
                    pos,
                );
            }
        }
    }

    fn interpret_assigment_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let NodeData::Assignment = node.node_data() else {
            panic!("Invalid AST node");
        };

        let lvalue_node = &node.child_nodes()[0];
        let rvalue_node = &node.child_nodes()[1];

        let rvalue = self.interpret_node(None, rvalue_node);
        let rvalue = rvalue.unwrap_or_else(|| {
            //Set rvalue to null data object
            DataObjectRef::new(DataObject::new())
        });

        match lvalue_node.node_data() {
            NodeData::Operation(operation) |
            NodeData::Math(operation) |
            NodeData::Condition(operation) => {
                //Composite type lvalue assignment (MEMBER_ACCESS, MEMBER_ACCESS_THIS, and GET_ITEM)
                let mut operation_node = lvalue_node;
                let mut operation = operation;

                while matches!(operation.operator(), Operator::Non | Operator::MathNon | Operator::ConditionalNon) {
                    let left_side_operand = operation.left_side_operand().unwrap();
                    match left_side_operand.node_data() {
                        NodeData::Operation(left_side_operation) |
                        NodeData::Math(left_side_operation) |
                        NodeData::Condition(left_side_operation) => {
                            operation_node = left_side_operand;
                            operation = left_side_operation;
                        },

                        _ => {},
                    }
                }

                let is_member_access_pointer_operator = matches!(operation.operator(), Operator::MemberAccessPointer);
                match operation.operator() {
                    Operator::MemberAccessPointer |
                    Operator::MemberAccess |
                    Operator::MemberAccessThis => {
                        let lvalue = self.interpret_operation_node(operation_node);
                        let Some(lvalue) = lvalue else {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some(&format!(
                                    "Invalid arguments for member access{}",
                                    if is_member_access_pointer_operator {
                                        " pointer"
                                    }else {
                                        ""
                                    }
                                )),
                                node.pos(),
                            ));
                        };

                        let mut lvalue = lvalue.borrow_mut();
                        if lvalue.variable_name().is_none() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAssignment,
                                Some("Anonymous values can not be changed"),
                                node.pos(),
                            ));
                        }

                        if lvalue.is_final_data() || lvalue.is_lang_var() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::FinalVarChange,
                                None,
                                node.pos(),
                            ));
                        }

                        let ret = lvalue.set_data(&rvalue.borrow());
                        if ret.is_err() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::IncompatibleDataType,
                                Some("Incompatible type for rvalue in assignment"),
                                node.pos(),
                            ));
                        }

                        return Some(rvalue);
                    },

                    Operator::GetItem => {
                        let composite_type_object = self.interpret_node(None, operation.left_side_operand().unwrap());
                        let Some(composite_type_object) = composite_type_object else {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Missing composite type operand for set item"),
                                node.pos(),
                            ));
                        };

                        let index_object = self.interpret_node(None, operation.right_side_operand().unwrap());
                        let Some(index_object) = index_object else {
                            return Some(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Missing index operand for set item"),
                                node.pos(),
                            ));
                        };

                        let ret = operators::op_set_item(
                            self,
                            &composite_type_object,
                            &index_object,
                            &rvalue,
                            operation_node.pos(),
                        );
                        if ret.is_none() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::IncompatibleDataType,
                                Some("Incompatible type for lvalue (composite type + index) or rvalue in assignment"),
                                node.pos(),
                            ));
                        }

                        return Some(rvalue);
                    },

                    _ => {},
                }

                //Continue in "Lang translation" below
            },

            NodeData::UnprocessedVariableName(variable_name) => {
                let mut variable_name = variable_name.to_string();

                let is_module_variable = variable_name.starts_with("[[");
                let module_name = if is_module_variable {
                    let index_module_identifier_end = variable_name.find("]]::");
                    let Some(index_module_identifier_end) = index_module_identifier_end else {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidAstNode,
                            Some("Invalid variable name"),
                            node.pos(),
                        ));
                    };

                    let module_name = variable_name[2..index_module_identifier_end].to_string();
                    if !Self::is_alpha_numeric_with_underline(&module_name) {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidAstNode,
                            Some("Invalid module name"),
                            node.pos(),
                        ));
                    }

                    variable_name = variable_name[index_module_identifier_end + 4..].to_string();

                    Some(module_name)
                }else {
                    None
                };

                if Self::is_var_name_full_without_prefix(&variable_name) {
                    let mut flags = [false, false];
                    let lvalue = self.get_or_create_data_object_from_variable_name(
                        None,
                        module_name.as_deref(),
                        &variable_name,
                        false,
                        true,
                        true,
                        &mut flags,
                        node.pos(),
                    );

                    if flags[0] {
                        //Forward error from getOrCreateDataObjectFromVariableName()
                        return lvalue;
                    }

                    let lvalue = lvalue.unwrap();

                    let is_same_reference = ptr::eq(lvalue.deref(), rvalue.deref());

                    let mut lvalue = lvalue.borrow_mut();

                    let variable_name = lvalue.variable_name();
                    let Some(variable_name) = variable_name else {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidAssignment,
                            Some("Anonymous values can not be changed"),
                            node.pos(),
                        ));
                    };
                    let variable_name = Rc::from(variable_name);

                    if lvalue.is_final_data() || lvalue.is_lang_var() {
                        if flags[1] {
                            self.data_mut().var.remove(&variable_name);
                        }

                        return Some(self.set_errno_error_object(
                            InterpretingError::FinalVarChange,
                            None,
                            node.pos(),
                        ));
                    }

                    //Do not set data if lvalue and rvalue are the same reference
                    if !is_same_reference {
                        let ret = lvalue.set_data(&rvalue.borrow());
                        if ret.is_err() {
                            if flags[1] {
                                self.data_mut().var.remove(&variable_name);
                            }

                            return Some(self.set_errno_error_object(
                                InterpretingError::IncompatibleDataType,
                                Some("Incompatible type for rvalue in assignment"),
                                node.pos(),
                            ));
                        }
                    }

                    if let Some(func_name) = variable_name.strip_prefix("fp.") {
                        if self.funcs.iter().any(|(key, _)| *func_name == **key) {
                            self.set_errno(
                                InterpretingError::VarShadowingWarning,
                                Some(&format!("\"{variable_name}\" shadows a predefined or linker function")),
                                node.pos(),
                            );
                        }
                    }

                    return Some(rvalue);
                }

                //Continue in "Lang translation" below if variableName is not valid
            },

            NodeData::ArgumentSeparator(..) => {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Neither lvalue nor translationKey"),
                    node.pos(),
                ));
            },

            _ => {},
        };

        //Lang translation
        let translation_key = self.interpret_node(None, lvalue_node);
        let Some(translation_key) = translation_key else {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Invalid translationKey"),
                node.pos(),
            ));
        };

        let translation_key = conversions::to_text(self, &translation_key, node.pos());
        if translation_key.starts_with("lang.") {
            self.interpret_lang_data_and_execution_flags(&translation_key, &rvalue, node.pos());
        }

        let translation_value = conversions::to_text(self, &rvalue, node.pos());

        self.data_mut().lang.insert(Rc::from(translation_key), Rc::from(translation_value));

        Some(rvalue)
    }

    /**
     * Will create a variable if it doesn't exist or returns an error object, or returns None if shouldCreateDataObject is set to false and variable doesn't exist
     * @param supportsPointerReferencing If true, this node will return pointer reference as DataObject<br>
     *                                   (e.g. $[abc] is not in variableNames, but $abc is -> $[abc] will return a DataObject)
     * @param flags Will set by this method in format: [error, created]
     */
    #[allow(clippy::too_many_arguments)]
    fn get_or_create_data_object_from_variable_name(
        &mut self,
        composite_type: OptionDataObjectRef,
        module_name: Option<&str>,
        variable_name: &str,
        supports_pointer_referencing: bool,
        supports_pointer_dereferencing: bool,
        should_create_data_object: bool,
        flags: &mut [bool; 2],
        pos: CodePosition,
    ) -> OptionDataObjectRef {
        let variables = if let Some(composite_type) = &composite_type {
            match composite_type.data_type() {
                DataType::ERROR => {
                    let error_value = composite_type.error_value().unwrap();

                    HashMap::from([
                        (
                            Rc::from("$text"),
                            DataObjectRef::new(DataObject::with_update_final(|data_object| {
                                data_object.
                                        set_text(error_value.err().error_text())?.
                                        set_variable_name(Some("$text"))?.
                                        set_type_constraint(Box::new(DataTypeConstraint::from_single_allowed_type(
                                            DataType::TEXT,
                                        )))
                            }).unwrap()),
                        ),
                        (
                            Rc::from("$code"),
                            DataObjectRef::new(DataObject::with_update_final(|data_object| {
                                data_object.
                                        set_int(error_value.err().error_code())?.
                                        set_variable_name(Some("$code"))?.
                                        set_type_constraint(Box::new(DataTypeConstraint::from_single_allowed_type(
                                            DataType::INT,
                                        )))
                            }).unwrap()),
                        ),
                        (
                            Rc::from("$message"),
                            DataObjectRef::new(DataObject::with_update_final(|data_object| {
                                if let Some(message) = error_value.message() {
                                    data_object.set_text(message)?;
                                }

                                data_object.
                                        set_variable_name(Some("$message"))?.
                                        set_type_constraint(Box::new(DataTypeConstraint::from_allowed_types(&[
                                            DataType::NULL, DataType::TEXT,
                                        ])))
                            }).unwrap()),
                        ),
                    ])
                },

                DataType::STRUCT => {
                    let mut variables = HashMap::new();

                    let struct_value = composite_type.struct_value().unwrap();

                    for member_name in struct_value.member_names() {
                        let member = struct_value.get_member(&member_name);

                        let member = match member {
                            Ok(member) => member,

                            Err(e) => {
                                flags[0] = true;

                                return Some(self.set_errno_error_object(
                                    InterpretingError::IncompatibleDataType,
                                    Some(e.message()),
                                    pos,
                                ));
                            },
                        };

                        variables.insert(Rc::from(member_name), member);
                    }

                    variables
                },

                DataType::OBJECT => {
                    let mut variables = HashMap::new();

                    let object_value = composite_type.object_value().unwrap();

                    let variable_name_prefix = &variable_name[..3];

                    if matches!(variable_name_prefix, "mp." | "op:" | "to:") {
                        for (function_name, functions) in object_value.borrow().methods().iter().
                                filter(|(key, _)| key.starts_with(variable_name_prefix)) {
                            variables.insert(Rc::from(&**function_name), DataObjectRef::new(DataObject::with_update_final(|data_object| {
                                data_object.
                                        set_function_pointer(Rc::new(
                                            functions.copy_with_function_name(function_name),
                                        ))?.
                                        set_variable_name(Some(function_name))
                            }).unwrap()));
                        }
                    }else {
                        for static_member in object_value.borrow().static_members() {
                            let variable_name = static_member.variable_name().unwrap();

                            variables.insert(Rc::from(variable_name), static_member.clone());
                        }

                        if let Some(members) = object_value.borrow().members() {
                            //If a static member and a member have the same variable name, the static member will be shadowed
                            for member in members {
                                let variable_name = member.variable_name().unwrap();

                                variables.insert(Rc::from(variable_name), member.clone());
                            }
                        }
                    }

                    variables
                },

                _ => {
                    flags[0] = true;

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Invalid composite type"),
                        pos,
                    ));
                },
            }
        }else if let Some(module_name) = module_name {
            let module = self.modules.get(module_name);

            let Some(module) = module else {
                flags[0] = true;

                return Some(self.set_errno_error_object(
                    InterpretingError::ModuleLoadUnloadErr,
                    Some(&format!("The module \"{module_name}\" is not loaded!")),
                    pos,
                ));
            };

            module.exported_variables().clone()
        }else {
            self.data_ref().var.clone()
        };

        if let Some(ret) = variables.get(variable_name) {
            if !ret.is_accessible(self.current_call_stack_element.lang_class()) {
                flags[0] = true;

                return Some(self.set_errno_error_object(
                    InterpretingError::MemberNotAccessible,
                    Some(&format!("For member \"{variable_name}\"")),
                    pos,
                ));
            }

            return Some(ret.clone());
        }

        if supports_pointer_dereferencing {
            if let Some(index) = variable_name.find("*") {
                let referenced_variable_name = variable_name[..index].to_string() +
                        &variable_name[index + 1..];
                let referenced_variable = self.get_or_create_data_object_from_variable_name(
                    composite_type,
                    module_name,
                    &referenced_variable_name,
                    supports_pointer_referencing,
                    true,
                    false,
                    flags,
                    pos,
                );

                let Some(referenced_variable) = referenced_variable else {
                    flags[0] = true;

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidPtr,
                        None,
                        pos,
                    ));
                };

                if let Some(var_pointer_value) = referenced_variable.var_pointer_value() {
                    return Some(var_pointer_value);
                };

                //If no var pointer was dereferenced, return null data object
                return Some(DataObjectRef::new(DataObject::new()));
            }
        }

        if supports_pointer_referencing && variable_name.contains("]") {
            if let Some(index_opening_bracket) = variable_name.find("[") {
                let index_matching_bracket = utils::get_index_of_matching_bracket_str(
                    variable_name,
                    index_opening_bracket,
                    usize::MAX, b'[', b']',
                );

                //0 can be used as default, because it will still go into the error if body
                let index_matching_bracket = index_matching_bracket.unwrap_or(0);
                if index_matching_bracket != variable_name.len() - 1 {
                    flags[0] = true;

                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some("Non matching referencing brackets"),
                        pos,
                    ));
                }

                let dereferenced_variable_name = variable_name[..index_opening_bracket].to_string() +
                        &variable_name[index_opening_bracket + 1..index_matching_bracket];
                let dereferenced_variable = self.get_or_create_data_object_from_variable_name(
                    composite_type,
                    module_name,
                    &dereferenced_variable_name,
                    true,
                    false,
                    false,
                    flags,
                    pos,
                );

                if let Some(dereferenced_variable) = dereferenced_variable {
                    return Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_var_pointer(dereferenced_variable)
                    }).unwrap()));
                };

                return if should_create_data_object {
                    flags[0] = true;

                    Some(self.set_errno_error_object(
                        InterpretingError::InvalidPtr,
                        Some("Pointer redirection is not supported"),
                        pos,
                    ))
                }else {
                    None
                };
            }
        }

        if !should_create_data_object {
            return None;
        }

        //Variable creation if possible
        if composite_type.is_some() || module_name.is_some() ||
                Self::is_lang_var_or_lang_var_pointer_redirection_without_prefix(variable_name) {
            flags[0] = true;

            return Some(self.set_errno_error_object(
                InterpretingError::FinalVarChange,
                if composite_type.is_some() {
                    Some("Composite type members can not be created")
                }else if module_name.is_some() {
                    Some("Module variables can not be created")
                }else {
                    None
                },
                pos,
            ));
        }

        flags[1] = true;

        let data_object = DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_variable_name(Some(variable_name))
        }).unwrap());

        self.data_mut().var.insert(Rc::from(variable_name), data_object.clone());

        Some(data_object)
    }
    /**
     * Will create a variable if it doesn't exist or returns an error object
     */
    fn interpret_variable_name_node(&mut self, composite_type: OptionDataObjectRef, node: &Node) -> OptionDataObjectRef {
        let NodeData::VariableName {
            variable_name, ..
        } = node.node_data() else {
            panic!("Invalid AST node");
        };

        let variable_name_orig = variable_name;
        let mut variable_name = variable_name_orig.to_string();

        let is_module_variable = composite_type.is_none() && variable_name.starts_with("[[");
        let module_name = if is_module_variable {
            let index_module_identifier_end = variable_name.find("]]::");
            let Some(index_module_identifier_end) = index_module_identifier_end else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid variable name"),
                    node.pos(),
                ));
            };

            let module_name = variable_name[2..index_module_identifier_end].to_string();
            if !Self::is_alpha_numeric_with_underline(&module_name) {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid module name"),
                    node.pos(),
                ));
            }

            variable_name = variable_name[index_module_identifier_end + 4..].to_string();

            Some(module_name)
        }else {
            None
        };

        if !Self::is_var_name_full_with_funcs_without_prefix(&variable_name) &&
                !Self::is_var_name_ptr_and_dereference_without_prefix(&variable_name) &&
                !Self::is_operator_method_name(&variable_name) &&
                !Self::is_conversion_method_name(&variable_name) {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Invalid variable name"),
                node.pos(),
            ));
        }

        if let Some(composite_type) = &composite_type {
            if let Some(object_value) = composite_type.object_value() {
                if (variable_name.starts_with("mp.") || variable_name.starts_with("op:") || variable_name.starts_with("to:")) &&
                        !object_value.borrow().is_class() {
                    return self.get_or_create_data_object_from_variable_name(
                        Some(composite_type.clone()),
                        module_name.as_deref(),
                        &variable_name,
                        false,
                        false,
                        false,
                        &mut [false, false],
                        node.pos(),
                    );
                }
            }
        }

        if variable_name.starts_with("$") || variable_name.starts_with("&") || variable_name.starts_with("fp.") {
            return self.get_or_create_data_object_from_variable_name(
                composite_type.clone(),
                module_name.as_deref(),
                &variable_name,
                variable_name.starts_with("$"),
                variable_name.starts_with("$"),
                true,
                &mut [false, false],
                node.pos(),
            );
        }

        if composite_type.is_some() {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some(&format!(
                    "Invalid composite type member name: \"{variable_name}\""
                )),
                node.pos(),
            ));
        }

        let is_linker_function;
        if !is_module_variable && variable_name.starts_with("func.") {
            is_linker_function = false;

            variable_name = variable_name[5..].to_string();
        }else if !is_module_variable && variable_name.starts_with("fn.") {
            is_linker_function = false;

            variable_name = variable_name[3..].to_string();
        }else if !is_module_variable && variable_name.starts_with("linker.") {
            is_linker_function = true;

            variable_name = variable_name[7..].to_string();
        }else if !is_module_variable && variable_name.starts_with("ln.") {
            is_linker_function = true;

            variable_name = variable_name[3..].to_string();
        }else {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Invalid variable name"),
                node.pos(),
            ));
        }

        let ret = self.funcs.iter().find(|(func_name, func)| {
            func.linker_function() == is_linker_function && *variable_name == ***func_name
        });

        let Some(ret) = ret else {
            return Some(self.set_errno_error_object(
                InterpretingError::FunctionNotFound,
                Some(&format!("\"{variable_name}\" was not found")),
                node.pos(),
            ));
        };

        let func = ret.1;

        Some(DataObjectRef::new(DataObject::with_update_final(|data_object| {
            data_object.
                    set_function_pointer(Rc::new(
                        func.copy_with_function_name(variable_name_orig),
                    ))?.
                    set_variable_name(Some(variable_name_orig))
        }).unwrap()))
    }

    /**
     * @return Will return None for ("\!" escape sequence)
     */
    fn interpret_escape_sequence_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let NodeData::EscapeSequence(char) = node.node_data() else {
            panic!("Invalid AST node");
        };

        match char {
            '0' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\0')
            }).unwrap())),
            'n' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\n')
            }).unwrap())),
            'r' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\r')
            }).unwrap())),
            'f' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\u{000C}')
            }).unwrap())),
            's' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(' ')
            }).unwrap())),
            'e' => Some(DataObjectRef::new(DataObject::new_text(""))),
            'E' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\u{001B}')
            }).unwrap())),
            'b' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\u{0008}')
            }).unwrap())),
            't' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char('\t')
            }).unwrap())),

            '$' | '&' | '#' | ',' | '.' | '(' | ')' | '[' | ']' | '{' | '}' | '=' | '<' | '>' |
            '+' | '-' | '/' | '*' | '%' | '|' | '~' | '^' | '?' | ':' | '@' | '\u{25b2}' |
            '\u{25bc}' | '\"' | '\\' => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(*char)
            }).unwrap())),

            '!' => None,

            //If no escape sequence: Remove "\" anyway
            _ => {
                self.set_errno(InterpretingError::UndefEscapeSequence, Some(&format!("\"\\{char}\" was used")), node.pos());

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_char(*char)
                }).unwrap()))
            },
        }
    }

    fn interpret_unicode_escape_sequence_node(&mut self, node: &Node) -> DataObjectRef {
        let NodeData::UnicodeEscapeSequence(hex_code_point) = node.node_data() else {
            panic!("Invalid AST node");
        };

        for c in hex_code_point.bytes() {
            if !c.is_ascii_alphanumeric() {
                return self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some(&format!(
                        "Invalid unicode escape sequence: \"\\u{{{hex_code_point}}}\"",
                    )),
                    node.pos(),
                );
            }
        }

        let code_point = u32::from_str_radix(&hex_code_point.to_ascii_uppercase(), 16);
        let char = code_point.ok().and_then(char::from_u32).unwrap_or('\u{FFFD}');

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_char(char)
        }).unwrap())
    }

    fn interpret_argument_separator_node(&mut self, node: &Node) -> DataObjectRef {
        let NodeData::ArgumentSeparator(original_text) = node.node_data() else {
            panic!("Invalid AST node");
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_argument_separator(original_text)
        }).unwrap())
    }

    fn get_and_reset_return_value(&mut self) -> OptionDataObjectRef {
        let ret_tmp = self.execution_state.returned_or_thrown_value.take();

        if self.execution_flags.lang_test && self.scope_id == self.lang_test_expected_return_value_scope_id {
            if let Some(lang_test_expected_throw_value) = self.lang_test_expected_throw_value.take() {
                let error = ret_tmp.as_ref().
                        and_then(|ret_tmp| self.execution_state.is_thrown_value.then(|| {
                            ret_tmp.error_value().map(|error_value| error_value.err())
                        })).flatten();

                self.lang_test_store.add_assert_result(AssertResult::new_throw_result(
                    error.is_some_and(|error| error == lang_test_expected_throw_value),
                    Some(&self.print_stack_trace(CodePosition::EMPTY)),
                    self.lang_test_message_for_last_test_result.take().as_deref(),
                    error,
                    lang_test_expected_throw_value,
                ));
            }

            if let Some(lang_test_expected_return_value) = self.lang_test_expected_return_value.take() {
                let assert_result = AssertResult::new_return_result(
                    !self.execution_state.is_thrown_value && ret_tmp.as_ref().is_some_and(
                        |ret_tmp| operators::is_strict_equals(
                            self,
                            &lang_test_expected_return_value,
                            ret_tmp,
                            CodePosition::EMPTY,
                        ),
                    ),
                    Some(&self.print_stack_trace(CodePosition::EMPTY)),
                    self.lang_test_message_for_last_test_result.take().as_deref(),
                    ret_tmp.as_ref().map(|ret_tmp| {
                        (ret_tmp, conversions::to_text(self, ret_tmp, CodePosition::EMPTY))
                    }),
                    lang_test_expected_return_value.borrow().deref(),
                    &conversions::to_text(self, &lang_test_expected_return_value, CodePosition::EMPTY),
                );

                self.lang_test_store.add_assert_result(assert_result);
            }

            if self.lang_test_expected_no_return_value {
                let assert_result = AssertResult::new_no_return_result(
                    ret_tmp.is_none(),
                    Some(&self.print_stack_trace(CodePosition::EMPTY)),
                    self.lang_test_message_for_last_test_result.take().as_deref(),
                    ret_tmp.as_ref().map(|ret_tmp| {
                        (ret_tmp, conversions::to_text(self, ret_tmp, CodePosition::EMPTY))
                    }),
                );

                self.lang_test_store.add_assert_result(assert_result);

                self.lang_test_expected_no_return_value = false;
            }

            self.lang_test_message_for_last_test_result = None;
            self.lang_test_expected_return_value_scope_id = 0;
        }

        self.execution_state.is_thrown_value = false;

        if self.execution_state.try_thrown_error.is_none() || self.execution_state.try_block_level == 0 ||
                (self.execution_state.is_soft_try && self.execution_state.try_body_scope_id != self.scope_id) {
            self.execution_state.stop_execution_flag = false;
        }

        ret_tmp.map(|ret_tmp| {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&ret_tmp.borrow())
            }).unwrap())
        })
    }
    fn is_thrown_value(&self) -> bool {
        self.execution_state.is_thrown_value || (self.execution_state.try_thrown_error.is_some() &&
                self.execution_state.try_block_level > 0 && (!self.execution_state.is_soft_try ||
                self.execution_state.try_body_scope_id == self.scope_id))
    }

    #[allow(clippy::too_many_arguments)]
    #[inline(always)]
    fn call_function_pointer_internal(
        &mut self,
        fp: &FunctionPointerObject,
        this_object: Option<&LangObjectRef>,
        function_name: &str,
        internal_function: &InternalFunction,
        argument_list: &[DataObjectRef],
        mut combined_argument_list: Vec<DataObjectRef>,
        parent_pos: CodePosition,
    ) -> OptionDataObjectRef {
        let function = internal_function.function();
        match function.function() {
            FunctionData::Normal(normal_function) => {
                let parameter_list = function.parameter_list();
                let argument_pos_list = normal_function.argument_pos_list();
                let arg_count = parameter_list.len();

                if function.combinator_function_call_count().is_some() {
                    let combined_argument_list_orig = combined_argument_list;

                    let combinator_function_provided_arguments = function.combinator_function_provided_arguments();

                    combined_argument_list = Vec::with_capacity(combined_argument_list_orig.len() + combinator_function_provided_arguments.len());
                    combined_argument_list.append(&mut Vec::from(combinator_function_provided_arguments));

                    combined_argument_list_orig.iter().for_each(|argument| {
                        combined_argument_list.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_data(&argument.borrow())
                        }).unwrap()));
                    });
                }

                let function_body = normal_function.function_body();

                if function.var_args_parameter().is_none() {
                    if function.combinator_function_call_count().is_none() && combined_argument_list.len() < arg_count {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidArgCount,
                            Some(&format!("Not enough arguments ({arg_count} needed)")),
                            parent_pos,
                        ));
                    }

                    if combined_argument_list.len() > arg_count {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidArgCount,
                            Some(&format!("Too many arguments ({arg_count} needed)")),
                            parent_pos,
                        ));
                    }
                }else {
                    //Infinite combinator functions (= Combinator functions with var args argument) must be called exactly two times
                    if function.combinator_function_call_count().is_none_or(|call_count| call_count > 0) && combined_argument_list.len() < arg_count - 1 {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidArgCount,
                            Some(&format!("Not enough arguments (at least {} needed)", arg_count - 1)),
                            parent_pos,
                        ));
                    }
                }

                let mut combinator_function_call_ret = None;
                let ret = 'early_ret: {
                    let caller_data = self.data_ref().var.clone();

                    self.enter_scope(None);

                    //Copies must not be final
                    for (key, val) in caller_data {
                        if val.is_static_data() {
                            //Static Lang vars should also be copied
                            self.data_mut().var.insert(key, val);
                        }else if !val.is_lang_var() {
                            //Copy non static and non lang vars
                            self.data_mut().var.insert(key, DataObjectRef::new(DataObject::with_update(|data_object| {
                                data_object.set_data(&val.borrow())?.
                                        set_variable_name(val.borrow().variable_name())
                            }).unwrap()));
                        }
                    }

                    //Set this-object and This-class
                    if let Some(this_object) = this_object {
                        let old = self.data_mut().var.insert(Rc::from("&this"), DataObjectRef::new(DataObject::with_update_final(|data_object| {
                            data_object.set_object(this_object.clone())?.
                                    set_variable_name(Some("&this"))
                        }).unwrap()));
                        if old.is_some_and(|old| old.is_static_data()) {
                            self.set_errno(
                                InterpretingError::VarShadowingWarning,
                                Some("This-object \"&this\" shadows a static variable"),
                                function_body.get_pos(),
                            );
                        }

                        let old = self.data_mut().var.insert(Rc::from("&This"), DataObjectRef::new(DataObject::with_update_final(|data_object| {
                            data_object.set_object(this_object.borrow().base_definition().unwrap())?.
                                    set_variable_name(Some("&This"))
                        }).unwrap()));
                        if old.is_some_and(|old| old.is_static_data()) {
                            self.set_errno(
                                InterpretingError::VarShadowingWarning,
                                Some("This-class \"&This\" shadows a static variable"),
                                function_body.get_pos(),
                            );
                        }
                    }

                    //Set arguments
                    let mut argument_index = 0;
                    for i in 0..arg_count {
                        if let Some(call_count) = function.combinator_function_call_count() {
                            if argument_index >= combined_argument_list.len() &&
                                    (function.var_args_parameter().is_none() || call_count == 0) {
                                combinator_function_call_ret = Some(function.combinator_call(
                                    this_object.map(|this_object| (this_object.clone(), internal_function.super_level().unwrap())),
                                    combined_argument_list,
                                ));

                                break;
                            }
                        }

                        let parameter = &parameter_list[i];
                        let argument_pos = argument_pos_list[i];

                        match parameter.parameter_type() {
                            ParameterType::VarArgs => {
                                //Infinite combinator functions (= Combinator functions with var args argument) must be called exactly two times
                                if function.combinator_function_call_count().is_some_and(|call_count| call_count == 0) {
                                    combinator_function_call_ret = Some(function.combinator_call(
                                        this_object.map(|this_object| (this_object.clone(), internal_function.super_level().unwrap())),
                                        combined_argument_list,
                                    ));

                                    break;
                                }

                                if parameter.parameter_name().starts_with("$") {
                                    //Text varargs
                                    if *parameter.type_constraint() != *data::CONSTRAINT_NORMAL {
                                        break 'early_ret Some(Some(self.set_errno_error_object(
                                            InterpretingError::InvalidAstNode,
                                            Some(&format!(
                                                "function parameter \"{}\": Text var args argument must not have a type constraint definition",
                                                parameter.parameter_name(),
                                            )),
                                            argument_pos,
                                        )));
                                    }

                                    let mut argument_list_copy = VecDeque::from_iter(argument_list.iter().cloned());

                                    //Remove leading arguments
                                    for _ in 0..i {
                                        for _ in 0..argument_list_copy.len() {
                                            if argument_list_copy.pop_front().
                                                    is_some_and(|val| val.
                                                            data_type() == DataType::ARGUMENT_SEPARATOR) {
                                                break;
                                            }
                                        }
                                    }

                                    //Remove trailing arguments
                                    for _ in 0..arg_count - i - 1 {
                                        for k in argument_list_copy.len()..=0 {
                                            if argument_list_copy.remove(k).
                                                    is_some_and(|val| val.
                                                            data_type() == DataType::ARGUMENT_SEPARATOR) {
                                                break;
                                            }
                                        }
                                    }

                                    let data_object = utils::combine_data_objects(
                                        argument_list_copy.make_contiguous(),
                                        self,
                                        CodePosition::EMPTY,
                                    );

                                    let text = data_object.map(|arg| conversions::to_text(
                                        self,
                                        &arg,
                                        CodePosition::EMPTY,
                                    )).unwrap_or_default();

                                    let new_data_object = DataObject::with_update(|data_object| {
                                        data_object.set_text(text)?.
                                                set_variable_name(Some(parameter.parameter_name()))
                                    });
                                    let new_data_object = match new_data_object {
                                        Ok(new_data_object) => new_data_object,
                                        Err(e) => {
                                            break 'early_ret Some(Some(self.set_errno_error_object(
                                                InterpretingError::InvalidAstNode,
                                                Some(&format!(
                                                    "Invalid argument value for function parameter \"{}\" ({})",
                                                    parameter.parameter_name(),
                                                    e.message(),
                                                )),
                                                argument_pos,
                                            )));
                                        },
                                    };

                                    let old = self.data_mut().var.insert(
                                        Rc::from(parameter.parameter_name()),
                                        DataObjectRef::new(new_data_object),
                                    );
                                    if old.is_some_and(|old| old.is_static_data()) {
                                        self.set_errno(
                                            InterpretingError::VarShadowingWarning,
                                            Some(&format!(
                                                "Parameter \"{}\" shadows a static variable",
                                                parameter.parameter_name(),
                                            )),
                                            function_body.get_pos(),
                                        );
                                    }
                                }else {
                                    //Array varargs
                                    let var_args_argument_list = combined_argument_list.iter().
                                            skip(i).
                                            take(combined_argument_list.len() + i + 1 - arg_count).
                                            map(|data_object| DataObjectRef::new(DataObject::with_update(|new_data_object|
                                                    new_data_object.set_data(&data_object.borrow())
                                            ).unwrap())).collect::<Vec<_>>();

                                    for (j, argument) in var_args_argument_list.iter().
                                            enumerate() {
                                        if !parameter.type_constraint().is_type_allowed(argument.data_type()) {
                                            break 'early_ret Some(Some(self.set_errno_error_object(
                                                InterpretingError::IncompatibleDataType,
                                                Some(&format!(
                                                    "Invalid argument (Argument {}) value for var args function parameter \"{}\": Value must be one of {:?}",
                                                    argument_index + j + 1,
                                                    parameter.parameter_name(),
                                                    parameter.type_constraint().allowed_types()
                                                )),
                                                argument_pos,
                                            )));
                                        }
                                    }

                                    let new_data_object = DataObject::with_update(|data_object| {
                                        data_object.set_array(var_args_argument_list.into_boxed_slice())?.
                                                set_variable_name(Some(parameter.parameter_name()))
                                    });
                                    let new_data_object = match new_data_object {
                                        Ok(new_data_object) => new_data_object,
                                        Err(e) => {
                                            break 'early_ret Some(Some(self.set_errno_error_object(
                                                InterpretingError::InvalidAstNode,
                                                Some(&format!(
                                                    "Invalid argument value for function parameter \"{}\" ({})",
                                                    parameter.parameter_name(),
                                                    e.message(),
                                                )),
                                                argument_pos,
                                            )));
                                        },
                                    };

                                    let old = self.data_mut().var.insert(
                                        Rc::from(parameter.parameter_name()),
                                        DataObjectRef::new(new_data_object),
                                    );
                                    if old.is_some_and(|old| old.is_static_data()) {
                                        self.set_errno(
                                            InterpretingError::VarShadowingWarning,
                                            Some(&format!(
                                                "Parameter \"{}\" shadows a static variable",
                                                parameter.parameter_name(),
                                            )),
                                            function_body.get_pos(),
                                        );
                                    }
                                }

                                argument_index = combined_argument_list.len() + i + 1 - arg_count;
                                continue;
                            },

                            ParameterType::CallByPointer => {
                                let new_data_object = DataObject::with_update(|data_object| {
                                    data_object.set_var_pointer(combined_argument_list[argument_index].clone())?.
                                            set_variable_name(Some(parameter.parameter_name()))?.
                                            set_type_constraint(Box::new(parameter.type_constraint().clone()))
                                });
                                let new_data_object = match new_data_object {
                                    Ok(new_data_object) => new_data_object,
                                    Err(e) => {
                                        break 'early_ret Some(Some(self.set_errno_error_object(
                                            InterpretingError::InvalidAstNode,
                                            Some(&format!(
                                                "Invalid argument value for function parameter \"{}\" ({})",
                                                parameter.parameter_name(),
                                                e.message(),
                                            )),
                                            argument_pos,
                                        )));
                                    },
                                };

                                let old = self.data_mut().var.insert(
                                    Rc::from(parameter.parameter_name()),
                                    DataObjectRef::new(new_data_object),
                                );
                                if old.is_some_and(|old| old.is_static_data()) {
                                    self.set_errno(
                                        InterpretingError::VarShadowingWarning,
                                        Some(&format!(
                                            "Parameter \"{}\" shadows a static variable",
                                            parameter.parameter_name(),
                                        )),
                                        function_body.get_pos(),
                                    );
                                }

                                argument_index += 1;
                                continue;
                            },

                            _ => {},
                        }

                        let mut value = combined_argument_list[argument_index].clone();
                        match parameter.parameter_type() {
                            ParameterType::Boolean => {
                                value = DataObjectRef::new(DataObject::with_update(|data_object| {
                                    data_object.set_bool(conversions::to_bool(
                                        self,
                                        &value,
                                        argument_pos,
                                    ))
                                }).unwrap());
                            },

                            ParameterType::Number => {
                                let number = conversions::to_number(
                                    self,
                                    &value,
                                    argument_pos,
                                );
                                let Some(number) = number else {
                                    break 'early_ret Some(Some(self.set_errno_error_object(
                                        InterpretingError::IncompatibleDataType,
                                        Some(&format!(
                                            "Invalid argument value for function parameter \"{}\" (Must be a number)",
                                            parameter.parameter_name(),
                                        )),
                                        argument_pos,
                                    )));
                                };

                                value = DataObjectRef::new(DataObject::with_update(|data_object| {
                                    data_object.set_number(number)
                                }).unwrap());
                            },

                            ParameterType::Callable => {
                                if !utils::is_callable(&value) {
                                    break 'early_ret Some(Some(self.set_errno_error_object(
                                        InterpretingError::IncompatibleDataType,
                                        Some(&format!(
                                            "Invalid argument value for function parameter \"{}\" (Must be callable)",
                                            parameter.parameter_name(),
                                        )),
                                        argument_pos,
                                    )));
                                };
                            },

                            _ => {},
                        }

                        let new_data_object = DataObject::with_update(|data_object| {
                            data_object.set_data(&value.borrow())?.
                                    set_variable_name(Some(parameter.parameter_name()))?.
                                    set_type_constraint(Box::new(parameter.type_constraint().clone()))
                        });
                        let new_data_object = match new_data_object {
                            Ok(new_data_object) => new_data_object,
                            Err(e) => {
                                break 'early_ret Some(Some(self.set_errno_error_object(
                                    InterpretingError::InvalidAstNode,
                                    Some(&format!(
                                        "Invalid argument value for function parameter \"{}\" ({})",
                                        parameter.parameter_name(),
                                        e.message(),
                                    )),
                                    argument_pos,
                                )));
                            },
                        };

                        let old = self.data_mut().var.insert(
                            Rc::from(parameter.parameter_name()),
                            DataObjectRef::new(new_data_object),
                        );
                        if old.is_some_and(|old| old.is_static_data()) {
                            self.set_errno(
                                InterpretingError::VarShadowingWarning,
                                Some(&format!(
                                    "Parameter \"{}\" shadows a static variable",
                                    parameter.parameter_name(),
                                )),
                                function_body.get_pos(),
                            );
                        }

                        argument_index += 1;
                    }

                    if combinator_function_call_ret.is_none() {
                        //Call function
                        self.interpret_ast(function_body);
                    }

                    None
                };
                //Finally (Execute even if early return)
                {
                    let scope_data = self.data_ref().lang.clone();

                    self.exit_scope();

                    //Add translations after call
                    self.data_mut().lang.extend(scope_data);
                }
                //Return if early return
                if let Some(ret) = ret {
                    return ret;
                }

                if let Some(deprecation_info) = fp.deprecated() {
                    let message = format!(
                        "Use of deprecated function \"{}\". This function will no longer be supported in \"{}\"!{}",
                        function_name,
                        deprecation_info.remove_version().unwrap_or("the_future"),
                        if let Some(deprecated_replacement_function) = deprecation_info.replacement_function() {
                            format!("\nUse \"{deprecated_replacement_function}\" instead!")
                        }else {
                            String::new()
                        }
                    );

                    self.set_errno(InterpretingError::DeprecatedFuncCall, Some(&message), parent_pos);
                }

                let (
                    return_value_type_constraint,
                    return_or_throw_statement_pos,
                ) = if combinator_function_call_ret.is_none() {
                    (function.return_value_type_constraint(), self.execution_state.return_or_throw_statement_pos)
                }else {
                    (None, CodePosition::EMPTY)
                };

                let thrown_value = self.is_thrown_value();
                let ret_tmp = combinator_function_call_ret.
                        unwrap_or_else(|| utils::none_to_lang_void(self.get_and_reset_return_value()));

                if !thrown_value {
                    if let Some(return_value_type_constraint) = return_value_type_constraint {
                        //Thrown values are always allowed
                        if !return_value_type_constraint.is_type_allowed(ret_tmp.data_type()) {
                            return Some(self.set_errno_error_object(
                                InterpretingError::IncompatibleDataType,
                                Some(&format!(
                                    "Invalid return value type \"{:?}\"",
                                    ret_tmp.data_type(),
                                )),
                                return_or_throw_statement_pos,
                            ));
                        }
                    }
                }

                Some(ret_tmp)
            },

            FunctionData::Native(_) => {
                let ret = function.call_native_func(
                    self,
                    this_object.map(|this_object| (this_object.clone(), internal_function.super_level().unwrap())),
                    Vec::from(argument_list),
                    combined_argument_list,
                );

                if let Some(deprecation_info) = fp.deprecated() {
                    let message = format!(
                        "Use of deprecated function \"{}\". This function will no longer be supported in \"{}\"!{}",
                        function_name,
                        deprecation_info.remove_version().unwrap_or("the_future"),
                        if let Some(deprecated_replacement_function) = deprecation_info.replacement_function() {
                            format!("\nUse \"{deprecated_replacement_function}\" instead!")
                        }else {
                            String::new()
                        }
                    );

                    self.set_errno(InterpretingError::DeprecatedFuncCall, Some(&message), parent_pos);
                }

                //Return non copy if copyStaticAndFinalModifiers flag is set for "func.asStatic()" and "func.asFinal()"
                if let Some(ret) = ret {
                    if ret.is_copy_static_and_final_modifiers() {
                        Some(ret)
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_data(&ret.borrow())
                        }).unwrap()))
                    }
                }else {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_void()
                    }).unwrap()))
                }
            },
        }
    }

    fn call_function_pointer(
        &mut self,
        fp: &FunctionPointerObject,
        function_name: Option<&str>,
        argument_list: &[DataObjectRef],
        parent_pos: CodePosition,
    ) -> OptionDataObjectRef {
        let this_object = fp.this_object();
        let mut original_super_level = None;

        let combined_argument_list = utils::combine_arguments_without_argument_separators(
            argument_list, self, parent_pos,
        );

        let internal_function = if fp.get_overloaded_function_count() == 1 {
            fp.get_function(0)
        }else {
            utils::get_most_restrictive_function(fp, &combined_argument_list)
        };

        let Some(internal_function) = internal_function else {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!(
                    "No matching function signature was found for the given arguments. \
                    Available function signatures:\n    {}{}",
                    function_name.unwrap_or("null"),
                    fp.functions().iter().
                            map(|function| function.to_function_signature_string()).
                            collect::<Vec<_>>().
                            join(&("\n    ".to_string() + function_name.unwrap_or("null"))),
                )),
                CodePosition::EMPTY,
            ));
        };

        if !internal_function.is_accessible(self.current_call_stack_element().lang_class()) {
            return Some(self.set_errno_error_object(
                InterpretingError::MemberNotAccessible,
                Some(&format!("For member \"{}\"", function_name.unwrap_or("null"))),
                CodePosition::EMPTY,
            ));
        }

        if let Some(this_object) = this_object {
            let mut this_object = this_object.borrow_mut();

            if !this_object.is_class() {
                original_super_level = this_object.super_level();
                this_object.set_super_level(internal_function.super_level().unwrap()).unwrap();
            }
        }

        let member_of_class = internal_function.member_of_class();

        let function_lang_path = internal_function.lang_path();
        let function_lang_file = internal_function.lang_file();

        //TODO improve when if let chains become stable
        let function_name = if let Some(function_name) = function_name {
            if fp.function_name().is_some() {
                fp.to_string()
            }else {
                function_name.to_string()
            }
        }else {
            fp.to_string()
        };

        //Update call stack
        let current_stack_element = self.current_call_stack_element();

        let lang_class_name = if let Some(member_of_class) = member_of_class {
            if let Some(class_name) = member_of_class.borrow().class_name() {
                Some(class_name.to_string())
            }else {
                Some("<class>".to_string())
            }
        }else {
            None
        };

        let new_stack_element = StackElement::new(
            if let Some(function_lang_path) = function_lang_path {
                function_lang_path
            }else {
                current_stack_element.lang_path()
            },
            if function_lang_path.is_none() && function_lang_file.is_none() {
                current_stack_element.lang_file()
            }else {
                function_lang_file
            },
            //TODO improve when if let chains become stable
            if let Some(member_of_class_value) = member_of_class {
                if !member_of_class_value.borrow().is_class() {
                    Some(member_of_class_value.borrow().base_definition().unwrap())
                }else {
                    member_of_class.cloned()
                }
            }else {
                member_of_class.cloned()
            },
            lang_class_name.as_deref(),
            Some(&function_name),
            current_stack_element.module.clone(),
        );
        self.push_stack_element(new_stack_element, parent_pos);

        let ret = self.call_function_pointer_internal(
            fp,
            this_object,
            &function_name,
            internal_function,
            argument_list,
            combined_argument_list,
            parent_pos,
        );

        if let Some(this_object) = this_object {
            let mut this_object = this_object.borrow_mut();

            if !this_object.is_class() {
                this_object.set_super_level(original_super_level.unwrap()).unwrap();
            }
        }

        //Update call stack
        self.pop_stack_element();

        ret
    }
    fn interpret_function_pointer_arguments(&mut self, argument_list: &[Node]) -> Vec<DataObjectRef> {
        let mut argument_value_list = Vec::new();
        let mut previous_data_object = None;
        for argument in argument_list {
            if matches!(argument.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) &&
                    previous_data_object.is_some() {
                let ret = self.process_function_call_previous_node_value_node(
                    argument,
                    previous_data_object.clone(),
                );

                if matches!(ret.node_data(), NodeData::FunctionCallPreviousNodeValue {..}) {
                    //Remove last data Object, because it is used as function pointer for a function call
                    argument_value_list.pop();
                    argument_value_list.push(self.interpret_function_call_previous_node(&ret, previous_data_object.unwrap()));
                }else {
                    argument_value_list.push(self.interpret_node(None, &ret).unwrap());
                }

                previous_data_object = Some(argument_value_list[argument_value_list.len() - 1].clone());

                continue;
            }

            //Composite type unpacking
            if let NodeData::UnprocessedVariableName(variable_name) = argument.node_data() {
                if variable_name.contains("&") && variable_name.ends_with("...") {
                    let mut variable_name = variable_name.to_string();

                    let is_module_variable = variable_name.starts_with("[[");
                    let module_name = if is_module_variable {
                        let index_module_identifier_end = variable_name.find("]]::");
                        let Some(index_module_identifier_end) = index_module_identifier_end else {
                            argument_value_list.push(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Invalid variable name"),
                                argument.pos(),
                            ));

                            continue;
                        };

                        let module_name = variable_name[2..index_module_identifier_end].to_string();
                        if !Self::is_alpha_numeric_with_underline(&module_name) {
                            argument_value_list.push(self.set_errno_error_object(
                                InterpretingError::InvalidAstNode,
                                Some("Invalid module name"),
                                argument.pos(),
                            ));

                            continue;
                        }

                        variable_name = variable_name[index_module_identifier_end + 4..].to_string();

                        Some(module_name)
                    }else {
                        None
                    };

                    if variable_name.starts_with("&") {
                        let data_object = self.get_or_create_data_object_from_variable_name(
                            None,
                            module_name.as_deref(),
                            &variable_name[..variable_name.len() - 3],
                            false,
                            false,
                            false,
                            &mut [false, false],
                            argument.pos(),
                        );

                        let Some(data_object) = data_object else {
                            argument_value_list.push(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some("Unpacking of undefined variable"),
                                argument.pos(),
                            ));

                            continue;
                        };

                        if let Some(array_value) = data_object.array_value() {
                            let array_value = array_value.borrow();
                            let array_value = array_value.deref().deref();

                            argument_value_list.append(&mut utils::separate_arguments_with_argument_separators(
                                array_value,
                            ));
                        }else if let Some(list_value) = data_object.list_value() {
                            let mut list_value = list_value.borrow().clone();

                            argument_value_list.append(&mut utils::separate_arguments_with_argument_separators(
                                list_value.make_contiguous(),
                            ));
                        }else if let Some(struct_value) = data_object.struct_value() {
                            if struct_value.is_definition() {
                                let member_names = struct_value.member_names().iter().
                                        map(|member_name| DataObjectRef::new(DataObject::new_text(&**member_name))).
                                        collect::<Vec<_>>();

                                argument_value_list.append(&mut utils::separate_arguments_with_argument_separators(
                                    &member_names,
                                ));
                            }else {
                                let members = struct_value.member_names().iter().
                                        map(|member_name| struct_value.get_member(member_name).unwrap()).
                                        collect::<Vec<_>>();

                                argument_value_list.append(&mut utils::separate_arguments_with_argument_separators(
                                    &members,
                                ));
                            }
                        }else {
                            argument_value_list.push(self.set_errno_error_object(
                                InterpretingError::InvalidArguments,
                                Some("Unpacking of unsupported composite type variable"),
                                argument.pos(),
                            ));
                        }

                        continue;
                    }
                }
            }

            let argument_value = self.interpret_node(None, argument);
            if let Some(argument_value) = &argument_value {
                argument_value_list.push(argument_value.clone());
            }

            previous_data_object = argument_value;
        }

        argument_value_list
    }
    /**
     * @return Will return void data for non-return value functions
     */
    fn interpret_function_call_node(&mut self, composite_type: OptionDataObjectRef, node: &Node) -> OptionDataObjectRef {
        let NodeData::FunctionCall(function_name) = node.node_data() else {
            panic!("Invalid AST node");
        };
        let original_function_name = function_name;
        let mut function_name = function_name.to_string();

        if function_name.starts_with("mp.") {
            let Some(composite_type) = &composite_type else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Method call without object"),
                    node.pos(),
                ));
            };

            //TODO improve when if let chains become stable
            if let Some(object_value) = composite_type.object_value() {
                if object_value.borrow().is_class() {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some("Method can not be called on classes"),
                        node.pos(),
                    ));
                }
            }else if let Some(text_value) = composite_type.text_value() {
                if &*text_value != "super" {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some("Method call without object"),
                        node.pos(),
                    ));
                }
            }else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Method call without object"),
                    node.pos(),
                ));
            }
        }

        let is_module_variable = composite_type.is_none() && function_name.starts_with("[[");
        let variables = if is_module_variable {
            let index_module_identifier_end = function_name.find("]]::");
            let Some(index_module_identifier_end) = index_module_identifier_end else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid variable name"),
                    node.pos(),
                ));
            };

            let module_name = function_name[2..index_module_identifier_end].to_string();
            if !Self::is_alpha_numeric_with_underline(&module_name) {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid module name"),
                    node.pos(),
                ));
            }

            function_name = function_name[index_module_identifier_end + 4..].to_string();

            let module = self.modules.get(&Box::from(&*module_name));

            let Some(module) = module else {
                return Some(self.set_errno_error_object(
                    InterpretingError::ModuleLoadUnloadErr,
                    Some(&format!("The module \"{module_name}\" is not loaded!")),
                    node.pos(),
                ));
            };

            module.exported_variables().clone()
        }else {
            self.data_ref().var.clone()
        };

        let fp = if let Some(composite_type) = composite_type {
            let object_value = composite_type.object_value();

            //TODO improve when if let chains become stable
            if let Some(struct_value) = composite_type.struct_value() {
                if !function_name.starts_with("fp.") {
                    function_name = "fp.".to_string() + &function_name;
                }

                let member = struct_value.get_member(&function_name);
                let member = match member {
                    Ok(member) => member,
                    Err(e) => {
                        return Some(self.set_errno_error_object(
                            InterpretingError::IncompatibleDataType,
                            Some(e.message()),
                            node.pos(),
                        ));
                    },
                };

                let Some(fp) = member.function_pointer_value() else {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidFuncPtr,
                        Some(&format!("\"{original_function_name}\": Function pointer is invalid")),
                        node.pos(),
                    ));
                };

                fp
            }else if let Some(text_value) = composite_type.text_value() {
                if &*text_value != "super" {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Invalid composite type"),
                        node.pos(),
                    ));
                }

                let composite_type = self.data_ref().var.get("&this").cloned();
                let Some(composite_type) = composite_type else {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Super can only be used in methods if \"&this\" is present"),
                        CodePosition::EMPTY,
                    ));
                };

                let object_value = {
                    let Some(object_value) = composite_type.object_value() else {
                        return Some(self.set_errno_error_object(
                            InterpretingError::InvalidArguments,
                            Some("Super can only be used in methods if \"&this\" is present"),
                            CodePosition::EMPTY,
                        ));
                    };

                    object_value
                };

                let argument_list = self.interpret_function_pointer_arguments(node.child_nodes());

                if function_name == "construct" {
                    return Some(self.call_super_constructor(&object_value, &argument_list, node.pos()));
                }

                return self.call_super_method(&object_value, &function_name, &argument_list, node.pos());
            }else if let Some(object_value) = object_value {
                let argument_list = self.interpret_function_pointer_arguments(node.child_nodes());

                if function_name == "construct" {
                    return Some(self.call_constructor(&object_value, &argument_list, node.pos()));
                }

                return self.call_method(&object_value, &function_name, &argument_list, node.pos());
            }else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Invalid composite type"),
                    node.pos(),
                ));
            }
        }else if !is_module_variable && Self::is_func_name(&function_name) {
            let is_linker_function;
            if !is_module_variable && function_name.starts_with("func.") {
                is_linker_function = false;

                function_name = function_name[5..].to_string();
            }else if !is_module_variable && function_name.starts_with("fn.") {
                is_linker_function = false;

                function_name = function_name[3..].to_string();
            }else if !is_module_variable && function_name.starts_with("linker.") {
                is_linker_function = true;

                function_name = function_name[7..].to_string();
            }else if !is_module_variable && function_name.starts_with("ln.") {
                is_linker_function = true;

                function_name = function_name[3..].to_string();
            }else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid native, predefined, or linker function name"),
                    node.pos(),
                ));
            }

            let ret = self.funcs.iter().find(|(func_name, func)| {
                func.linker_function() == is_linker_function && function_name == ***func_name
            });

            let Some(ret) = ret else {
                return Some(self.set_errno_error_object(
                    InterpretingError::FunctionNotFound,
                    Some(&format!(
                        "\"{function_name}\": Native, predefined, or linker function was not found",
                    )),
                    node.pos(),
                ));
            };

            let func = ret.1;
            Rc::new(func.copy_with_function_name(original_function_name))
        }else if Self::is_var_name_func_ptr_without_prefix(&function_name) {
            let ret = variables.get(&Rc::from(&*function_name));

            let Some(fp) = ret.and_then(|ret| ret.function_pointer_value()) else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidFuncPtr,
                    Some(&format!("\"{original_function_name}\": Function pointer is invalid")),
                    node.pos(),
                ));
            };

            fp
        }else {
            //Function call without prefix

            //Function pointer
            let ret = variables.get(&Rc::from("fp.".to_string() + &function_name));
            if let Some(ret) = ret {
                let Some(fp) = ret.function_pointer_value() else {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidFuncPtr,
                        Some(&format!("\"{original_function_name}\": Function pointer is invalid")),
                        node.pos(),
                    ));
                };

                fp
            }else if !is_module_variable {
                //Predefined function

                let ret = self.funcs.iter().find(|(func_name, func)| {
                    !func.linker_function() && function_name == ***func_name
                });

                if let Some(ret) = ret {
                    let func = ret.1;
                    Rc::new(func.copy_with_function_name(&("func.".to_string() + &function_name)))
                }else {
                    //Predefined linker function
                    let ret = self.funcs.iter().find(|(func_name, func)| {
                        func.linker_function() && function_name == ***func_name
                    });

                    let Some(ret) = ret else {
                        return Some(self.set_errno_error_object(
                            InterpretingError::FunctionNotFound,
                            Some(&format!(
                                "\"{original_function_name}\": Normal, native, predefined, or linker function was not found",
                            )),
                            node.pos(),
                        ));
                    };

                    let func = ret.1;
                    Rc::new(func.copy_with_function_name(&("linker.".to_string() + &function_name)))
                }
            }else {
                return Some(self.set_errno_error_object(
                    InterpretingError::FunctionNotFound,
                    Some(&format!(
                        "\"{original_function_name}\": Normal, native, predefined, or linker function was not found",
                    )),
                    node.pos(),
                ));
            }
        };

        let argument_list = self.interpret_function_pointer_arguments(node.child_nodes());

        self.call_function_pointer(fp.deref(), Some(&function_name), &argument_list, node.pos())
    }

    fn interpret_function_call_previous_node(&mut self, node: &Node, previous_value: DataObjectRef) -> DataObjectRef {
        let NodeData::FunctionCallPreviousNodeValue { .. } = node.node_data() else {
            panic!("Invalid AST node");
        };

        let args = self.interpret_function_pointer_arguments(node.child_nodes());

        let ret = operators::op_call(self, &previous_value, &args, node.pos());
        if let Some(ret) = ret {
            ret
        }else {
            self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Invalid data type"),
                node.pos(),
            )
        }
    }

    fn interpret_function_definition_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let NodeData::FunctionDefinition(function_definition) = node.node_data() else {
            panic!("Invalid AST node");
        };

        let function_name_without_prefix;
        let function_pointer_data_object;
        let mut flags = [false, false];
        if let Some(function_name) = function_definition.function_name() {
            if function_name.starts_with("$") || function_name.starts_with("&") {
                function_name_without_prefix = Some(&function_name[1..]);
            }else {
                function_name_without_prefix = function_name.strip_prefix("fp.");
            }

            function_pointer_data_object = self.get_or_create_data_object_from_variable_name(
                None,
                None,
                function_name,
                false,
                false,
                !function_definition.overloaded(),
                &mut flags,
                node.pos(),
            );

            if flags[0] {
                //Forward error from getOrCreateDataObjectFromVariableName()
                return function_pointer_data_object;
            }

            //TODO improve when if let chains become stable
            let Some(function_pointer_data_object) = &function_pointer_data_object else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some(&format!(
                        "Can not overload variable \"{function_name}\" because the variable does not \
                        exist or is not of type function pointer",
                    )),
                    node.pos(),
                ));
            };
            if function_definition.overloaded() &&
                    function_pointer_data_object.data_type() != DataType::FUNCTION_POINTER {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some(&format!(
                        "Can not overload variable \"{function_name}\" because the variable does not \
                        exist or is not of type function pointer",
                    )),
                    node.pos(),
                ));
            }

            if !function_definition.overloaded() &&
                    function_pointer_data_object.data_type() != DataType::NULL {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some(&format!(
                        "Can not set \"{function_name}\" to function because the variable already \
                        exists (You should use \"function overload\" instead of \"function\" to overload a function)",
                    )),
                    node.pos(),
                ));
            }

            let Some(variable_name) = function_pointer_data_object.variable_name() else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some("Anonymous values can not be changed"),
                    node.pos(),
                ));
            };

            if function_pointer_data_object.is_final_data() || function_pointer_data_object.is_lang_var() {
                if flags[1] {
                    self.data_mut().var.remove(&Rc::from(variable_name));
                }

                return Some(self.set_errno_error_object(
                    InterpretingError::FinalVarChange,
                    None,
                    node.pos(),
                ));
            }
        }else {
            function_pointer_data_object = None;
            function_name_without_prefix = None;
        }

        let mut parameter_doc_comments = HashMap::new();
        let mut string_builder = String::new();
        if let Some(doc_comment) = function_definition.doc_comment() {
            for token in doc_comment.split("\n") {
                let token = token.trim();

                if token.starts_with("@param") && token.chars().nth(6).
                        is_some_and(|char| char.is_whitespace()) {
                    if let Some(colon_index) = token.find(":") {
                        let name = token[6..colon_index].trim();
                        let comment = token[colon_index + 1..].trim();
                        parameter_doc_comments.insert(name, comment);

                        continue;
                    }
                }

                string_builder += token;
                string_builder += "\n";
            }

            //Remove trailing "\n"
            if !string_builder.is_empty() {
                string_builder.remove(string_builder.len() - 1);
            }
        }

        let function_doc_comment = function_definition.doc_comment().map(|_| string_builder);

        let mut parameters = Vec::with_capacity(node.child_nodes().len());
        let mut argument_pos_list = Vec::with_capacity(node.child_nodes().len());

        for (index, child) in node.child_nodes().iter().
                enumerate() {
            let NodeData::VariableName {
                variable_name,
                type_constraint,
            } = child.node_data() else {
                return if matches!(child.node_data(), NodeData::ParsingError {..}) {
                    self.interpret_node(None, child)
                }else {
                    Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some("Invalid AST node type for parameter"),
                        node.pos(),
                    ))
                };
            };

            let parameter_type;
            let parameter_type_constraint;
            if let Some(type_constraint) = type_constraint {
                match &**type_constraint {
                    "bool" => {
                        parameter_type_constraint = None;
                        parameter_type = ParameterType::Boolean;
                    },

                    "number" => {
                        parameter_type_constraint = None;
                        parameter_type = ParameterType::Number;
                    },

                    "callable" => {
                        parameter_type_constraint = None;
                        parameter_type = ParameterType::Callable;
                    },

                    _ => {
                        let mut error_out = DataObject::with_update(|data_object| {
                            data_object.set_void()
                        }).unwrap();
                        parameter_type_constraint = Some(self.interpret_type_constraint(
                            type_constraint, &mut error_out, child.pos(),
                        ));

                        if error_out.data_type() == DataType::ERROR {
                            return Some(DataObjectRef::new(error_out));
                        }

                        parameter_type = ParameterType::Normal;
                    },
                }
            }else {
                parameter_type_constraint = None;
                parameter_type = ParameterType::Normal;
            }

            if index == node.child_nodes().len() - 1 && !Self::is_lang_var_without_prefix(variable_name) &&
                    Self::is_func_call_var_args(variable_name) {
                //Varargs (only the last parameter can be a varargs parameter)

                //Remove "..."
                let variable_name = &variable_name[..variable_name.len() - 3];

                parameters.push(ParameterMetadata::new(
                    variable_name,
                    parameter_doc_comments.remove(variable_name),
                    Some(parameter_type_constraint.unwrap_or_else(|| data::CONSTRAINT_NORMAL.clone())),
                    ParameterType::VarArgs,
                ));
                argument_pos_list.push(child.pos());

                continue;
            }

            if Self::is_func_call_call_by_ptr(variable_name) &&
                    !Self::is_func_call_call_by_ptr_lang_var(variable_name) {
                //Remove '[' and ']' from variable name;
                let variable_name = "$".to_string() + &variable_name[2..variable_name.len() - 1];

                parameters.push(ParameterMetadata::new(
                    &variable_name,
                    parameter_doc_comments.remove(&*variable_name),
                    Some(parameter_type_constraint.unwrap_or_else(|| DataObject::get_type_constraint_for(Some(&variable_name)).clone())),
                    ParameterType::CallByPointer,
                ));
                argument_pos_list.push(child.pos());

                continue;
            }

            if !Self::is_var_name_without_prefix(variable_name) || Self::is_lang_var_without_prefix(variable_name) {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some(&format!("Invalid parameter: \"{variable_name}\"")),
                    node.pos(),
                ));
            }

            parameters.push(ParameterMetadata::new(
                variable_name,
                parameter_doc_comments.remove(&**variable_name),
                Some(parameter_type_constraint.unwrap_or_else(|| DataObject::get_type_constraint_for(Some(variable_name)).clone())),
                parameter_type,
            ));
            argument_pos_list.push(child.pos());
        }

        let return_value_type_constraint = if let Some(return_value_type_constraint) = function_definition.return_value_type_constraint() {
            let mut error_out = DataObject::with_update(|data_object| {
                data_object.set_void()
            }).unwrap();
            let return_value_type_constraint = self.interpret_type_constraint(
                return_value_type_constraint, &mut error_out, node.pos(),
            );

            if error_out.data_type() == DataType::ERROR {
                return Some(DataObjectRef::new(error_out));
            }

            return_value_type_constraint
        }else {
            data::CONSTRAINT_NORMAL.clone()
        };

        if !parameter_doc_comments.is_empty() {
            self.set_errno(
                InterpretingError::InvalidDocComment,
                Some(&format!(
                    "The following parameters defined in the doc comment do not exist: {}",
                    parameter_doc_comments.into_keys().
                            collect::<Vec<_>>().
                            join(", "),
                )),
                node.pos(),
            );
        }

        let function_metadata = FunctionMetadata::new(
            function_name_without_prefix,
            function_doc_comment.as_deref(),
            false,
            function_definition.combinator(),
            false,
            None,
            parameters,
            Some(return_value_type_constraint),
        );

        let function = Function::new_normal(
            NormalFunction::new(
                argument_pos_list,
                function_definition.function_body().clone(),
                Some(&self.current_call_stack_element.lang_path),
                self.current_call_stack_element.lang_file.as_deref(),
            ),
            &function_metadata,
        );

        let Some(function_pointer_data_object) = function_pointer_data_object else {
            return Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                let mut fp = FunctionPointerObject::new(&function_metadata, function);
                if let Some(lang_class) = &self.current_call_stack_element.lang_class {
                    fp = fp.copy_with_mapped_functions(|internal_functions| internal_functions.
                            copy_with_class_member_attributes(lang_class.clone(), Visibility::Public));
                }

                data_object.set_function_pointer(Rc::new(fp))
            }).unwrap()));
        };

        let ret = if function_definition.overloaded() {
            let mut internal_function = InternalFunction::new(Rc::new(function));
            if let Some(lang_class) = &self.current_call_stack_element.lang_class {
                internal_function = internal_function.copy_with_class_member_attributes(lang_class.clone(), Visibility::Public);
            }

            let fp = function_pointer_data_object.function_pointer_value().unwrap().
                    copy_with_added_function(internal_function);

            function_pointer_data_object.borrow_mut().set_function_pointer(Rc::new(fp)).map(|_| ())
        }else {
            let mut fp = FunctionPointerObject::new(&function_metadata, function);
            if let Some(lang_class) = &self.current_call_stack_element.lang_class {
                fp = fp.copy_with_mapped_functions(|internal_functions| internal_functions.
                        copy_with_class_member_attributes(lang_class.clone(), Visibility::Public));
            }

            function_pointer_data_object.borrow_mut().update(|data_object| {
                data_object.set_function_pointer(Rc::new(fp))?.
                        set_type_constraint(Box::new(DataTypeConstraint::from_single_allowed_type(
                            DataType::FUNCTION_POINTER,
                        )))
            })
        };
        if ret.is_err() {
            if flags[1] {
                self.data_mut().var.remove(&Rc::from(function_pointer_data_object.variable_name().unwrap()));
            }

            return Some(self.set_errno_error_object(
                InterpretingError::IncompatibleDataType,
                Some(&format!(
                    "Incompatible type for function definition: \"{}\" was already defined and cannot be set to a function definition",
                    function_pointer_data_object.variable_name().unwrap(),
                )),
                node.pos(),
            ));
        }

        Some(function_pointer_data_object)
    }

    fn interpret_array_node(&mut self, node: &Node) -> DataObjectRef {
        let NodeData::ArrayValue = node.node_data() else {
            panic!("Invalid AST node");
        };

        let mut interpreted_nodes = Vec::new();

        for element in node.child_nodes() {
            let argument_value = self.interpret_node(None, element);
            let Some(argument_value) = argument_value else {
                continue;
            };

            interpreted_nodes.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&argument_value.borrow())
            }).unwrap()));
        }

        let elements = utils::combine_arguments_without_argument_separators(
            &interpreted_nodes,
            self,
            node.pos(),
        );
        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(elements.into_boxed_slice())
        }).unwrap())
    }

    fn interpret_struct_definition_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let NodeData::StructDefinition(struct_definition) = node.node_data() else {
            panic!("Invalid AST node");
        };

        let struct_data_object;
        let mut flags = [false, false];
        if let Some(struct_name) = struct_definition.struct_name() {
            struct_data_object = self.get_or_create_data_object_from_variable_name(
                None,
                None,
                struct_name,
                false,
                false,
                true,
                &mut flags,
                node.pos(),
            );
            if flags[0] {
                return struct_data_object; //Forward error from getOrCreateDataObjectFromVariableName()
            }

            //TODO improve when if let chains become stable
            let Some(struct_data_object) = &struct_data_object else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some("Anonymous values can not be changed"),
                    node.pos(),
                ));
            };

            let Some(variable_name) = struct_data_object.variable_name() else {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAssignment,
                    Some("Anonymous values can not be changed"),
                    node.pos(),
                ));
            };

            if struct_data_object.is_final_data() || struct_data_object.is_lang_var() {
                if flags[1] {
                    self.data_mut().var.remove(&Rc::from(variable_name));
                }

                return Some(self.set_errno_error_object(
                    InterpretingError::FinalVarChange,
                    None,
                    node.pos(),
                ));
            }
        }else {
            struct_data_object = None;
        }

        let members = struct_definition.members();

        for member in members {
            if !Self::is_var_name_without_prefix(member.name()) {
                return Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some(&format!(
                        "\"{}\" is no valid struct member name",
                        member.name(),
                    )),
                    node.pos(),
                ));
            }
        }

        let unique_member_count = members.iter().
                map(|member| member.name()).
                collect::<HashSet<_>>().
                len();
        if unique_member_count < members.len() {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Struct member name may not be duplicated"),
                node.pos(),
            ));
        }

        let mut interpreted_members = Vec::new();
        for member in members {
            let type_constraint = if let Some(type_constraint) = member.type_constraint() {
                let mut error_out = DataObject::with_update(|data_object| {
                    data_object.set_void()
                }).unwrap();
                let type_constraint = self.interpret_type_constraint(
                    type_constraint, &mut error_out, node.pos(),
                );

                if error_out.data_type() == DataType::ERROR {
                    return Some(DataObjectRef::new(error_out));
                }

                Some(type_constraint)
            }else {
                None
            };

            interpreted_members.push((
                member.name(),
                type_constraint,
            ));
        }

        let struct_object = Rc::new(StructObject::new_definition(
            &interpreted_members,
        ));

        if let Some(data_object) = &struct_data_object {
            let ret = data_object.borrow_mut().update_final(|data_object| {
                data_object.set_struct(struct_object)?.
                        set_type_constraint(Box::new(DataTypeConstraint::from_single_allowed_type(
                            DataType::STRUCT,
                        )))
            });
            if ret.is_err() {
                if flags[1] {
                    self.data_mut().var.remove(&Rc::from(data_object.variable_name().unwrap()));
                }

                return Some(self.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(&format!(
                        "Incompatible type for struct definition: \"{}\" was already defined and cannot be set to a struct definition",
                        data_object.variable_name().unwrap(),
                    )),
                    node.pos(),
                ));
            }

            struct_data_object
        }else {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_struct(struct_object)
            }).unwrap()))
        }
    }

    fn interpret_class_definition_node(&mut self, node: &Node) -> OptionDataObjectRef {
        let NodeData::ClassDefinition(class_definition) = node.node_data() else {
            panic!("Invalid AST node");
        };

        let ret = 'early_ret: {
            //Update call stack
            let current_stack_element = self.current_call_stack_element();
            let new_stack_element = StackElement::new(
                &current_stack_element.lang_path,
                current_stack_element.lang_file.as_deref(),
                Some(self.dummy_class_definition_class.as_ref().unwrap().clone()),
                if let Some(class_name) = class_definition.class_name() {
                    Some(class_name)
                }else {
                    Some("<class>")
                },
                Some("<class-definition>"),
                current_stack_element.module.clone(),
            );
            self.push_stack_element(new_stack_element, node.pos());

            let class_data_object;
            let mut flags = [false, false];
            if let Some(class_name) = class_definition.class_name() {
                class_data_object = self.get_or_create_data_object_from_variable_name(
                    None,
                    None,
                    class_name,
                    false,
                    false,
                    true,
                    &mut flags,
                    node.pos(),
                );
                if flags[0] {
                    break 'early_ret class_data_object; //Forward error from getOrCreateDataObjectFromVariableName()
                }

                //TODO improve when if let chains become stable
                let Some(class_data_object) = &class_data_object else {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAssignment,
                        Some("Anonymous values can not be changed"),
                        node.pos(),
                    ));
                };

                let Some(variable_name) = class_data_object.variable_name() else {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAssignment,
                        Some("Anonymous values can not be changed"),
                        node.pos(),
                    ));
                };

                if class_data_object.is_final_data() || class_data_object.is_lang_var() {
                    if flags[1] {
                        self.data_mut().var.remove(&Rc::from(variable_name));
                    }

                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::FinalVarChange,
                        None,
                        node.pos(),
                    ));
                }
            }else {
                class_data_object = None;
            }

            let parent_class_list = self.interpret_function_pointer_arguments(
                class_definition.parent_classes(),
            );
            let parent_class_list = utils::combine_arguments_without_argument_separators(
                &parent_class_list, self, node.pos(),
            );

            let mut parent_class_object_list = Vec::with_capacity(parent_class_list.len());
            for parent_class in parent_class_list {
                //TODO improve when if let chains become stable
                let Some(object_value) = parent_class.object_value() else {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some("Parent classes must be classes"),
                        node.pos(),
                    ));
                };

                if !object_value.borrow().is_class() {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some("Parent classes must be classes"),
                        node.pos(),
                    ));
                }

                parent_class_object_list.push(object_value);
            }

            let mut interpreted_static_members = Vec::with_capacity(class_definition.static_members().len());
            for static_member in class_definition.static_members() {
                if !Self::is_var_name_without_prefix(static_member.name()) {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some(&format!(
                            "\"{}\" is no valid static member name",
                            static_member.name(),
                        )),
                        node.pos(),
                    ));
                }

                let type_constraint = if let Some(type_constraint) = static_member.type_constraint() {
                    let mut error_out = DataObject::with_update(|data_object| {
                        data_object.set_void()
                    }).unwrap();
                    let type_constraint = self.interpret_type_constraint(
                        type_constraint, &mut error_out, node.pos(),
                    );

                    if error_out.data_type() == DataType::ERROR {
                        break 'early_ret Some(DataObjectRef::new(error_out));
                    }

                    Some(type_constraint)
                }else {
                    None
                };

                let static_member_data_object = DataObject::with_update(|data_object| {
                    let value = static_member.value().
                            map(|value| self.interpret_node(None, value).unwrap_or_else(|| DataObjectRef::new(
                                DataObject::with_update(|data_object| {
                                    data_object.set_void()
                                }).unwrap()),
                            )).
                            unwrap_or_else(|| DataObjectRef::new(DataObject::new()));

                    data_object.set_data(&value.borrow())?;
                    data_object.set_variable_name(Some(static_member.name()))?;

                    if let Some(type_constraint) = type_constraint {
                        data_object.set_type_constraint(Box::new(type_constraint))?;
                    }

                    if static_member.final_flag() {
                        data_object.set_final_data(true);
                    }

                    data_object.set_member_visibility(Some(Visibility::from(static_member.visibility())));

                    Ok(data_object)
                });
                let static_member_data_object = match static_member_data_object {
                    Ok(data_object) => data_object,
                    Err(e) => {
                        break 'early_ret Some(self.set_errno_error_object(
                            InterpretingError::InvalidAstNode,
                            Some(e.message()),
                            node.pos(),
                        ));
                    },
                };

                interpreted_static_members.push(static_member_data_object);
            }

            let unique_static_member_count = class_definition.static_members().iter().
                    map(|member| member.name()).
                    collect::<HashSet<_>>().
                    len();
            if unique_static_member_count < class_definition.static_members().len() {
                break 'early_ret Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Static member name may not be duplicated"),
                    node.pos(),
                ));
            }

            let mut interpreted_members = Vec::with_capacity(class_definition.members().len());
            for member in class_definition.members() {
                if !Self::is_var_name_without_prefix(member.name()) {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some(&format!(
                            "\"{}\" is no valid member name",
                            member.name(),
                        )),
                        node.pos(),
                    ));
                }

                let type_constraint = if let Some(type_constraint) = member.type_constraint() {
                    let mut error_out = DataObject::with_update(|data_object| {
                        data_object.set_void()
                    }).unwrap();
                    let type_constraint = self.interpret_type_constraint(
                        type_constraint, &mut error_out, node.pos(),
                    );

                    if error_out.data_type() == DataType::ERROR {
                        break 'early_ret Some(DataObjectRef::new(error_out));
                    }

                    Some(type_constraint)
                }else {
                    None
                };

                if member.value().is_some() {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some("Member can not be initialized with value"),
                        node.pos(),
                    ));
                }

                interpreted_members.push(MemberDefinition::new(
                    member.name(),
                    type_constraint.map(Box::new),
                    member.final_flag(),
                    Visibility::from(member.visibility()),
                    self.dummy_class_definition_class.as_ref().unwrap().clone(),
                ));
            }

            let unique_member_count = class_definition.members().iter().
                    map(|member| member.name()).
                    collect::<HashSet<_>>().
                    len();
            if unique_member_count < class_definition.members().len() {
                break 'early_ret Some(self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Member name may not be duplicated"),
                    node.pos(),
                ));
            }

            let mut methods = HashMap::<Box<str>, FunctionPointerObject>::new();
            let mut method_override_flags = HashMap::<Box<str>, Vec<bool>>::new();
            let mut method_visibility = HashMap::<Box<str>, Vec<Visibility>>::new();
            for method in class_definition.methods() {
                if !Self::is_method_name(method.name()) && !Self::is_operator_method_name(method.name()) &&
                        !Self::is_conversion_method_name(method.name()) {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some(&format!(
                            "\"{}\" is no valid method name",
                            method.name(),
                        )),
                        node.pos(),
                    ));
                }

                let method_body = self.interpret_node(None, method.body());
                let Some(method_body) = method_body.
                        and_then(|method_body| method_body.function_pointer_value()) else {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(&format!(
                            "Methods must be of type \"{:?}\"",
                            DataType::FUNCTION_POINTER,
                        )),
                        node.pos(),
                    ));
                };

                let method_name: Box<str> = Box::from(method.name());
                let overloaded_function_count = method_body.get_overloaded_function_count();

                let entry = methods.entry(method_name.clone());
                match entry {
                    Entry::Occupied(mut entry) => {
                        entry.insert(entry.get().copy_with_added_functions(&method_body));
                    },

                    Entry::Vacant(entry) => {
                        entry.insert(method_body.deref().clone());
                        method_override_flags.insert(method_name.clone(), Vec::new());
                        method_visibility.insert(method_name.clone(), Vec::new());
                    },
                }

                let method_override_flags = method_override_flags.get_mut(&method_name).unwrap();
                for _ in 0..overloaded_function_count {
                    method_override_flags.push(method.override_flag());
                }

                let method_visibility = method_visibility.get_mut(&method_name).unwrap();
                for _ in 0..overloaded_function_count {
                    method_visibility.push(Visibility::from(method.visibility()));
                }
            }

            let mut constructors: Option<FunctionPointerObject> = None;
            let mut constructor_visibility = Vec::new();
            for constructor in class_definition.constructors() {
                let constructor_body = self.interpret_node(None, constructor.body());
                let Some(constructor_body) = constructor_body.
                        and_then(|constructor_body| constructor_body.function_pointer_value()) else {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(&format!(
                            "Constructors must be of type \"{:?}\"",
                            DataType::FUNCTION_POINTER,
                        )),
                        node.pos(),
                    ));
                };

                let overloaded_function_count = constructor_body.get_overloaded_function_count();

                if let Some(constructors) = &mut constructors {
                    *constructors = constructors.copy_with_added_functions(&constructor_body);
                }else {
                    constructors = Some(constructor_body.deref().clone());
                }

                for _ in 0..overloaded_function_count {
                    constructor_visibility.push(Visibility::from(constructor.visibility()));
                }
            }

            //Set default constructor if no constructor is defined
            let constructors = constructors.unwrap_or_else(|| {
                constructor_visibility.push(Visibility::Public);

                let constructor = |_: &mut Interpreter, _: LangObjectRef| {};
                FunctionPointerObject::from(crate::lang_func!(
                    constructor,
                    crate::lang_func_metadata!(
                        name="construct",
                        return_type_constraint(
                            allowed=["VOID"],
                        ),
                    ),
                ))
            });

            let lang_object = LangObject::new_class(
                self,
                class_definition.class_name(),
                interpreted_static_members,
                interpreted_members,
                methods,
                method_override_flags,
                method_visibility,
                constructors,
                constructor_visibility,
                parent_class_object_list,
            );
            let lang_object = match lang_object {
                Ok(lang_object) => lang_object,

                Err(e) => {
                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::InvalidAstNode,
                        Some(e.message()),
                        node.pos(),
                    ));
                },
            };

            //"break early_ret": Prevent false positive RustRover error "value used after move"
            break 'early_ret if let Some(data_object) = &class_data_object {
                let ret = data_object.borrow_mut().update_final(|data_object| {
                    data_object.set_object(lang_object)?.
                            set_type_constraint(Box::new(DataTypeConstraint::from_single_allowed_type(
                                DataType::OBJECT,
                            )))
                });
                if ret.is_err() {
                    if flags[1] {
                        self.data_mut().var.remove(&Rc::from(data_object.variable_name().unwrap()));
                    }

                    break 'early_ret Some(self.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(&format!(
                            "Incompatible type for class definition: \"{}\" was already defined and cannot be set to a class definition",
                            data_object.variable_name().unwrap(),
                        )),
                        node.pos(),
                    ));
                }

                class_data_object
            }else {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_object(lang_object)
                }).unwrap()))
            };
        };
        //Finally (Execute even if early return)
        {
            //Update call stack
            self.pop_stack_element();
        }
        //Return if early return and normally
        ret
    }

    fn interpret_type_constraint(
        &mut self,
        mut type_constraint: &str,
        error_out: &mut DataObject,
        pos: CodePosition,
    ) -> DataTypeConstraint {
        if type_constraint.is_empty() {
            error_out.set_data(&self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Empty type constraint is not allowed"),
                pos,
            ).borrow()).unwrap();

            return DataTypeConstraint::from_allowed_types(&[]);
        }

        let nullable = type_constraint.as_bytes()[0] == b'?';
        let inverted = type_constraint.as_bytes()[0] == b'!';
        let mut type_values = Vec::new();

        if nullable || inverted {
            type_constraint = &type_constraint[1..];
        }

        loop {
            let pipe_index = type_constraint.find("|");
            let end_index = pipe_index.unwrap_or(type_constraint.len());
            let type_value = &type_constraint[..end_index];

            if type_value.is_empty() || pipe_index.is_some_and(|pipe_index| pipe_index == type_constraint.len() - 1) {
                error_out.set_data(&self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Empty type constraint is not allowed"),
                    pos,
                ).borrow()).unwrap();

                return DataTypeConstraint::from_allowed_types(&[]);
            }

            type_constraint = pipe_index.
                    map(|pipe_index| &type_constraint[pipe_index + 1..]).
                    unwrap_or("");

            let data_type = DataType::from_str(type_value);
            if let Some(data_type) = data_type {
                type_values.push(data_type);
            }else {
                error_out.set_data(&self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some(&format!("Invalid type: \"{type_value}\"")),
                    pos,
                ).borrow()).unwrap();

                return DataTypeConstraint::from_allowed_types(&[]);
            }

            if pipe_index.is_none() {
                break;
            }
        }

        if nullable {
            type_values.push(DataType::NULL);
        }

        if inverted {
            DataTypeConstraint::from_not_allowed_types(&type_values)
        }else {
            DataTypeConstraint::from_allowed_types(&type_values)
        }
    }

    /**
     * @param argumentList The argument list without argument separators of the function call without the format argument (= argument at index 0). Used data objects will be removed from the list
     * @param fullArgumentList The argument list of the function call where every argument are already combined to single values without argument separators with the format argument
     * (= argument at index 0). This list will not be modified and is used for value referencing by index
     *
     * @return The count of chars used for the format sequence
    */
    fn interpret_next_format_sequence(
        &mut self,
        format: &str,
        builder: &mut String,
        argument_list: &mut VecDeque<&DataObjectRef>,
        full_argument_list: &[DataObjectRef],
    ) -> Result<usize, FormatSequenceError> {
        const POSSIBLE_FORMATS: [char; 10] = ['b', 'c', 'd', 'f', 'n', 'o', 's', 't', 'x', '?'];

        let min_end_index = POSSIBLE_FORMATS.into_iter().
                filter_map(|possible_format| format.find(possible_format)).
                min();

        let Some(min_end_index) = min_end_index else {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid format specifier")));
        };

        let mut full_format = &format[..min_end_index + 1];
        let format_type = full_format.as_bytes()[full_format.len() - 1];

        //Parsing format arguments
        let value_specified_index = if full_format.as_bytes()[0] == b'[' {
            let value_specified_index_end_index = full_format.find(']');
            let Some(value_specified_index_end_index) = value_specified_index_end_index else {
                return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Missing closing bracket in value index")));
            };

            let number = &full_format[1..value_specified_index_end_index];
            full_format = &full_format[value_specified_index_end_index + 1..];

            for char in number.bytes() {
                if !char.is_ascii_digit() {
                    return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid number in value index")));
                }
            }
            let number = usize::from_str(number).unwrap();
            if number >= full_argument_list.len() {
                return Err(FormatSequenceError::SpecifiedIndexOutOfBounds(Box::from("For value index")));
            }

            Some(number)
        }else {
            None
        };

        let force_sign = full_format.as_bytes()[0] == b'+';
        if force_sign {
            full_format = &full_format[1..];
        }

        let sing_space = full_format.as_bytes()[0] == b' ';
        if sing_space {
            full_format = &full_format[1..];
        }

        let left_justify = full_format.as_bytes()[0] == b'-';
        if left_justify {
            full_format = &full_format[1..];
        }

        let mut leading_zeros = full_format.as_bytes()[0] == b'0';
        if leading_zeros {
            full_format = &full_format[1..];
        }

        let size_in_argument = full_format.as_bytes()[0] == b'*';
        if size_in_argument {
            full_format = &full_format[1..];
        }
        let size_argument_index = if size_in_argument && full_format.as_bytes()[0] == b'[' {
            let value_specified_index_end_index = full_format.find(']');
            let Some(value_specified_index_end_index) = value_specified_index_end_index else {
                return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Missing closing bracket in size argument index")));
            };

            let number = &full_format[1..value_specified_index_end_index];
            full_format = &full_format[value_specified_index_end_index + 1..];

            for char in number.bytes() {
                if !char.is_ascii_digit() {
                    return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid number in value index")));
                }
            }
            let number = usize::from_str(number).unwrap();
            if number >= full_argument_list.len() {
                return Err(FormatSequenceError::SpecifiedIndexOutOfBounds(Box::from("For value index")));
            }

            Some(number)
        }else {
            None
        };

        let mut size = if full_format.as_bytes()[0].is_ascii_digit() {
            let mut i = 0;
            while i < full_format.len() && full_format.as_bytes()[i].is_ascii_digit() {
                i += 1;
            }

            let number = &full_format[..i];
            full_format = &full_format[i..];

            Some(usize::from_str(number).unwrap())
        }else {
            None
        };

        let decimal_places = full_format.as_bytes()[0] == b'.';
        let decimal_places_in_argument;
        let decimal_places_count_index;
        let mut decimal_places_count;
        if decimal_places {
            full_format = &full_format[1..];
            decimal_places_in_argument = full_format.as_bytes()[0] == b'*';
            if decimal_places_in_argument {
                full_format = &full_format[1..];
            }

            decimal_places_count_index = if decimal_places_in_argument && full_format.as_bytes()[0] == b'[' {
                let decimal_places_count_index_end_index = full_format.find(']');
                let Some(decimal_places_count_index_end_index) = decimal_places_count_index_end_index else {
                    return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Missing closing bracket in decimal places index")));
                };

                let number = &full_format[1..decimal_places_count_index_end_index];
                full_format = &full_format[decimal_places_count_index_end_index + 1..];

                for char in number.bytes() {
                    if !char.is_ascii_digit() {
                        return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid number in decimal places index")));
                    }
                }
                let number = usize::from_str(number).unwrap();
                if number >= full_argument_list.len() {
                    return Err(FormatSequenceError::SpecifiedIndexOutOfBounds(Box::from("For decimal places index")));
                }

                Some(number)
            }else {
                None
            };

            decimal_places_count = if full_format.as_bytes()[0].is_ascii_digit() {
                let mut i = 0;
                while i < full_format.len() && full_format.as_bytes()[i].is_ascii_digit() {
                    i += 1;
                }

                let number = &full_format[..i];
                full_format = &full_format[i..];

                Some(usize::from_str(number).unwrap())
            }else {
                None
            };
        }else {
            decimal_places_in_argument = false;
            decimal_places_count_index = None;
            decimal_places_count = None;
        }

        if full_format.as_bytes()[0] != format_type {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid characters")));
        }

        if (size_in_argument && size.is_some()) || (decimal_places_in_argument && decimal_places_count.is_some()) || (left_justify && leading_zeros) {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Invalid format argument combinations")));
        }

        if left_justify && (!size_in_argument && size.is_none()) {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from("Missing size format argument for leftJustify")));
        }

        //Invalid arguments for formatType
        if matches!(
            format_type,
            b'n'
        ) && (value_specified_index.is_some() || size_in_argument || size.is_some()) {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from(format!("Value index and size can not be used with %{format_type}"))));
        }

        if matches!(
            format_type,
            b'n' |
            b'c' | b'?' |
            b's' | b't'
        ) && (force_sign || sing_space || leading_zeros) {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from(format!("Force sign, space sing, and leading zeros can not be used with %{format_type}"))));
        }

        if matches!(
            format_type,
            b'n' |
            b'c' | b'?' |
            //No %s nor %t, because they can have decimal places
            b'b' | b'd' | b'o' | b'x'
        ) && decimal_places {
            return Err(FormatSequenceError::InvalidFormatSequence(Box::from(format!("Decimal places can not be used with %{format_type}"))));
        }

        //Get size from arguments
        if size_in_argument {
            if size_argument_index.is_none() && argument_list.is_empty() {
                return Err(FormatSequenceError::InvalidArgCount(Box::from("Size argument missing")));
            }

            let data_object = if let Some(size_argument_index) = size_argument_index {
                full_argument_list.get(size_argument_index).unwrap()
            }else {
                argument_list.pop_front().unwrap()
            };

            let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return Err(FormatSequenceError::InvalidArguments(Box::from("Invalid number for size from arguments")));
            };

            let number = number.int_value();
            if number < 0 {
                return Err(FormatSequenceError::InvalidArguments(Box::from("Size must be >= 0")));
            }

            size = Some(number as usize);
        }
        if decimal_places_in_argument {
            if decimal_places_count_index.is_none() && argument_list.is_empty() {
                return Err(FormatSequenceError::InvalidArgCount(Box::from("Decimal places argument missing")));
            }

            let data_object = if let Some(decimal_places_count_index) = decimal_places_count_index {
                full_argument_list.get(decimal_places_count_index).unwrap()
            }else {
                argument_list.pop_front().unwrap()
            };

            let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return Err(FormatSequenceError::InvalidArguments(Box::from("Invalid number for decimal places from arguments")));
            };

            let number = number.int_value();
            if number < 0 {
                return Err(FormatSequenceError::InvalidArguments(Box::from("Decimal places must be >= 0")));
            }

            decimal_places_count = Some(number as usize);
        }

        let output = if format_type == b'n' {
            Some(utils::LINE_SEPARATOR.to_string())
        }else {
            if value_specified_index.is_none() && argument_list.is_empty() {
                return Err(FormatSequenceError::InvalidArgCount(Box::from("Argument missing")));
            }

            let data_object = if let Some(value_specified_index) = value_specified_index {
                full_argument_list.get(value_specified_index).unwrap()
            }else {
                argument_list.pop_front().unwrap()
            };

            match format_type {
                b'd' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let mut output = format!("{}", number.long_value());
                    if force_sign && output.as_bytes()[0] != b'-' {
                        output = "+".to_string() + &output;
                    }

                    if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                        output = " ".to_string() + &output;
                    }

                    Some(output)
                },

                b'b' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let sign = if number.long_value().is_negative() { "-" } else { "" };
                    let number_abs = number.long_value().unsigned_abs();

                    let mut output = format!("{sign}{:b}", number_abs);
                    if force_sign && output.as_bytes()[0] != b'-' {
                        output = "+".to_string() + &output;
                    }

                    if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                        output = " ".to_string() + &output;
                    }

                    Some(output)
                },

                b'o' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let sign = if number.long_value().is_negative() { "-" } else { "" };
                    let number_abs = number.long_value().unsigned_abs();

                    let mut output = format!("{sign}{:o}", number_abs);
                    if force_sign && output.as_bytes()[0] != b'-' {
                        output = "+".to_string() + &output;
                    }

                    if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                        output = " ".to_string() + &output;
                    }

                    Some(output)
                },

                b'x' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let sign = if number.long_value().is_negative() { "-" } else { "" };
                    let number_abs = number.long_value().unsigned_abs();

                    let mut output = format!("{sign}{:X}", number_abs);
                    if force_sign && output.as_bytes()[0] != b'-' {
                        output = "+".to_string() + &output;
                    }

                    if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                        output = " ".to_string() + &output;
                    }

                    Some(output)
                },

                b'f' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let value = number.double_value();
                    if value.is_nan() {
                        let mut output = "NaN".to_string();
                        leading_zeros = false;

                        if force_sign || sing_space {
                            output = " ".to_string() + &output;
                        }

                        Some(output)
                    }else if value.is_infinite() {
                        let mut output = (if value == f64::NEG_INFINITY { "-" } else { "" }).to_string() + "Infinity";
                        leading_zeros = false;

                        if force_sign && output.as_bytes()[0] != b'-' {
                            output = "+".to_string() + &output;
                        }

                        if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                            output = " ".to_string() + &output;
                        }

                        Some(output)
                    }else {
                        let mut output = if let Some(decimal_places_count) = decimal_places_count {
                            format!("{:.*}", decimal_places_count, value)
                        }else {
                            format!("{}", value)
                        };

                        if force_sign && output.as_bytes()[0] != b'-' {
                            output = "+".to_string() + &output;
                        }

                        if sing_space && !matches!(output.as_bytes()[0], b'+'| b'-') {
                            output = " ".to_string() + &output;
                        }

                        Some(output)
                    }
                },

                b'c' => {
                    let number = conversions::to_number(self, data_object, CodePosition::EMPTY);
                    let Some(number) = number else {
                        return Err(FormatSequenceError::InvalidArguments(Box::from(format!("Argument can not be converted to number which is required for %{format_type}"))));
                    };

                    let code_point = number.int_value() as u32;
                    let char = char::from_u32(code_point).unwrap_or('\u{FFFD}');

                    Some(char.to_string())
                },

                b's' => {
                    let mut output = conversions::to_text(self, data_object, CodePosition::EMPTY).to_string();

                    if let Some(decimal_places_count) = decimal_places_count {
                        let ret = utils::format_translation_template_pluralization(
                            &output,
                            decimal_places_count as i32,
                        );
                        match ret {
                            Ok(ret) => output = ret,

                            Err(e) => {
                                return Err(FormatSequenceError::TranslationInvalidPluralizationTemplate(Box::from(e.message())));
                            },
                        }
                    }

                    Some(output)
                },

                b't' => {
                    let translation_key = conversions::to_text(self, data_object, CodePosition::EMPTY).to_string();
                    let translation_value = self.data_ref().lang.get(&Rc::from(translation_key.as_str())).cloned();

                    let Some(output) = translation_value else {
                        return Err(FormatSequenceError::TranslationKeyNotFound(Box::from(format!("For translation key \"{translation_key}\""))));
                    };

                    let mut output = output.to_string();

                    if let Some(decimal_places_count) = decimal_places_count {
                        let ret = utils::format_translation_template_pluralization(
                            &output,
                            decimal_places_count as i32,
                        );
                        match ret {
                            Ok(ret) => output = ret,

                            Err(e) => {
                                return Err(FormatSequenceError::TranslationInvalidPluralizationTemplate(Box::from(e.message())));
                            },
                        }
                    }

                    Some(output)
                },

                b'?' => {
                    let output = conversions::to_bool(
                        self,
                        data_object,
                        CodePosition::EMPTY,
                    );
                    let output = if output { "true" } else { "false" };

                    Some(output.to_string())
                },

                _ => None,
            }
        };

        if let Some(mut output) = output {
            if let Some(size) = size {
                if left_justify {
                    if output.len() < size {
                        output = format!("{1:<0$}", size, output);
                    }
                }else if leading_zeros {
                    let sign_output = if matches!(output.as_bytes()[0], b'+'| b'-' | b' ') {
                        let sign_output = output.as_bytes()[0];
                        output = output[1..].to_string();

                        sign_output
                    }else {
                        0
                    };

                    let padding_size = size - if sign_output == 0 { 0 } else { 1 };
                    output = format!("{1:0>0$}", padding_size, output);

                    if sign_output != 0 {
                        output = (sign_output as char).to_string() + &output;
                    }
                }else if output.len() < size {
                    output = format!("{1:>0$}", size, output);
                }
            }

            *builder += &output;
        };

        Ok(min_end_index + 1)
    }
    /**
     * @param argumentList The argument list without argument separators of the function call. Used data objects will be removed from the list
     *
     * @return The formated text as TextObject or an ErrorObject if an error occurred
     */
    fn format_text(&mut self, format: &str, argument_list: &[DataObjectRef]) -> DataObjectRef {
        let mut builder = String::new();

        let mut full_argument_list = Vec::with_capacity(argument_list.len() + 1);
        full_argument_list.push(DataObjectRef::new(DataObject::new_text(format)));
        full_argument_list.extend_from_slice(argument_list);

        let mut argument_list = VecDeque::from_iter(argument_list);

        let mut i = 0;
        while i < format.len() {
            let percent_index = format[i..].find("%").
                    map(|percent_index| percent_index + i);
            if let Some(percent_index) = percent_index {
                builder += &format[i..percent_index];

                i = percent_index + 1;

                if i == format.len() {
                    return self.set_errno_error_object_error_only(
                        InterpretingError::InvalidFormat,
                    );
                }

                if format.as_bytes()[i] == b'%' {
                    builder.push('%');

                    i += 1;

                    continue;
                }

                let ret = self.interpret_next_format_sequence(
                    &format[i..],
                    &mut builder,
                    &mut argument_list,
                    &full_argument_list,
                );
                match ret {
                    Ok(char_count_used) => i += char_count_used,

                    Err(e) => {
                        let (interpreting_error, message) = match e {
                            FormatSequenceError::InvalidFormatSequence(message) => (InterpretingError::InvalidFormat, message),
                            FormatSequenceError::InvalidArguments(message) => (InterpretingError::InvalidArguments, message),
                            FormatSequenceError::InvalidArgCount(message) => (InterpretingError::InvalidArgCount, message),
                            FormatSequenceError::TranslationKeyNotFound(message) => (InterpretingError::TransKeyNotFound, message),
                            FormatSequenceError::SpecifiedIndexOutOfBounds(message) => (InterpretingError::IndexOutOfBounds, message),
                            FormatSequenceError::TranslationInvalidPluralizationTemplate(message) => (InterpretingError::InvalidTemplateSyntax, message),
                        };

                        return self.set_errno_error_object(
                            interpreting_error,
                            Some(&message),
                            CodePosition::EMPTY,
                        );
                    },
                }
            }else {
                builder += &format[i..];
                break;
            }
        }

        DataObjectRef::new(DataObject::new_text(builder))
    }

    pub fn call_constructor(
        &mut self,
        lang_object: &LangObjectRef,
        argument_list: &[DataObjectRef],
        pos: CodePosition,
    ) -> DataObjectRef {
        let is_class = lang_object.borrow().is_class();

        if is_class {
            let created_object = LangObject::new_object(lang_object).unwrap();

            let constructors = created_object.borrow().constructors();

            let ret = self.call_function_pointer(
                &constructors,
                constructors.function_name(),
                argument_list,
                pos,
            ).unwrap_or_else(|| DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_void()
            }).unwrap()));

            if ret.data_type() != DataType::VOID {
                return self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid constructor implementation: VOID must be returned"),
                    pos,
                );
            }

            let ret = created_object.borrow_mut().post_construct();
            if let Err(e) = ret {
                return self.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(&format!(
                        "Invalid constructor implementation (Some members have invalid types): {}",
                        e.message(),
                    )),
                    pos,
                );
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_object(created_object)
            }).unwrap())
        }else {
            if lang_object.borrow().is_initialized().unwrap() {
                return self.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Object is already initialized"),
                    pos,
                );
            }

            //Current constructors for super level instead of normal constructors, because constructors are not overridden
            let constructors = lang_object.borrow().constructors_for_current_super_level();

            let ret = self.call_function_pointer(
                &constructors,
                constructors.function_name(),
                argument_list,
                pos,
            ).unwrap_or_else(|| DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_void()
            }).unwrap()));

            if ret.data_type() != DataType::VOID {
                return self.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid constructor implementation: VOID must be returned"),
                    pos,
                );
            }

            ret
        }
    }

    pub fn call_method(
        &mut self,
        lang_object: &LangObjectRef,
        raw_method_name: &str,
        argument_list: &[DataObjectRef],
        pos: CodePosition,
    ) -> OptionDataObjectRef {
        if raw_method_name.starts_with("fn.") || raw_method_name.starts_with("func.") ||
                raw_method_name.starts_with("ln.") || raw_method_name.starts_with("linker.") {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The method \"{raw_method_name}\" is not part of this object")),
                pos,
            ));
        }

        let method_name = if lang_object.borrow().is_class() || raw_method_name.starts_with("fp.") {
            None
        }else if raw_method_name.starts_with("mp.") || raw_method_name.starts_with("op:") ||
                raw_method_name.starts_with("to:") {
            Some(raw_method_name.to_string())
        }else {
            Some("mp.".to_string() + raw_method_name)
        };

        let fp = {
            let lang_object = lang_object.borrow();
            let methods = method_name.and_then(|method_name| lang_object.methods().
                    get(&Box::from(method_name)).cloned());

            if let Some(methods) = methods {
                methods
            }else {
                if raw_method_name.starts_with("mp.") {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!("The method \"{raw_method_name}\" is not part of this object")),
                        pos,
                    ));
                }

                let raw_method_name =  if !raw_method_name.starts_with("fp.") &&
                        !raw_method_name.starts_with("op:") &&
                        !raw_method_name.starts_with("to:") {
                    "fp.".to_string() + raw_method_name
                }else {
                    raw_method_name.to_string()
                };


                let member = lang_object.static_member(&raw_method_name);
                let member = match member {
                    Ok(member) => member,

                    Err(e) => {
                        if lang_object.is_class() {
                            return Some(self.set_errno_error_object(
                                InterpretingError::IncompatibleDataType,
                                Some(e.message()),
                                pos,
                            ));
                        }

                        let member = lang_object.member(&raw_method_name);

                        match member {
                            Ok(member) => member,

                            Err(e) => {
                                return Some(self.set_errno_error_object(
                                    InterpretingError::IncompatibleDataType,
                                    Some(e.message()),
                                    pos,
                                ));
                            },
                        }
                    },
                };

                if !member.is_accessible(self.current_call_stack_element().lang_class()) {
                    return Some(self.set_errno_error_object(
                        InterpretingError::MemberNotAccessible,
                        Some(&format!("For member \"{raw_method_name}\"")),
                        CodePosition::EMPTY,
                    ));
                }

                let Some(fp) = member.function_pointer_value() else {
                    return Some(self.set_errno_error_object(
                        InterpretingError::InvalidFuncPtr,
                        Some(&format!("\"{raw_method_name}\": Function pointer is invalid")),
                        pos,
                    ));
                };

                fp
            }
        };

        self.call_function_pointer(&fp, Some(raw_method_name), argument_list, pos)
    }

    pub fn call_super_constructor(
        &mut self,
        lang_object: &LangObjectRef,
        argument_list: &[DataObjectRef],
        pos: CodePosition,
    ) -> DataObjectRef {
        if lang_object.borrow().is_class() {
            return self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Super constructor can not be called on class"),
                pos,
            );
        }

        if lang_object.borrow().is_initialized().unwrap() {
            return self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Object is already initialized"),
                pos,
            );
        }

        let super_constructors = lang_object.borrow().super_constructors();

        //Bind "&this" on super constructor
        let super_constructors = FunctionPointerObject::copy_with_this_object(
            &super_constructors,
            lang_object.clone(),
        ).unwrap().copy_with_mapped_functions(|internal_function| internal_function.
                copy_with_super_level(
                    internal_function.super_level().unwrap_or_default() +
                            lang_object.borrow().super_level().unwrap() + 1,
                ));

        let ret = self.call_function_pointer(
            &super_constructors,
            super_constructors.function_name(),
            argument_list,
            pos,
        ).unwrap_or_else(|| DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_void()
        }).unwrap()));

        if ret.data_type() != DataType::VOID {
            return self.set_errno_error_object(
                InterpretingError::InvalidAstNode,
                Some("Invalid constructor implementation: VOID must be returned"),
                pos,
            );
        }

        ret
    }

    pub fn call_super_method(
        &mut self,
        lang_object: &LangObjectRef,
        raw_method_name: &str,
        argument_list: &[DataObjectRef],
        pos: CodePosition,
    ) -> OptionDataObjectRef {
        if raw_method_name.starts_with("fp.") || raw_method_name.starts_with("fn.") ||
                raw_method_name.starts_with("func.") || raw_method_name.starts_with("ln.") ||
                raw_method_name.starts_with("linker.") {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The method \"{raw_method_name}\" is not part of this object")),
                pos,
            ));
        }

        let method_name = if raw_method_name.starts_with("mp.") || raw_method_name.starts_with("op:") ||
                raw_method_name.starts_with("to:") {
            raw_method_name.to_string()
        }else {
            "mp.".to_string() + raw_method_name
        };

        let methods = lang_object.borrow().super_methods().get(&Box::from(method_name)).cloned();
        let Some(methods) = methods else {
            return Some(self.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The method \"{raw_method_name}\" is not in any super class of this object")),
                pos,
            ));
        };

        //Bind "&this" on super method
        let fp = FunctionPointerObject::copy_with_this_object(
            &methods,
            lang_object.clone(),
        ).unwrap().copy_with_mapped_functions(|internal_function| internal_function.
                copy_with_super_level(
                    internal_function.super_level().unwrap_or_default() +
                            lang_object.borrow().super_level().unwrap() + 1,
                ));

        self.call_function_pointer(&fp, Some(raw_method_name), argument_list, pos)
    }

    /**
     * LangPatterns: Regex: \w+
     */
    fn is_alpha_numeric_with_underline(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        for c in token.bytes() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                return false;
            }
        }

        true
    }

    /**
     * LangPatterns: LANG_VAR ((\$|&)LANG_.*)
     */
    fn is_lang_var_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let first_char = token.as_bytes()[0];
        (first_char == b'$' || first_char == b'&') && token[1..].starts_with("LANG_")
    }

    /**
     * LangPatterns: LANG_VAR ((\$|&)LANG_.*) || LANG_VAR_POINTER_REDIRECTION (\$\[+LANG_.*\]+)
     */
    fn is_lang_var_or_lang_var_pointer_redirection_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let first_char = token.as_bytes()[0];
        (first_char == b'$' || first_char == b'&') && (token[1..].starts_with("LANG_") || token.contains("[LANG_"))
    }

    /**
     * LangPatterns: FUNC_CALL_VAR_ARGS ((\$|&)\w+\.\.\.)
     */
    fn is_func_call_var_args(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let first_char = token.as_bytes()[0];
        if !((first_char == b'$' || first_char == b'&') && token.ends_with("...")) {
            return false;
        }

        let mut i = 1;
        let mut has_var_name = false;
        while i < token.len() - 3 {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: FUNC_CALL_CALL_BY_PTR (\$\[\w+\])
     */
    fn is_func_call_call_by_ptr(token: &str) -> bool {
        if !(token.starts_with("$[") && token.ends_with("]")) {
            return false;
        }

        let mut i = 2;
        let mut has_var_name = false;
        while i < token.len() - 1 {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: FUNC_CALL_CALL_BY_PTR_LANG_VAR (\$\[LANG_.*\])
     */
    fn is_func_call_call_by_ptr_lang_var(token: &str) -> bool {
        token.starts_with("$[LANG_") && token.ends_with("]")
    }

    /**
     * LangPatterns: VAR_NAME_WITHOUT_PREFIX ((\$|&|fp\.)\w+)
     */
    fn is_var_name_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let is_func_ptr = token.starts_with("fp.");

        let first_char = token.as_bytes()[0];
        if !(is_func_ptr || first_char == b'$' || first_char == b'&') {
            return false;
        }

        let mut i = if is_func_ptr { 3 } else { 1 };
        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: METHOD_NAME (mp\.\w+)
     */
    fn is_method_name(token: &str) -> bool {
        if !token.starts_with("mp.") {
            return false;
        }

        let mut i = 3;

        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    const OPERATOR_METHOD_NAMES: [&'static str; 55] = [
        "op:len",
        "op:deepCopy",
        "op:inc",
        "op:dec",
        "op:pos",
        "op:inv",
        "op:not",
        "op:abs",
        "op:iter",
        "op:hasNext",
        "op:next",

        "op:concat", "op:r-concat",
        "op:add", "op:r-add",
        "op:sub", "op:r-sub",
        "op:mul", "op:r-mul",
        "op:pow", "op:r-pow",
        "op:div", "op:r-div",
        "op:truncDiv", "op:r-truncDiv",
        "op:floorDiv", "op:r-floorDiv",
        "op:ceilDiv", "op:r-ceilDiv",
        "op:mod", "op:r-mod",
        "op:and", "op:r-and",
        "op:or", "op:r-or",
        "op:xor", "op:r-xor",
        "op:lshift", "op:r-lshift",
        "op:rshift", "op:r-rshift",
        "op:rzshift", "op:r-rzshift",
        "op:isEquals", "op:r-isEquals",
        "op:isStrictEquals", "op:r-isStrictEquals",
        "op:isLessThan", "op:r-isLessThan",
        "op:isGreaterThan", "op:r-isGreaterThan",

        "op:getItem",
        "op:setItem",
        "op:slice",

        "op:call",
    ];
    /**
     * LangPatterns: OPERATOR_METHOD_NAME <code>op:((len|deepCopy|inc|dec|pos|inv|not|abs|iter|hasNext|next)|
     * ((r-)?(concat|add|sub|mul|pow|div|truncDiv|floorDiv|ceilDiv|mod|and|or|xor|lshift|rshift|rzshift|
     * isEquals|isStrictEquals|isLessThan|isGreaterThan))|(getItem|setItem|slice)|(call)))</code>
     */
    fn is_operator_method_name(token: &str) -> bool {
        Self::OPERATOR_METHOD_NAMES.contains(&token)
    }

    const CONVERSION_METHOD_NAMES: [&'static str; 11] = [
        "to:text",
        "to:char",
        "to:int",
        "to:long",
        "to:float",
        "to:double",
        "to:byteBuffer",
        "to:array",
        "to:list",

        "to:bool",
        "to:number",
    ];
    /**
     * LangPatterns: CONVERSION_METHOD_NAME <code>to:(text|char|int|long|float|double|byteBuffer|array|list|bool|number)</code>
     */
    fn is_conversion_method_name(token: &str) -> bool {
        Self::CONVERSION_METHOD_NAMES.contains(&token)
    }

    /**
     * LangPatterns: VAR_NAME_FULL ((\$\**|&|fp\.)\w+)
     */
    fn is_var_name_full_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let func_ptr = token.starts_with("fp.");
        let first_char = token.as_bytes()[0];
        let normal_var = first_char == b'$';

        if !(func_ptr || normal_var || first_char == b'&') {
            return false;
        }

        let mut i = if func_ptr { 3 } else { 1 };

        if normal_var {
            while i < token.len() {
                if token.as_bytes()[i] != b'*' {
                    break;
                }

                i += 1;
            }
        }

        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: VAR_NAME_FULL_WITH_FUNCS ((\$\**|&|fp\.|mp\.|func\.|fn\.|linker\.|ln\.)\w+)
     */
    fn is_var_name_full_with_funcs_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let is_func_ptr = token.starts_with("fp.");
        let is_method_ptr = token.starts_with("mp.");
        let is_func = token.starts_with("func.");
        let is_fn = token.starts_with("fn.");
        let is_linker = token.starts_with("linker.");
        let is_ln = token.starts_with("ln.");

        let first_char = token.as_bytes()[0];
        let normal_var = first_char == b'$';

        if !(is_func_ptr || is_method_ptr || is_func || is_fn || is_linker || is_ln || normal_var || first_char == b'&') {
            return false;
        }

        let mut i = if is_func_ptr || is_method_ptr || is_fn || is_ln { 3 } else if is_func { 5 } else if is_linker { 7 } else { 1 };

        if normal_var {
            while i < token.len() {
                if token.as_bytes()[i] != b'*' {
                    break;
                }

                i += 1;
            }
        }

        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: VAR_NAME_PTR_AND_DEREFERENCE (\$\**\[+\w+\]+)
     */
    fn is_var_name_ptr_and_dereference_without_prefix(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        if token.as_bytes()[0] != b'$' {
            return false;
        }

        let mut i = 1;
        while i < token.len() {
            if token.as_bytes()[i] != b'*' {
                break;
            }

            i += 1;
        }

        let mut has_no_bracket_opening = true;
        while i < token.len() {
            if token.as_bytes()[i] == b'[' {
                has_no_bracket_opening = false;
            }else {
                break;
            }

            i += 1;
        }

        if has_no_bracket_opening {
            return false;
        }

        let mut has_no_var_name = true;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_no_var_name = false;
            }else {
                break;
            }

            i += 1;
        }

        if has_no_var_name {
            return false;
        }

        let mut has_bracket_closing = false;
        while i < token.len() {
            if token.as_bytes()[i] == b']' {
                has_bracket_closing = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_bracket_closing
    }

    /**
     * LangPatterns: FUNC_NAME ((func\.|fn\.|linker\.|ln\.)\w+)
     */
    fn is_func_name(token: &str) -> bool {
        let is_func = token.starts_with("func.");
        let is_linker = token.starts_with("linker.");

        if !(is_func || is_linker || token.starts_with("fn.") || token.starts_with("ln.")) {
            return false;
        }

        let mut i = if is_func { 5 } else if is_linker { 7 } else { 3 };

        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    /**
     * LangPatterns: VAR_NAME_FUNC_PTR (fp\.\w+)
     */
    fn is_var_name_func_ptr_without_prefix(token: &str) -> bool {
        if !token.starts_with("fp.") {
            return false;
        }

        let mut i = 3;

        let mut has_var_name = false;
        while i < token.len() {
            let c = token.as_bytes()[i];
            if c.is_ascii_alphanumeric() || c == b'_' {
                has_var_name = true;
            }else {
                return false;
            }

            i += 1;
        }

        has_var_name
    }

    fn scope_id(&self) -> isize {
        self.scope_id
    }

    fn init_lang_standard(&mut self) {
        if !self.is_initializing_lang_standard_implementation {
            panic!("Initialization of lang standard implementation was already completed");
        }

        //&Object class
        {
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

            self.object_class = Some(LangObject::new_class_internal(
                self, true, Some("&Object"), Vec::new(), Vec::new(),
                methods, method_override_flags, method_visibility, constructor, constructor_visibility,
                Vec::new(),
            ).unwrap());
        }

        //<class-definition> tmp class
        {
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

            self.dummy_class_definition_class = Some(LangObject::new_class(
                self, Some("<class-definition>"), Vec::new(), Vec::new(),
                HashMap::new(), HashMap::new(), HashMap::new(), constructor, constructor_visibility,
                Vec::new(),
            ).unwrap());
        }

        //Init predefined and linker functions
        {
            predefined_functions::add_predefined_functions(&mut self.funcs);
            predefined_functions::add_predefined_linker_functions(&mut self.funcs);
        }

        //Temporary scope for lang standard implementation in lang code
        self.push_stack_element(
            StackElement::new(
                "<standard>",
                Some("standard.lang"),
                None,
                None,
                None,
                None,
            ),
            CodePosition::EMPTY,
        );
        self.enter_scope(None);

        let ret = self.init_lang_standard_lang_code();
        if let Err(e) = ret {
            panic!("Could not load lang standard implementation in lang code: {e:?}")
        }

        //Cleanup of temporary scope
        self.pop_stack_element();

        self.exit_scope();

        self.reset_parser_positional_vars();

        self.is_initializing_lang_standard_implementation = false;
    }

    fn init_lang_standard_lang_code(&mut self) -> Result<(), NativeError> {
        let file = Self::RESOURCES_DIR.
                get_file("lang/standard.lang").
                ok_or_else(|| NativeError::new(
                    "The \"standard.lang\" file was not complied into executable!", None,
                ))?;

        lang_vars::add_essential_lang_vars(self, None);

        let lang_standard_implementation = String::from_utf8_lossy(file.contents());

        //Interpret lang standard implementation lang code
        self.interpret_lines(lang_standard_implementation);

        let data = self.data().clone();
        for (variable_name, variable) in &data.borrow().var {
            if let Some(function_name) = variable_name.strip_prefix("fp.") {
                if let Some(function_value) = variable.function_pointer_value() {
                    self.funcs.insert(Box::from(function_name), Rc::new(function_value.
                            copy_with_function_name(&("func.".to_string() + function_name))));
                }
            }else if matches!(variable.data_type(), DataType::STRUCT | DataType::OBJECT) {
                self.standard_types.insert(Box::from(&**variable_name), variable.clone());
            }
        }

        Ok(())
    }

    fn enter_scope(&mut self, lang_args: Option<Vec<Box<str>>>) {
        self.scope_id += 1;

        self.data.insert(self.scope_id as usize, Rc::new(RefCell::new(Data::new())));

        if let Some(lang_args) = lang_args {
            let lang_args = lang_args.into_iter().
                    map(|arg| DataObjectRef::new(DataObject::new_text(arg))).
                    collect::<Box<[_]>>();

            self.data_mut().var.insert(Rc::from("&LANG_ARGS"), DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(lang_args).
                        unwrap().set_final_data(true).
                        set_variable_name(Some("&LANG_ARGS"))
            }).unwrap()));
        }

        self.reset_vars_and_func_ptrs();

        if self.scope_id > 0 {
            //Copy translation map (except "lang.* = *") to the new scope's translation map
            for (k, v) in &self.data[&(self.scope_id as usize - 1)].clone().borrow().lang {
                if !k.starts_with("lang.") {
                    self.data_mut().lang.insert(k.clone(), v.clone());
                }
            }
        }
    }
    fn reset_vars_and_func_ptrs(&mut self) {
        let lang_args = self.data_ref().var.get("&LANG_ARGS").cloned();
        self.data_mut().var.clear();

        if self.is_initializing_lang_standard_implementation {
            lang_vars::add_essential_lang_vars(self, lang_args);
        }else {
            lang_vars::add_lang_vars(self, lang_args);
        }
    }
    fn reset_vars(&mut self) {
        self.data_mut().var.retain(|k, v| v.is_lang_var() &&
                (k.starts_with("$") || k.starts_with("&")));

        //Not final vars
        self.set_errno(InterpretingError::NoError, None, CodePosition::EMPTY); //Set $LANG_ERRNO
    }

    fn exit_scope(&mut self) {
        if !self.is_initializing_lang_standard_implementation && self.scope_id == 0 {
            self.set_errno(
                InterpretingError::SystemError,
                Some("Main scope can not be exited"),
                CodePosition::EMPTY,
            );

            return;
        }

        self.data.remove(&(self.scope_id as usize));

        self.scope_id -= 1;
    }

    fn set_errno(
        &mut self,
        error: InterpretingError,
        message: Option<&str>,
        pos: CodePosition,
    ) {
        self.set_errno_internal(error, message, pos, false)
    }

    fn set_errno_internal(
        &mut self,
        error: InterpretingError,
        message: Option<&str>,
        pos: CodePosition,
        force_no_error_output: bool,
    ) {
        let current_errno = self.data_ref().var["$LANG_ERRNO"].int_value().unwrap();
        let new_errno = error.error_code();

        if new_errno >= 0 || current_errno < 1 {
            self.data_mut().var.get_mut("$LANG_ERRNO").unwrap().borrow_mut().set_int(new_errno).unwrap();
        }

        if !force_no_error_output && self.execution_flags.error_output.should_print(new_errno) {
            let message = message.unwrap_or_default();

            let current_stack_element = self.current_call_stack_element();
            let lang_path = current_stack_element.lang_path();
            let lang_file = current_stack_element.lang_file().unwrap_or("<shell>");

            let lang_path_with_file = lang_path.to_string() + if lang_path.ends_with("/") {
                ""
            }else {
                "/"
            } + lang_file;
            let lang_function_name = current_stack_element.lang_function_name();

            let output = format!(
                "A{} {} occurred in \"{}:{}\" (FUNCTION: \"{}\", SCOPE_ID: \"{}\")!\n{}: {} ({}){}\nStack trace:\n{}",
                if new_errno < 0 { "" } else { "n" },
                if new_errno < 0 { "warning" } else { "error" },
                lang_path_with_file,
                if pos == CodePosition::EMPTY {
                    "x".to_string()
                }else {
                    pos.to_compact_string()
                },
                lang_function_name.unwrap_or("<main>"),
                self.scope_id,
                if new_errno < 0 { "Warning" } else { "Error" },
                error.error_text(),
                error.error_code(),
                if message.is_empty() {
                    "".to_string()
                }else {
                    "\nMessage: ".to_string() + message
                },
                self.print_stack_trace(pos),
            );

            if let Some(term) = &mut self.term {
                term.logln(
                    if new_errno < 0 { Level::Warning } else { Level::Error },
                    output,
                    Self::LOG_TAG,
                );
            }else {
                eprintln!("{}", output);
            }
        }

        if new_errno > 0 {
            self.execution_state.is_thrown_value = true;

            if self.execution_state.try_block_level > 0 && (!self.execution_state.is_soft_try ||
                    self.execution_state.try_body_scope_id == self.scope_id) {
                self.execution_state.try_thrown_error = Some(error);
                self.execution_state.stop_execution_flag = true;
            }
        }
    }

    #[must_use]
    fn set_errno_error_object_error_only(
        &mut self,
        error: InterpretingError,
    ) -> DataObjectRef {
        self.set_errno_error_object(error, None, CodePosition::EMPTY)
    }

    #[must_use]
    fn set_errno_error_object(
        &mut self,
        error: InterpretingError,
        message: Option<&str>,
        pos: CodePosition,
    ) -> DataObjectRef {
        self.set_errno_error_object_internal(error, message, pos, false)
    }

    #[must_use]
    fn set_errno_error_object_internal(
        &mut self,
        error: InterpretingError,
        message: Option<&str>,
        pos: CodePosition,
        force_no_error_output: bool,
    ) -> DataObjectRef {
        self.set_errno_internal(error, message, pos, force_no_error_output);

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_error(Rc::new(ErrorObject::new(error, message)))
        }).unwrap())
    }

    fn get_and_clear_errno_error_object(&mut self) -> InterpretingError {
        let errno = self.data_ref().var["$LANG_ERRNO"].int_value().unwrap();

        self.set_errno(InterpretingError::NoError, None, CodePosition::EMPTY); //Reset errno

        InterpretingError::get_error_from_error_code(errno)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum FormatSequenceError {
    InvalidFormatSequence(Box<str>),
    InvalidArguments(Box<str>),
    InvalidArgCount(Box<str>),
    TranslationKeyNotFound(Box<str>),
    SpecifiedIndexOutOfBounds(Box<str>),
    TranslationInvalidPluralizationTemplate(Box<str>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InterpretingError {
    NoError,

    //ERRORS
    FinalVarChange,
    ToManyInnerLinks,
    NoLangFile,
    FileNotFound,
    InvalidFuncPtr,
    StackOverflow,
    NoTerminal,
    InvalidArgCount,
    InvalidLogLevel,
    InvalidArrPtr,
    NoHexNum,
    NoChar,
    NoNum,
    DivByZero,
    NegativeArrayLen,
    EmptyArray,
    LengthNan,
    IndexOutOfBounds,
    ArgCountNotArrLen,
    InvalidFuncPtrLoop,
    InvalidArguments,
    FunctionNotFound,
    Eof,
    SystemError,
    NegativeRepeatCount,
    TransKeyNotFound,
    FunctionNotSupported,
    BracketMismatch,
    ContFlowArgMissing,
    InvalidAstNode,
    InvalidPtr,
    IncompatibleDataType,
    LangArraysCopy,
    LangVerError,
    InvalidConPart,
    InvalidFormat,
    InvalidAssignment,
    NoBinNum,
    NoOctNum,
    NoBaseNNum,
    InvalidNumberBase,
    InvalidRegexSyntax,
    InvalidTemplateSyntax,
    InvalidModule,
    ModuleLoadUnloadErr,
    MemberNotAccessible,

    //WARNINGS
    DeprecatedFuncCall,
    NoTerminalWarning,
    LangVerWarning,
    InvalidExecFlagData,
    VarShadowingWarning,
    UndefEscapeSequence,
    InvalidDocComment,
}

impl InterpretingError {
    pub(crate) const VALUES: [InterpretingError; 54] = [
        Self::NoError,

        //ERRORS
        Self::FinalVarChange,
        Self::ToManyInnerLinks,
        Self::NoLangFile,
        Self::FileNotFound,
        Self::InvalidFuncPtr,
        Self::StackOverflow,
        Self::NoTerminal,
        Self::InvalidArgCount,
        Self::InvalidLogLevel,
        Self::InvalidArrPtr,
        Self::NoHexNum,
        Self::NoChar,
        Self::NoNum,
        Self::DivByZero,
        Self::NegativeArrayLen,
        Self::EmptyArray,
        Self::LengthNan,
        Self::IndexOutOfBounds,
        Self::ArgCountNotArrLen,
        Self::InvalidFuncPtrLoop,
        Self::InvalidArguments,
        Self::FunctionNotFound,
        Self::Eof,
        Self::SystemError,
        Self::NegativeRepeatCount,
        Self::TransKeyNotFound,
        Self::FunctionNotSupported,
        Self::BracketMismatch,
        Self::ContFlowArgMissing,
        Self::InvalidAstNode,
        Self::InvalidPtr,
        Self::IncompatibleDataType,
        Self::LangArraysCopy,
        Self::LangVerError,
        Self::InvalidConPart,
        Self::InvalidFormat,
        Self::InvalidAssignment,
        Self::NoBinNum,
        Self::NoOctNum,
        Self::NoBaseNNum,
        Self::InvalidNumberBase,
        Self::InvalidRegexSyntax,
        Self::InvalidTemplateSyntax,
        Self::InvalidModule,
        Self::ModuleLoadUnloadErr,
        Self::MemberNotAccessible,

        //WARNINGS
        Self::DeprecatedFuncCall,
        Self::NoTerminalWarning,
        Self::LangVerWarning,
        Self::InvalidExecFlagData,
        Self::VarShadowingWarning,
        Self::UndefEscapeSequence,
        Self::InvalidDocComment,
    ];

    pub fn name(&self) -> &'static str {
        match self {
            Self::NoError => "NO_ERROR",

            //ERRORS
            Self::FinalVarChange => "FINAL_VAR_CHANGE",
            Self::ToManyInnerLinks => "TO_MANY_INNER_LINKS",
            Self::NoLangFile => "NO_LANG_FILE",
            Self::FileNotFound => "FILE_NOT_FOUND",
            Self::InvalidFuncPtr => "INVALID_FUNC_PTR",
            Self::StackOverflow => "STACK_OVERFLOW",
            Self::NoTerminal => "NO_TERMINAL",
            Self::InvalidArgCount => "INVALID_ARG_COUNT",
            Self::InvalidLogLevel => "INVALID_LOG_LEVEL",
            Self::InvalidArrPtr => "INVALID_ARR_PTR",
            Self::NoHexNum => "NO_HEX_NUM",
            Self::NoChar => "NO_CHAR",
            Self::NoNum => "NO_NUM",
            Self::DivByZero => "DIV_BY_ZERO",
            Self::NegativeArrayLen => "NEGATIVE_ARRAY_LEN",
            Self::EmptyArray => "EMPTY_ARRAY",
            Self::LengthNan => "LENGTH_NAN",
            Self::IndexOutOfBounds => "INDEX_OUT_OF_BOUNDS",
            Self::ArgCountNotArrLen => "ARG_COUNT_NOT_ARR_LEN",
            Self::InvalidFuncPtrLoop => "INVALID_FUNC_PTR_LOOP",
            Self::InvalidArguments => "INVALID_ARGUMENTS",
            Self::FunctionNotFound => "FUNCTION_NOT_FOUND",
            Self::Eof => "EOF",
            Self::SystemError => "SYSTEM_ERROR",
            Self::NegativeRepeatCount => "NEGATIVE_REPEAT_COUNT",
            Self::TransKeyNotFound => "TRANS_KEY_NOT_FOUND",
            Self::FunctionNotSupported => "FUNCTION_NOT_SUPPORTED",
            Self::BracketMismatch => "BRACKET_MISMATCH",
            Self::ContFlowArgMissing => "CONT_FLOW_ARG_MISSING",
            Self::InvalidAstNode => "INVALID_AST_NODE",
            Self::InvalidPtr => "INVALID_PTR",
            Self::IncompatibleDataType => "INCOMPATIBLE_DATA_TYPE",
            Self::LangArraysCopy => "LANG_ARRAYS_COPY",
            Self::LangVerError => "LANG_VER_ERROR",
            Self::InvalidConPart => "INVALID_CON_PART",
            Self::InvalidFormat => "INVALID_FORMAT",
            Self::InvalidAssignment => "INVALID_ASSIGNMENT",
            Self::NoBinNum => "NO_BIN_NUM",
            Self::NoOctNum => "NO_OCT_NUM",
            Self::NoBaseNNum => "NO_BASE_N_NUM",
            Self::InvalidNumberBase => "INVALID_NUMBER_BASE",
            Self::InvalidRegexSyntax => "INVALID_REGEX_SYNTAX",
            Self::InvalidTemplateSyntax => "INVALID_TEMPLATE_SYNTAX",
            Self::InvalidModule => "INVALID_MODULE",
            Self::ModuleLoadUnloadErr => "MODULE_LOAD_UNLOAD_ERR",
            Self::MemberNotAccessible => "MEMBER_NOT_ACCESSIBLE",

            //WARNINGS
            Self::DeprecatedFuncCall => "DEPRECATED_FUNC_CALL",
            Self::NoTerminalWarning => "NO_TERMINAL_WARNING",
            Self::LangVerWarning => "LANG_VER_WARNING",
            Self::InvalidExecFlagData => "INVALID_EXEC_FLAG_DATA",
            Self::VarShadowingWarning => "VAR_SHADOWING_WARNING",
            Self::UndefEscapeSequence => "UNDEF_ESCAPE_SEQUENCE",
            Self::InvalidDocComment => "INVALID_DOC_COMMENT",
        }
    }

    pub fn error_code(&self) -> i32 {
        match self {
            Self::NoError => 0,

            //ERRORS
            Self::FinalVarChange =>  1,
            Self::ToManyInnerLinks =>  2,
            Self::NoLangFile =>  3,
            Self::FileNotFound =>  4,
            Self::InvalidFuncPtr =>  5,
            Self::StackOverflow =>  6,
            Self::NoTerminal =>  7,
            Self::InvalidArgCount =>  8,
            Self::InvalidLogLevel =>  9,
            Self::InvalidArrPtr => 10,
            Self::NoHexNum => 11,
            Self::NoChar => 12,
            Self::NoNum => 13,
            Self::DivByZero => 14,
            Self::NegativeArrayLen => 15,
            Self::EmptyArray => 16,
            Self::LengthNan => 17,
            Self::IndexOutOfBounds => 18,
            Self::ArgCountNotArrLen => 19,
            Self::InvalidFuncPtrLoop => 20,
            Self::InvalidArguments => 21,
            Self::FunctionNotFound => 22,
            Self::Eof => 23,
            Self::SystemError => 24,
            Self::NegativeRepeatCount => 25,
            Self::TransKeyNotFound => 26,
            Self::FunctionNotSupported => 27,
            Self::BracketMismatch => 28,
            Self::ContFlowArgMissing => 29,
            Self::InvalidAstNode => 30,
            Self::InvalidPtr => 31,
            Self::IncompatibleDataType => 32,
            Self::LangArraysCopy => 33,
            Self::LangVerError => 34,
            Self::InvalidConPart => 35,
            Self::InvalidFormat => 36,
            Self::InvalidAssignment => 37,
            Self::NoBinNum => 38,
            Self::NoOctNum => 39,
            Self::NoBaseNNum => 40,
            Self::InvalidNumberBase => 41,
            Self::InvalidRegexSyntax => 42,
            Self::InvalidTemplateSyntax => 43,
            Self::InvalidModule => 44,
            Self::ModuleLoadUnloadErr => 45,
            Self::MemberNotAccessible => 46,

            //WARNINGS
            Self::DeprecatedFuncCall => -1,
            Self::NoTerminalWarning => -2,
            Self::LangVerWarning => -3,
            Self::InvalidExecFlagData => -4,
            Self::VarShadowingWarning => -5,
            Self::UndefEscapeSequence => -6,
            Self::InvalidDocComment => -7,
        }
    }

    pub fn error_text(&self) -> &'static str {
        match self {
            Self::NoError => "No Error",

            //ERRORS
            Self::FinalVarChange =>  "LANG or final vars must not be changed",
            Self::ToManyInnerLinks =>  "To many inner links",
            Self::NoLangFile =>  "No .lang-File",
            Self::FileNotFound =>  "File not found",
            Self::InvalidFuncPtr =>  "Function pointer is invalid",
            Self::StackOverflow =>  "Stack overflow",
            Self::NoTerminal =>  "No terminal available",
            Self::InvalidArgCount =>  "Invalid argument count",
            Self::InvalidLogLevel =>  "Invalid log level",
            Self::InvalidArrPtr => "Invalid array pointer",
            Self::NoHexNum => "No hexadecimal number",
            Self::NoChar => "No char",
            Self::NoNum => "No number",
            Self::DivByZero => "Dividing by 0",
            Self::NegativeArrayLen => "Negative array length",
            Self::EmptyArray => "Empty array",
            Self::LengthNan => "Length NAN",
            Self::IndexOutOfBounds => "Index out of bounds",
            Self::ArgCountNotArrLen => "Argument count is not array length",
            Self::InvalidFuncPtrLoop => "Invalid function pointer",
            Self::InvalidArguments => "Invalid arguments",
            Self::FunctionNotFound => "Function not found",
            Self::Eof => "End of file was reached early",
            Self::SystemError => "System Error",
            Self::NegativeRepeatCount => "Negative repeat count",
            Self::TransKeyNotFound => "Translation key does not exist",
            Self::FunctionNotSupported => "Function not supported",
            Self::BracketMismatch => "Bracket mismatch",
            Self::ContFlowArgMissing => "Control flow statement condition(s) or argument(s) is/are missing",
            Self::InvalidAstNode => "Invalid AST node or AST node order",
            Self::InvalidPtr => "Invalid pointer",
            Self::IncompatibleDataType => "Incompatible data type",
            Self::LangArraysCopy => "&LANG arrays can not be copied",
            Self::LangVerError => "Lang file's version is not compatible with this version",
            Self::InvalidConPart => "Invalid statement in control flow statement",
            Self::InvalidFormat => "Invalid format sequence",
            Self::InvalidAssignment => "Invalid assignment",
            Self::NoBinNum => "No binary number",
            Self::NoOctNum => "No octal number",
            Self::NoBaseNNum => "Number is not in base N",
            Self::InvalidNumberBase => "Invalid number base",
            Self::InvalidRegexSyntax => "Invalid RegEx syntax",
            Self::InvalidTemplateSyntax => "Invalid translation template syntax",
            Self::InvalidModule => "The Lang module is invalid",
            Self::ModuleLoadUnloadErr => "Error during load or unload of Lang module",
            Self::MemberNotAccessible => "The class/object member is not visible/accessible from the current scope",

            //WARNINGS
            Self::DeprecatedFuncCall => "A deprecated predefined function was called",
            Self::NoTerminalWarning => "No terminal available",
            Self::LangVerWarning => "Lang file's version is not compatible with this version",
            Self::InvalidExecFlagData => "Execution flag or Lang data is invalid",
            Self::VarShadowingWarning => "Variable name shadows an other variable",
            Self::UndefEscapeSequence => "An undefined escape sequence was used",
            Self::InvalidDocComment => "Dangling or invalid doc comment syntax",
        }
    }

    pub fn get_error_from_error_code(error_code: i32) -> Self {
        match error_code {
            0 => Self::NoError,

            //ERRORS
            1 => Self::FinalVarChange,
            2 => Self::ToManyInnerLinks,
            3 => Self::NoLangFile,
            4 => Self::FileNotFound,
            5 => Self::InvalidFuncPtr,
            6 => Self::StackOverflow,
            7 => Self::NoTerminal,
            8 => Self::InvalidArgCount,
            9 => Self::InvalidLogLevel,
            10 => Self::InvalidArrPtr,
            11 => Self::NoHexNum,
            12 => Self::NoChar,
            13 => Self::NoNum,
            14 => Self::DivByZero,
            15 => Self::NegativeArrayLen,
            16 => Self::EmptyArray,
            17 => Self::LengthNan,
            18 => Self::IndexOutOfBounds,
            19 => Self::ArgCountNotArrLen,
            20 => Self::InvalidFuncPtrLoop,
            21 => Self::InvalidArguments,
            22 => Self::FunctionNotFound,
            23 => Self::Eof,
            24 => Self::SystemError,
            25 => Self::NegativeRepeatCount,
            26 => Self::TransKeyNotFound,
            27 => Self::FunctionNotSupported,
            28 => Self::BracketMismatch,
            29 => Self::ContFlowArgMissing,
            30 => Self::InvalidAstNode,
            31 => Self::InvalidPtr,
            32 => Self::IncompatibleDataType,
            33 => Self::LangArraysCopy,
            34 => Self::LangVerError,
            35 => Self::InvalidConPart,
            36 => Self::InvalidFormat,
            37 => Self::InvalidAssignment,
            38 => Self::NoBinNum,
            39 => Self::NoOctNum,
            40 => Self::NoBaseNNum,
            41 => Self::InvalidNumberBase,
            42 => Self::InvalidRegexSyntax,
            43 => Self::InvalidTemplateSyntax,
            44 => Self::InvalidModule,
            45 => Self::ModuleLoadUnloadErr,
            46 => Self::MemberNotAccessible,

            //WARNINGS
            -1 => Self::DeprecatedFuncCall,
            -2 => Self::NoTerminalWarning,
            -3 => Self::LangVerWarning,
            -4 => Self::InvalidExecFlagData,
            -5 => Self::VarShadowingWarning,
            -6 => Self::UndefEscapeSequence,
            -7 => Self::InvalidDocComment,

            _ => Self::NoError,
        }
    }
}

#[derive(Debug)]
pub struct Data {
    lang: HashMap<Rc<str>, Rc<str>>,
    var: HashMap<Rc<str>, DataObjectRef>,
}

impl Data {
    fn new() -> Self {
        Self {
            lang: HashMap::new(),
            var: HashMap::new(),
        }
    }

    pub fn lang(&self) -> &HashMap<Rc<str>, Rc<str>> {
        &self.lang
    }

    pub fn var(&self) -> &HashMap<Rc<str>, DataObjectRef> {
        &self.var
    }
}

#[derive(Debug, Clone)]
pub struct StackElement {
    lang_path: Box<str>,
    lang_file: Option<Box<str>>,
    pos: CodePosition,
    lang_class: OptionLangObjectRef,
    lang_class_name: Option<Box<str>>,
    lang_function_name: Option<Box<str>>,
    module: Option<Rc<Module>>,
}

impl StackElement {
    pub fn new(
        lang_path: &str,
        lang_file: Option<&str>,
        lang_class: OptionLangObjectRef,
        lang_class_name: Option<&str>,
        lang_function_name: Option<&str>,
        module: Option<Rc<Module>>,
    ) -> Self {
        Self {
            lang_path: Box::from(lang_path),
            lang_file: lang_file.map(Box::from),
            pos: CodePosition::EMPTY,
            lang_class,
            lang_class_name: lang_class_name.map(Box::from),
            lang_function_name: lang_function_name.map(Box::from),
            module,
        }
    }

    #[must_use]
    pub fn copy_with_pos(&self, pos: CodePosition) -> Self {
        Self {
            lang_path: self.lang_path.clone(),
            lang_file: self.lang_file.clone(),
            pos,
            lang_class: self.lang_class.clone(),
            lang_class_name: self.lang_class_name.clone(),
            lang_function_name: self.lang_function_name.clone(),
            module: self.module.clone(),
        }
    }

    pub fn lang_path(&self) -> &str {
        &self.lang_path
    }

    pub fn lang_file(&self) -> Option<&str> {
        self.lang_file.as_deref()
    }

    pub fn pos(&self) -> CodePosition {
        self.pos
    }

    pub fn lang_class(&self) -> Option<&LangObjectRef> {
        self.lang_class.as_ref()
    }

    pub fn lang_class_name(&self) -> Option<&str> {
        self.lang_class_name.as_deref()
    }

    pub fn lang_function_name(&self) -> Option<&str> {
        self.lang_function_name.as_deref()
    }

    pub fn module(&self) -> Option<Rc<Module>> {
        self.module.clone()
    }
}

impl Display for StackElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lang_path_with_file = format!(
            "{}{}{}",
            self.lang_path,
            if self.lang_path.ends_with("/") { "" } else { "/" },
            if let Some(lang_file) = &self.lang_file {
                lang_file
            }else {
                "<shell>"
            },
        );

        write!(
            f, "    at \"{}:{}\" in {} \"{}\"",
            lang_path_with_file,
            if self.pos == CodePosition::EMPTY { "x".to_string() } else { self.pos.to_compact_string() },
            if self.lang_class_name.is_some() {
                "method"
            }else {
                "function"
            },
            if let Some(lang_function_name) = &self.lang_function_name {
                if let Some(lang_class_name) = &self.lang_class_name {
                    format!("{}::{}", lang_class_name, lang_function_name)
                }else {
                    lang_function_name.to_string()
                }
            }else {
                "<main>".to_string()
            },
        )
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
#[repr(u8)]
pub enum ErrorOutputFlag {
    Nothing,
    All,
    #[default]
    ErrorOnly,
}

impl ErrorOutputFlag {
    pub fn get_error_flag_for(number: i32) -> Self {
        match number {
            0 => Self::Nothing,
            1.. => Self::All,
            ..=-1 => Self::ErrorOnly,
        }
    }

    pub fn should_print(self, error_code: i32) -> bool {
        (error_code < 0 && self == Self::All) || (error_code > 0 && self != Self::Nothing)
    }
}

#[derive(Debug)]
struct ExecutionFlags {
    /**
     * Allow terminal function to redirect to standard input, output, or error if no terminal is available
     */
    allow_term_redirect: bool,
    /**
     * Will print all errors and warnings in the terminal or to standard error if no terminal is available
     */
    error_output: ErrorOutputFlag,
    /**
     * Will enable langTest unit tests (Can not be disabled if enabled once)
     */
    lang_test: bool,
    /**
     * Will disable variable name processing which makes the interpreter faster
     */
    raw_variable_names: bool,
    /**
     * Will enable printing of native stack traces
     */
    native_stack_traces: bool,
}

impl ExecutionFlags {
    pub fn new() -> Self {
        Self {
            allow_term_redirect: true,
            error_output: ErrorOutputFlag::ErrorOnly,
            lang_test: false,
            raw_variable_names: false,
            native_stack_traces: false,
        }
    }
}

#[derive(Debug)]
struct ExecutionState {
    /**
     * Will be set to true for returning/throwing a value or breaking/continuing a loop or for try statements
     */
    stop_execution_flag: bool,
    force_stop_execution_flag: bool,

    //Fields for return statements
    returned_or_thrown_value: OptionDataObjectRef,
    is_thrown_value: bool,
    return_or_throw_statement_pos: CodePosition,

    //Fields for continue & break statements
    /**
     * If > 0: break or continue statement is being processed
     */
    break_continue_count: u32,
    is_continue_statement: bool,

    //Fields for try statements
    /**
     * Current try block level
     */
    try_block_level: u32,
    try_thrown_error: Option<InterpretingError>,
    is_soft_try: bool,
    try_body_scope_id: isize,
}

impl ExecutionState {
    pub fn new() -> Self {
        Self {
            stop_execution_flag: false,
            force_stop_execution_flag: false,

            returned_or_thrown_value: None,
            is_thrown_value: false,
            return_or_throw_statement_pos: CodePosition::EMPTY,

            break_continue_count: 0,
            is_continue_statement: false,

            try_block_level: 0,
            try_thrown_error: None,
            is_soft_try: false,
            try_body_scope_id: 0,
        }
    }
}
