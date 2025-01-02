use std::fmt::{Display, Formatter};
use std::time::SystemTime;
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, DataTypeConstraintError};
use crate::interpreter::InterpretingError;
use crate::terminal_io::{Level, TerminalIO};

#[derive(Debug)]
pub struct LangTest {
    units: Vec<Unit>,
    start_time: Option<SystemTime>,
}

impl LangTest {
    pub const LOG_TAG: &'static str = "LangTest";

    fn print_failed_test_result(line_prefix: &str, assert_result: &AssertResult) -> String {
        let message = assert_result.message.as_deref();
        let actual_value = assert_result.actual_value.as_deref();
        let expected_value = assert_result.expected_value.as_deref();
        let stack_trace = assert_result.stack_trace.as_deref();

        let stack_trace = stack_trace.map(|stack_trace| {
            let mut stack_trace_with_line_prefixes = String::new();

            for line in stack_trace.split("\n") {
                stack_trace_with_line_prefixes += &format!("\n{line_prefix}{line}");
            }

            stack_trace_with_line_prefixes
        });

        let mut builder = String::new();
        builder += assert_result.assert_test_name;
        builder += ":";

        if let Some(message) = message {
            builder += &format!("\n{line_prefix}Message:  {message}");
        }

        if let Some(actual_value) = actual_value {
            if let Some(expected_value) = expected_value {
                builder += &format!("\n{line_prefix}Actual:   {actual_value}");
                builder += &format!("\n{line_prefix}Excepted: {expected_value}");
            }
        }

        if let Some(stack_trace) = stack_trace {
            builder += &format!("\n{line_prefix}Stack trace:{stack_trace}");
        }

        builder
    }

    pub fn new() -> Self {
        Self {
            units: vec![Unit::new_without_name()],
            start_time: None,
        }
    }

    pub fn add_unit(&mut self, name: &str) {
        self.units.push(Unit::new(name));
    }

    pub fn add_sub_unit(&mut self, name: &str) -> Result<(), DataTypeConstraintError> {
        let len = self.units.len();
        let current_unit = &mut self.units[len - 1];
        if current_unit.name.is_none() {
            return Err(DataTypeConstraintError::with_message("the no name unit must not have sub units"));
        }

        current_unit.sub_units.push(SubUnit::new(name));

        Ok(())
    }

    pub fn add_assert_result(&mut self, assert_result: AssertResult) {
        if self.start_time.is_none() {
            self.start_time = Some(SystemTime::now());
        }

        let len = self.units.len();
        let current_unit = &mut self.units[len - 1];
        let len = current_unit.sub_units.len();
        let current_sub_unit = &mut current_unit.sub_units[len - 1];

        current_sub_unit.add_assert_result(assert_result);
    }

    pub fn test_count(&self) -> usize {
        self.units.iter().
                map(|sub_unit| sub_unit.test_count()).
                sum()
    }

    pub fn test_passed_count(&self) -> usize {
        self.units.iter().
                map(|sub_unit| sub_unit.test_passed_count()).
                sum()
    }

    pub fn print_results(&self) -> String {
        self.to_string()
    }

    pub fn print_results_to_terminal(&self, term: &mut TerminalIO) {
        let end_time = SystemTime::now();
        let start_time = self.start_time.unwrap_or(end_time);

        let diff = end_time.duration_since(start_time).
                expect("Time manipulation detected (Start time is in the future)!").
                as_millis();

        for unit in &self.units {
            unit.print_results_to_terminal(term);
        }

        term.logln(Level::Info, "------------------------------------------", Self::LOG_TAG);
        term.logln(Level::Config, format!(
            "Summary:\nTime taken: {:.3} s\nTests passed: {}/{}",
            diff as f64 / 1000.0,
            self.test_passed_count(),
            self.test_count(),
        ), Self::LOG_TAG);

        let mut out = String::new();
        if self.test_passed_count() != self.test_count() {
            out += "Failed tests:";
        }

        for unit in &self.units {
            if unit.test_passed_count() == unit.test_count() || unit.test_count() == 0 {
                continue;
            }

            out += &format!(
                "\n\tUnit: {}:",
                unit.name.as_ref().map(|name| format!("\"{name}\"")).unwrap_or("noname".to_string()),
            );

            for sub_unit in &unit.sub_units {
                let failed_tests = sub_unit.failed_tests();
                if !failed_tests.is_empty() {
                    if let Some(name) = &sub_unit.name {
                        out += &format!("\n\t\tSubUnit: \"{name}\"");

                        for failed_test in failed_tests {
                            out += &format!(
                                "\n\t\t\t{}",
                                Self::print_failed_test_result("\t\t\t\t", failed_test),
                            );
                        }
                    }else {
                        for failed_test in failed_tests {
                            out += &format!(
                                "\n\t\t{}",
                                Self::print_failed_test_result("\t\t\t", failed_test),
                            );
                        }
                    }
                }
            }
        }

        if !out.is_empty() {
            term.logln(Level::Error, &out, Self::LOG_TAG);
        }
    }
}

impl Default for LangTest {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for LangTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let end_time = SystemTime::now();
        let start_time = self.start_time.unwrap_or(end_time);

        let diff = end_time.duration_since(start_time).
                expect("Time manipulation detected (Start time is in the future)!").
                as_millis();

        let mut out = self.units.iter().
                map(Unit::print_results).
                collect::<Vec<_>>().
                join("");

        out += &format!(
            "------------------------------------------\nSummary:\nTime taken: {:.3} s\nTests passed: {}/{}",
            diff as f64 / 1000.0,
            self.test_passed_count(),
            self.test_count(),
        );

        if self.test_passed_count() != self.test_count() {
            out += "\nFailed tests:";
        }

        for unit in &self.units {
            if unit.test_passed_count() == unit.test_count() || unit.test_count() == 0 {
                continue;
            }

            out += &format!(
                "\n\tUnit: {}:",
                unit.name.as_ref().map(|name| format!("\"{name}\"")).unwrap_or("noname".to_string()),
            );

            for sub_unit in &unit.sub_units {
                let failed_tests = sub_unit.failed_tests();
                if !failed_tests.is_empty() {
                    if let Some(name) = &sub_unit.name {
                        out += &format!("\n\t\tSubUnit: \"{name}\"");

                        for failed_test in failed_tests {
                            out += &format!(
                                "\n\t\t\t{}",
                                Self::print_failed_test_result("\t\t\t\t", failed_test),
                            );
                        }
                    }else {
                        for failed_test in failed_tests {
                            out += &format!(
                                "\n\t\t{}",
                                Self::print_failed_test_result("\t\t\t", failed_test),
                            );
                        }
                    }
                }
            }
        }

        f.write_str(&out)
    }
}

#[derive(Debug)]
struct Unit {
    name: Option<Box<str>>,
    sub_units: Vec<SubUnit>,
}

impl Unit {
    pub const LOG_TAG: &'static str = "LangTest";

    pub fn new(name: &str) -> Self {
        Self {
            name: Some(Box::from(name)),
            sub_units: vec![SubUnit::new_without_name()],
        }
    }

    pub fn new_without_name() -> Self {
        Self {
            name: None,
            sub_units: vec![SubUnit::new_without_name()],
        }
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn sub_units(&self) -> &[SubUnit] {
        &self.sub_units
    }

    pub fn add_sub_unit(&mut self, sub_unit: SubUnit) {
        self.sub_units.push(sub_unit);
    }

    pub fn test_count(&self) -> usize {
        self.sub_units.iter().
                map(|sub_unit| sub_unit.test_count()).
                sum()
    }

    pub fn test_passed_count(&self) -> usize {
        self.sub_units.iter().
                map(|sub_unit| sub_unit.test_passed_count()).
                sum()
    }

    pub fn print_results(&self) -> String {
        self.to_string()
    }

    pub fn print_results_to_terminal(&self, term: &mut TerminalIO) {
        if self.test_count() == 0 {
            return;
        }

        term.logln(Level::Config, format!(
            "Unit: {}:\nTests passed: {}/{}",
            self.name.as_ref().map(|name| format!("\"{name}\"")).unwrap_or("noname".to_string()),
            self.test_passed_count(),
            self.test_count(),
        ), Self::LOG_TAG);

        let mut out = String::new();
        if self.test_passed_count() != self.test_count() {
            out += "Failed tests:";
        }

        for sub_unit in &self.sub_units {
            let failed_tests = sub_unit.failed_tests();
            if !failed_tests.is_empty() {
                if let Some(name) = &sub_unit.name {
                    out += &format!("\n\tSubUnit: \"{name}\"");

                    for failed_test in failed_tests {
                        out += &format!(
                            "\n\t\t{}\n",
                            LangTest::print_failed_test_result("\t\t\t", failed_test),
                        );
                    }
                }else {
                    for failed_test in failed_tests {
                        out += &format!(
                            "\n\t{}\n",
                            LangTest::print_failed_test_result("\t\t", failed_test),
                        );
                    }
                }
            }
        }

        if !out.is_empty() {
            term.logln(Level::Error, &out, Self::LOG_TAG);
        }

        if self.sub_units.len() > 1 {
            term.logln(Level::Config, "SubUnits:", Self::LOG_TAG);

            for sub_unit in &self.sub_units {
                sub_unit.print_results_to_terminal(term);
            }
        }else if out.is_empty() {
            term.logln(Level::Config, "", Self::LOG_TAG);
        }
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.test_count() == 0 {
            return f.write_str("");
        }

        let mut out = format!(
            "Unit: {}:\nTests passed: {}/{}\n",
            self.name.as_ref().map(|name| format!("\"{name}\"")).unwrap_or("noname".to_string()),
            self.test_passed_count(),
            self.test_count(),
        );

        if self.test_passed_count() != self.test_count() {
            out += "Failed tests:";
        }

        for sub_unit in &self.sub_units {
            let failed_tests = sub_unit.failed_tests();
            if !failed_tests.is_empty() {
                if let Some(name) = &sub_unit.name {
                    out += &format!("\n\tSubUnit: \"{name}\"");

                    for failed_test in failed_tests {
                        out += &format!(
                            "\n\t\t{}\n",
                            LangTest::print_failed_test_result("\t\t\t", failed_test),
                        );
                    }
                }else {
                    for failed_test in failed_tests {
                        out += &format!(
                            "\n\t{}\n",
                            LangTest::print_failed_test_result("\t\t", failed_test),
                        );
                    }
                }
            }
        }

        if self.sub_units.len() > 1 {
            out += "SubUnits:";

            for sub_unit in &self.sub_units {
                out += &sub_unit.print_results();
            }
        }else if out.is_empty() {
            out += "\n";
        }

        f.write_str(&out)
    }
}

#[derive(Debug)]
struct SubUnit {
    name: Option<Box<str>>,
    assert_results: Vec<AssertResult>,
}

impl SubUnit {
    pub const LOG_TAG: &'static str = "LangTest";

    pub fn new(name: &str) -> Self {
        Self {
            name: Some(Box::from(name)),
            assert_results: Vec::new(),
        }
    }

    pub fn new_without_name() -> Self {
        Self {
            name: None,
            assert_results: Vec::new(),
        }
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn add_assert_result(&mut self, assert_result: AssertResult) {
        self.assert_results.push(assert_result);
    }

    pub fn test_count(&self) -> usize {
        self.assert_results.len()
    }

    pub fn test_passed_count(&self) -> usize {
        self.assert_results.iter().
                map(|assert_result| assert_result.passed as usize).
                sum::<usize>()
    }

    pub fn failed_tests(&self) -> Vec<&AssertResult> {
        self.assert_results.iter().
                filter(|assert_result| !assert_result.passed).
                collect::<Vec<_>>()
    }

    pub fn print_results(&self) -> String {
        self.to_string()
    }

    pub fn print_results_to_terminal(&self, term: &mut TerminalIO) {
        if self.test_count() == 0 {
            return;
        }

        let Some(name) = &self.name else {
            return;
        };

        term.logln(Level::Config, format!(
            "\tSubUnit: {}:\n\t\tTests passed: {}/{}",
            name,
            self.test_passed_count(),
            self.test_count(),
        ), Self::LOG_TAG);

        let mut out = String::new();
        let failed_tests = self.failed_tests();
        if !failed_tests.is_empty() {
            out += "\t\tFailed tests:";

            for assert_result in failed_tests {
                out += &format!(
                    "\n\t\t\t{}",
                    LangTest::print_failed_test_result("\t\t\t\t", assert_result),
                );
            }
        }

        out += "\n";

        term.logln(Level::Error, &out, Self::LOG_TAG);
    }
}

impl Display for SubUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.test_count() == 0 {
            return f.write_str("");
        }

        let Some(name) = &self.name else {
            return f.write_str("");
        };

        let mut out = format!(
            "\tSubUnit: {}:\n\t\tTests passed: {}/{}\n",
            name,
            self.test_passed_count(),
            self.test_count(),
        );

        let failed_tests = self.failed_tests();
        if !failed_tests.is_empty() {
            out += "\t\tFailed tests:";

            for assert_result in failed_tests {
                out += &format!(
                    "\n\t\t\t{}",
                    LangTest::print_failed_test_result("\t\t\t\t", assert_result),
                );
            }
        }

        out += "\n";

        f.write_str(&out)
    }
}

#[derive(Debug)]
pub struct AssertResult {
    passed: bool,
    assert_test_name: &'static str,
    stack_trace: Option<Box<str>>,
    message: Option<Box<str>>,
    actual_value: Option<Box<str>>,
    expected_value: Option<Box<str>>,
}

impl AssertResult {
    pub fn new_error_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: InterpretingError,
        expected_value: InterpretingError,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertError",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "{} ({})",
                actual_value.error_code(),
                actual_value.name(),
            ))),
            expected_value: Some(Box::from(&*format!(
                "== {} ({})",
                expected_value.error_code(),
                expected_value.name(),
            ))),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn new_generic_data_object_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
        assert_test_name: &'static str,
        expected_operator: &'static str,
    ) -> Self {
        Self {
            passed,
            assert_test_name,
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{}\", Type: {:?}",
                actual_value_text,
                actual_value.data_type(),
            ))),
            expected_value: Some(Box::from(&*format!(
                "{expected_operator} \"{}\", Type: {:?}",
                expected_value_text,
                expected_value.data_type(),
            ))),
        }
    }

    pub fn new_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultEquals", "==",
        )
    }

    pub fn new_not_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultNotEquals", "!=",
        )
    }

    pub fn new_less_than_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultLessThan", "<",
        )
    }

    pub fn new_greater_than_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultGreaterThan", ">",
        )
    }

    pub fn new_less_than_or_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultLessThanOrEquals", "<=",
        )
    }

    pub fn new_greater_than_or_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultGreaterThanOrEquals", ">=",
        )
    }

    pub fn new_strict_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultStrictEquals", "===",
        )
    }

    pub fn new_strict_not_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_result(
            passed, stack_trace, message, actual_value, actual_value_text, expected_value, expected_value_text,
            "assertResultStrictNotEquals", "!==",
        )
    }

    pub fn new_translation_value_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        translation_key: &str,
        translation_value: Option<&str>,
        expected_value: &str,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultTranslationValueEquals",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{translation_key}\": {}",
                if let Some(translation_value) = translation_value {
                    format!("\"{translation_value}\"")
                }else {
                    "Translation key not found".to_string()
                },
            ))),
            expected_value: Some(Box::from(&*format!(
                "== \"{expected_value}\"",
            ))),
        }
    }

    pub fn new_translation_value_not_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        translation_key: &str,
        translation_value: Option<&str>,
        expected_value: &str,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultTranslationValueNotEquals",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{translation_key}\": {}",
                if let Some(translation_value) = translation_value {
                    format!("\"{translation_value}\"")
                }else {
                    "Translation key not found".to_string()
                },
            ))),
            expected_value: Some(Box::from(&*format!(
                "!= \"{expected_value}\"",
            ))),
        }
    }

    pub fn new_translation_key_found_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        translation_key: &str,
        translation_value: Option<&str>,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultTranslationKeyFound",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{translation_key}\": {}",
                if translation_value.is_some() {
                    "Translation key found"
                }else {
                    "Translation key not found"
                },
            ))),
            expected_value: Some(Box::from("== Translation key found")),
        }
    }

    pub fn new_translation_key_not_found_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        translation_key: &str,
        translation_value: Option<&str>,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultTranslationKeyNotFound",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{translation_key}\": {}",
                if translation_value.is_some() {
                    "Translation key found"
                }else {
                    "Translation key not found"
                },
            ))),
            expected_value: Some(Box::from("== Translation key not found")),
        }
    }

    fn new_generic_data_object_string_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_value: &str,
        assert_test_name: &'static str,
    ) -> Self {
        Self {
            passed,
            assert_test_name,
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(Box::from(&*format!(
                "\"{}\", Type: {:?}",
                actual_value_text,
                actual_value.data_type(),
            ))),
            expected_value: Some(Box::from(expected_value)),
        }
    }

    pub fn new_type_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_type: DataType,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            &format!(
                "Type: == {expected_type:?}"
            ),
            "assertResultTypeEquals",
        )
    }

    pub fn new_type_not_equals_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
        expected_type: DataType,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            &format!(
                "Type: != {expected_type:?}"
            ),
            "assertResultTypeNotEquals",
        )
    }

    pub fn new_null_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "== null",
            "assertResultNull",
        )
    }

    pub fn new_not_null_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "!= null",
            "assertResultNotNull",
        )
    }

    pub fn new_void_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "== void",
            "assertResultVoid",
        )
    }

    pub fn new_not_void_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "!= void",
            "assertResultNotVoid",
        )
    }

    pub fn new_final_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "== final",
            "assertResultFinal",
        )
    }

    pub fn new_not_final_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "!= final",
            "assertResultNotFinal",
        )
    }

    pub fn new_static_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "== static",
            "assertResultStatic",
        )
    }

    pub fn new_not_static_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: &DataObject,
        actual_value_text: &str,
    ) -> Self {
        Self::new_generic_data_object_string_result(
            passed, stack_trace, message, actual_value, actual_value_text,
            "!= static",
            "assertResultNotStatic",
        )
    }

    pub fn new_throw_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: Option<InterpretingError>,
        expected_value: InterpretingError,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertThrow",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(if let Some(actual_value) = actual_value {
                Box::from(&*format!("{} ({})", actual_value.error_code(), actual_value.name()))
            }else {
                Box::from("nothing thrown")
            }),
            expected_value: Some(Box::from(&*format!(
                "== {} ({})", expected_value.error_code(), expected_value.name(),
            ))),
        }
    }

    pub fn new_return_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: Option<(&DataObjectRef, Box<str>)>,
        expected_value: &DataObject,
        expected_value_text: &str,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultReturn",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(actual_value.map(|(value, text)| Box::from(&*format!(
                "\"{text}\", Type: {:?}",
                value.data_type(),
            ))).unwrap_or_else(|| Box::from("nothing returned"))),
            expected_value: Some(Box::from(&*format!(
                "=== \"{}\", Type: {:?}",
                expected_value_text,
                expected_value.data_type(),
            ))),
        }
    }

    pub fn new_no_return_result(
        passed: bool,
        stack_trace: Option<&str>,
        message: Option<&str>,
        actual_value: Option<(&DataObjectRef, Box<str>)>,
    ) -> Self {
        Self {
            passed,
            assert_test_name: "assertResultNoReturn",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: Some(actual_value.map(|(value, text)| Box::from(&*format!(
                "\"{text}\", Type: {:?}",
                value.data_type(),
            ))).unwrap_or_else(|| Box::from("nothing returned"))),
            expected_value: Some(Box::from("== nothing returned")),
        }
    }

    pub fn new_fail_result(
        stack_trace: Option<&str>,
        message: Option<&str>,
    ) -> Self {
        Self {
            passed: false,
            assert_test_name: "assertResultFail",
            stack_trace: stack_trace.map(Box::from),
            message: message.map(Box::from),
            actual_value: None,
            expected_value: None,
        }
    }

    pub fn passed(&self) -> bool {
        self.passed
    }

    pub fn assert_test_name(&self) -> &str {
        self.assert_test_name
    }

    pub fn stack_trace(&self) -> Option<&str> {
        self.stack_trace.as_deref()
    }

    pub fn message(&self) -> Option<&str> {
        self.message.as_deref()
    }

    pub fn actual_value(&self) -> Option<&str> {
        self.actual_value.as_deref()
    }

    pub fn expected_value(&self) -> Option<&str> {
        self.expected_value.as_deref()
    }
}
