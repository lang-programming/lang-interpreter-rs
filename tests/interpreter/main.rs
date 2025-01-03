use std::env;
use std::fs::File;
use std::io::Read;
use lang_interpreter::interpreter::Interpreter;
use lang_interpreter::interpreter::platform::DefaultPlatformAPI;
use lang_interpreter::terminal_io::TerminalIO;

#[test]
fn lang_spec_test() {
    let mut current_dir = env::current_dir().unwrap();
    current_dir.push("tests");
    current_dir.push("interpreter");
    current_dir.push("spec-test");

    let mut interpreter = Interpreter::new(
        current_dir.to_str().unwrap(),
        Some("test.lang"),
        Some(TerminalIO::new(None).unwrap()),
        Box::new(DefaultPlatformAPI::new()),
        None,
    );

    let mut main_test_file = current_dir;
    main_test_file.push("test.lang");

    let mut main_test_file = File::open(main_test_file).unwrap();
    let mut lang_code = String::new();
    main_test_file.read_to_string(&mut lang_code).unwrap();

    interpreter.interpret_lines(lang_code);

    let test_count = interpreter.lang_test_store().test_count();
    let passed_test_count = interpreter.lang_test_store().test_passed_count();

    assert!(test_count > 0, "Lang spec tests where not initialized correctly");
    assert_eq!(
        test_count,
        passed_test_count,
        "Some Lang spec test failed: There are {test_count} tests but only {passed_test_count} tests passed.",
    );
}
