use std::env;
use std::fs::File;
use std::io::Read;
use lang_interpreter::interpreter::Interpreter;
use lang_interpreter::interpreter::platform::DefaultPlatformAPI;

#[test]
fn lang_spec_test() {
    let mut current_dir = env::current_dir().unwrap();
    current_dir.push("tests");
    current_dir.push("interpreter");
    current_dir.push("spec-test");

    let mut interpreter = Interpreter::new(
        current_dir.to_str().unwrap(),
        None,
        None,
        Box::new(DefaultPlatformAPI::new()),
        None,
    );

    let mut main_test_file = current_dir;
    main_test_file.push("test.lang");

    let mut main_test_file = File::open(main_test_file).unwrap();
    let mut lang_code = String::new();
    main_test_file.read_to_string(&mut lang_code).unwrap();

    interpreter.interpret_lines(lang_code);
    
    assert!(interpreter.lang_test_store().test_count() > 0);
    assert_eq!(interpreter.lang_test_store().test_count(), interpreter.lang_test_store().test_passed_count());
}
