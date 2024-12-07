use crate::lexer::*;

#[test]
fn is_null_value() {
    assert!(Lexer::is_null_value("null"));

    assert!(!Lexer::is_null_value(""));
    assert!(!Lexer::is_null_value("\0"));
    assert!(!Lexer::is_null_value("nul"));
    assert!(!Lexer::is_null_value("Null"));
    assert!(!Lexer::is_null_value("Nil"));
    assert!(!Lexer::is_null_value("nil"));
    assert!(!Lexer::is_null_value("void"));
}

#[test]
fn is_numeric_value_int() {
    assert!(Lexer::is_numeric_value("0"));
    assert!(Lexer::is_numeric_value("42"));
    assert!(Lexer::is_numeric_value("2147483647"));

    assert!(Lexer::is_numeric_value("027"));
}

#[test]
fn is_numeric_value_long() {
    assert!(Lexer::is_numeric_value("0L"));
    assert!(Lexer::is_numeric_value("1000000000000000"));
    assert!(Lexer::is_numeric_value("42L"));
    assert!(Lexer::is_numeric_value("1000000000000000l"));
    assert!(Lexer::is_numeric_value("9223372036854775807"));
}

#[test]
fn is_numeric_value_float() {
    assert!(Lexer::is_numeric_value("0f"));
    assert!(Lexer::is_numeric_value(".0f"));
    assert!(Lexer::is_numeric_value("0.f"));
    assert!(Lexer::is_numeric_value("0.0f"));
    assert!(Lexer::is_numeric_value("1f"));
    assert!(Lexer::is_numeric_value("1.f"));
    assert!(Lexer::is_numeric_value("1.F"));
    assert!(Lexer::is_numeric_value(".24f"));
    assert!(Lexer::is_numeric_value("24.f"));
    assert!(Lexer::is_numeric_value("1.e-12f"));
    assert!(Lexer::is_numeric_value("1.e+12f"));
    assert!(Lexer::is_numeric_value("1.e12f"));
    assert!(Lexer::is_numeric_value("1.E12f"));
}

#[test]
fn is_numeric_value_double() {
    assert!(Lexer::is_numeric_value(".0"));
    assert!(Lexer::is_numeric_value("0."));
    assert!(Lexer::is_numeric_value("0.0"));
    assert!(Lexer::is_numeric_value("1."));
    assert!(Lexer::is_numeric_value(".24"));
    assert!(Lexer::is_numeric_value("24."));
    assert!(Lexer::is_numeric_value("1.e-12"));
    assert!(Lexer::is_numeric_value("1.e+12"));
    assert!(Lexer::is_numeric_value("1.e12"));
    assert!(Lexer::is_numeric_value("1.E12"));

    assert!(Lexer::is_numeric_value("9223372036854775808"));
}

#[test]
fn is_numeric_value_invalid() {
    assert!(!Lexer::is_numeric_value(""));

    assert!(!Lexer::is_numeric_value("0b01"));
    assert!(!Lexer::is_numeric_value("0xA"));

    assert!(!Lexer::is_numeric_value("1i"));

    assert!(!Lexer::is_numeric_value("1d"));
    assert!(!Lexer::is_numeric_value("1.d"));
    assert!(!Lexer::is_numeric_value("1.0d"));
    assert!(!Lexer::is_numeric_value(".1d"));

    assert!(!Lexer::is_numeric_value("."));
    assert!(!Lexer::is_numeric_value(".f"));
    assert!(!Lexer::is_numeric_value("1.2.f"));
    assert!(!Lexer::is_numeric_value("1.2."));
    assert!(!Lexer::is_numeric_value("1.."));
    assert!(!Lexer::is_numeric_value(".e-12f"));
    assert!(!Lexer::is_numeric_value(".e-12"));
    assert!(!Lexer::is_numeric_value("1.e-1.2"));
    assert!(!Lexer::is_numeric_value("1.e-1.2f"));

    assert!(!Lexer::is_numeric_value("xyz"));
}
