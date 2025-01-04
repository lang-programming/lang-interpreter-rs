use crate::utils::math::*;

#[test]
#[should_panic(expected = "Radix must be between 2 and 36")]
fn to_number_base_i32_base_too_low() {
    42_i32.to_number_base(1);
}

#[test]
#[should_panic(expected = "Radix must be between 2 and 36")]
fn to_number_base_i64_base_too_low() {
    42_i64.to_number_base(1);
}

#[test]
#[should_panic(expected = "Radix must be between 2 and 36")]
fn to_number_base_i32_base_too_high() {
    42_i32.to_number_base(37);
}

#[test]
#[should_panic(expected = "Radix must be between 2 and 36")]
fn to_number_base_i64_base_too_high() {
    42_i64.to_number_base(37);
}

#[test]
fn to_number_base_i32_min_value() {
    assert_eq!(
        i32::MIN.to_number_base(2),
        "-10000000000000000000000000000000",
    );

    assert_eq!(
        i32::MIN.to_number_base(10),
        "-2147483648",
    );

    assert_eq!(
        i32::MIN.to_number_base(16),
        "-80000000",
    );

    assert_eq!(
        i32::MIN.to_number_base(32),
        "-2000000",
    );

    assert_eq!(
        i32::MIN.to_number_base(36),
        "-zik0zk",
    );
}

#[test]
fn to_number_base_i64_min_value() {
    assert_eq!(
        i64::MIN.to_number_base(2),
        "-1000000000000000000000000000000000000000000000000000000000000000",
    );

    assert_eq!(
        i64::MIN.to_number_base(10),
        "-9223372036854775808",
    );

    assert_eq!(
        i64::MIN.to_number_base(16),
        "-8000000000000000",
    );

    assert_eq!(
        i64::MIN.to_number_base(32),
        "-8000000000000",
    );

    assert_eq!(
        i64::MIN.to_number_base(36),
        "-1y2p0ij32e8e8",
    );
}

#[test]
fn to_number_base_i32_max_value() {
    assert_eq!(
        i32::MAX.to_number_base(2),
        "1111111111111111111111111111111",
    );

    assert_eq!(
        i32::MAX.to_number_base(10),
        "2147483647",
    );

    assert_eq!(
        i32::MAX.to_number_base(16),
        "7fffffff",
    );

    assert_eq!(
        i32::MAX.to_number_base(32),
        "1vvvvvv",
    );

    assert_eq!(
        i32::MAX.to_number_base(36),
        "zik0zj",
    );
}

#[test]
fn to_number_base_i64_max_value() {
    assert_eq!(
        i64::MAX.to_number_base(2),
        "111111111111111111111111111111111111111111111111111111111111111",
    );

    assert_eq!(
        i64::MAX.to_number_base(10),
        "9223372036854775807",
    );

    assert_eq!(
        i64::MAX.to_number_base(16),
        "7fffffffffffffff",
    );

    assert_eq!(
        i64::MAX.to_number_base(32),
        "7vvvvvvvvvvvv",
    );

    assert_eq!(
        i64::MAX.to_number_base(36),
        "1y2p0ij32e8e7",
    );
}

#[test]
fn to_number_base_positive_value() {
    assert_eq!(
        42_i32.to_number_base(2),
        "101010",
    );

    assert_eq!(
        42_i32.to_number_base(3),
        "1120",
    );

    assert_eq!(
        42_i32.to_number_base(7),
        "60",
    );

    assert_eq!(
        42_i32.to_number_base(10),
        "42",
    );

    assert_eq!(
        42_i32.to_number_base(16),
        "2a",
    );

    assert_eq!(
        42_i32.to_number_base(23),
        "1j",
    );

    assert_eq!(
        42_i32.to_number_base(32),
        "1a",
    );

    assert_eq!(
        42_i32.to_number_base(36),
        "16",
    );
}

#[test]
fn to_number_base_negative_value() {
    assert_eq!(
        (-42_i32).to_number_base(2),
        "-101010",
    );

    assert_eq!(
        (-42_i32).to_number_base(3),
        "-1120",
    );

    assert_eq!(
        (-42_i32).to_number_base(7),
        "-60",
    );

    assert_eq!(
        (-42_i32).to_number_base(10),
        "-42",
    );

    assert_eq!(
        (-42_i32).to_number_base(16),
        "-2a",
    );

    assert_eq!(
        (-42_i32).to_number_base(23),
        "-1j",
    );

    assert_eq!(
        (-42_i32).to_number_base(32),
        "-1a",
    );

    assert_eq!(
        (-42_i32).to_number_base(36),
        "-16",
    );
}

#[test]
fn to_number_base_positive_value_radix_minus_1() {
    assert_eq!(
        1.to_number_base(2),
        "1",
    );

    assert_eq!(
        15.to_number_base(16),
        "f",
    );

    assert_eq!(
        35.to_number_base(36),
        "z",
    );

    const DIGITS: &[u8] = b"123456789abcdefghijklmnopqrstuvwxyz";

    for (i, d) in DIGITS.iter().
            enumerate() {
        assert_eq!(
            (i as i32 + 1).to_number_base(i as u32 + 2),
            format!("{}", *d as char),
        );
    }
}

#[test]
fn to_number_base_positive_value_0() {
    assert_eq!(
        0.to_number_base(2),
        "0",
    );

    assert_eq!(
        0.to_number_base(16),
        "0",
    );

    assert_eq!(
        0.to_number_base(36),
        "0",
    );

    for i in 2..=36 {
        assert_eq!(
            0.to_number_base(i as u32),
            "0",
        );
    }
}

#[test]
fn to_number_base_positive_value_radix() {
    assert_eq!(
        2.to_number_base(2),
        "10",
    );

    assert_eq!(
        16.to_number_base(16),
        "10",
    );

    assert_eq!(
        36.to_number_base(36),
        "10",
    );

    for i in 2..=36 {
        assert_eq!(
            i.to_number_base(i as u32),
            "10",
        );
    }
}

#[test]
fn to_number_base_positive_value_2_times_radix_minus_1() {
    assert_eq!(
        3.to_number_base(2),
        "11",
    );

    assert_eq!(
        31.to_number_base(16),
        "1f",
    );

    assert_eq!(
        71.to_number_base(36),
        "1z",
    );

    const DIGITS: &[u8] = b"123456789abcdefghijklmnopqrstuvwxyz";

    for (i, d) in DIGITS.iter().
            enumerate() {
        assert_eq!(
            (i as i32 * 2 + 3).to_number_base(i as u32 + 2),
            format!("1{}", *d as char),
        );
    }
}

#[test]
fn to_number_base_positive_value_neg_2_times_radix_plus_1() {
    assert_eq!(
        (-3).to_number_base(2),
        "-11",
    );

    assert_eq!(
        (-31).to_number_base(16),
        "-1f",
    );

    assert_eq!(
        (-71).to_number_base(36),
        "-1z",
    );

    const DIGITS: &[u8] = b"123456789abcdefghijklmnopqrstuvwxyz";

    for (i, d) in DIGITS.iter().
            enumerate() {
        assert_eq!(
            (-(i as i32 * 2 + 3)).to_number_base(i as u32 + 2),
            format!("-1{}", *d as char),
        );
    }
}
