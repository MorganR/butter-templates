use crate::semantics::Package;

/// A translator takes package data and translates it to another language.
pub trait Translator {
    /// Translate the package into text, ready to save to a file.
    fn translate(pkg: &Package) -> String;
}

/// Converts the given string (expected to be in camelCase) to snake_case.
pub fn to_snake_case(s: &str) -> String {
    let mut len = s.len();
    for (i, r) in s.char_indices() {
        if i > 0 && (r >= 'A' && r <= 'Z') {
            len += 1;
        }
    }
    let mut result = String::with_capacity(len);
    for (i, r) in s.char_indices() {
        if i > 0 && (r >= 'A' && r <= 'Z') {
            result.push('_');
        }
        result.push(r);
    }
    result
}
