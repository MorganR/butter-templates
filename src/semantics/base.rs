use pest::iterators::Pair;
use std::result::Result;
use std::{fmt, rc::Rc};

use crate::parser::Rule;

/// A data type that can be created from a parsed token pair.
pub trait FromPair
where
    Self: Sized,
{
    /// Converts the given pair into this data type.
    fn from_pair<'a>(pair: Pair<'a, Rule>) -> Result<Self, Error>;
}

macro_rules! process_pair {
    ($pair:ident, $($expected_rule:pat, $rule_type:expr, $process:expr),+) => {
        match $pair.as_rule() {
            $($expected_rule => $process),+
            _ => return Err(Error::unexpected_token(&$pair, vec![$($rule_type),+])),
        }
    };
    ($pair:ident, $($expected_rule:pat, $rule_type:expr, $process:expr,)+) => {
        match $pair.as_rule() {
            $($expected_rule => $process),+
            _ => return Err(Error::unexpected_token(&$pair, vec![$($rule_type),+])),
        }
    };
}

/// Primitive types for variables and arguments.
#[derive(Debug, Copy, Clone)]
pub enum PrimitiveKind {
    /// An integer (limited by Javascript to the range [-2^53 + 1, 2^53 - 1]).
    Int,
    /// A double-precision floating point value.
    Float,
    /// A string.
    String,
    /// A boolean.
    Bool,
}

/// Compound types for block returns, variables, and arguments.
#[derive(Debug, Copy, Clone)]
pub enum BlockKind {
    /// Sanitized HTML content.
    HTML,
}

impl FromPair for BlockKind {
    /// Creates a [BlockKind] from `Rule::block_type`.
    fn from_pair<'a>(pair: Pair<'a, Rule>) -> Result<Self, Error> {
        process_pair!(pair, Rule::block_type, Rule::block_type, {
            return match pair.as_str() {
                "html" => Ok(BlockKind::HTML),
                other => Err(Error::unexpected_token_value(&pair, other)),
            };
        });
    }
}

/// A literal value.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(Rc<str>),
    Bool(bool),
    // TODO: Add protos?
}

/// Defines the type of a variable.
#[derive(Debug, Clone)]
pub enum VariableKind {
    Primitive(PrimitiveKind),
    Block(BlockKind),
}

/// An expression.
#[derive(Debug)]
pub enum Expression {
    /// A call to a block with expressions (i.e. arguments), for example {call()}.
    Call(Rc<str>, Vec<Expression>, BlockKind),
    /// A call to a block with expressions (i.e. arguments), for example {call()}.
    Variable(Rc<str>, VariableKind),
    /// A literal, eg. "text".
    Literal(Literal),
}

/// The 1-based line and column number of a position in the input text.
#[derive(Debug, Clone)]
struct LineCol(usize, usize);

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "(line {}, col {})", self.0, self.1);
    }
}

impl From<(usize, usize)> for LineCol {
    fn from(value: (usize, usize)) -> Self {
        let (line, col) = value;
        return LineCol(line, col);
    }
}

/// A position in a line, with the text of the whole line.
#[derive(Debug, Clone)]
struct LinePosition(LineCol, Rc<str>);

impl fmt::Display for LinePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}: {}", self.0, self.1);
    }
}

impl From<pest::Position<'_>> for LinePosition {
    fn from(value: pest::Position) -> Self {
        return LinePosition(value.line_col().into(), Rc::from(value.line_of()));
    }
}

impl From<&'_ pest::iterators::Pair<'_, Rule>> for LinePosition {
    fn from(value: &pest::iterators::Pair<'_, Rule>) -> Self {
        let loc = value.as_span().start_pos();
        return loc.into();
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    /// The first line was not a package declaration.
    MissingPackage,
    /// An unexpected token `Rule` was received at line `LinePosition`, when rules in `Vec<Rule>` were expected.
    UnexpectedToken(Rule, Vec<Rule>, LinePosition),
    /// An unexpected token value was received from `Rule` at line `LinePosition`.
    UnexpectedTokenValue(Rule, Rc<str>, LinePosition),
    /// A pair was missing where it was expected. Includes the parent rule, and its line position.
    MissingPair(Rule, LinePosition),
    /// The block type is not valid.
    InvalidBlockType(Rc<str>),
}

impl Error {
    pub fn missing_pair<'a>(parent_pair: &Pair<'a, Rule>) -> Error {
        return Self::MissingPair(parent_pair.as_rule(), parent_pair.into());
    }

    pub fn unexpected_token<'a>(pair: &Pair<'a, Rule>, expected_rules: Vec<Rule>) -> Error {
        return Self::UnexpectedToken(pair.as_rule(), expected_rules, pair.into());
    }

    pub fn unexpected_token_value<'a>(pair: &Pair<'a, Rule>, value: &'a str) -> Error {
        return Self::UnexpectedTokenValue(pair.as_rule(), Rc::from(value), pair.into());
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            Self::MissingPackage => {
                write!(f, "template file must start with a package declaration")
            }
            Self::MissingPair(parent, line) => write!(
                f,
                "expected statement was missing; parent rule: {:?}, line: {}",
                parent, line
            ),
            Self::UnexpectedToken(found, expected, line) => write!(
                f,
                "received an unexpected token type {:?}; expected {:?} at line {}",
                found, expected, line
            ),
            Self::UnexpectedTokenValue(rule, found, line) => write!(
                f,
                "received an unexpected token value {} for rule {:?} at line {}",
                found, rule, line
            ),
            Self::InvalidBlockType(found) => write!(f, "received an invalid block type {}", found),
        };
    }
}
