use pest::iterators::{Pair, Pairs};
use std::collections::HashMap;
use std::convert::AsRef;
use std::result::Result;
use std::{fmt, rc::Rc};
use strum::IntoEnumIterator;
use strum_macros::{AsRefStr, EnumIter};

use crate::parser::Rule;

macro_rules! process_pair {
    ($pair:ident, $($expected_rule:tt, $process:tt),+) => {
        match $pair.as_rule() {
            $($expected_rule => $process),+
            _ => return Err(Error::unexpected_token_type(&$pair, vec![$($expected_rule),+])),
        }
    };
    ($pair:ident, $($expected_rule:tt, $process:tt,)+) => {
        match $pair.as_rule() {
            $($expected_rule => $process,)+
            _ => return Err(Error::unexpected_token_type(&$pair, vec![$($expected_rule),+])),
        }
    };
}

macro_rules! validate_context {
    ($ctx:ident, $pair:ident, $($expected_context:pat, $expected_context_type:expr),+) => {
        match $ctx.peek_semantic_context() {
            $(Some($expected_context) => {}),+
            Some(sc) => return Err(Error::InvalidSemanticContext(Some(sc.clone()), vec![$($expected_context_type,)*], $pair.into())),
            None => return Err(Error::InvalidSemanticContext(None, vec![$($expected_context_type,)*], $pair.into())),
        }
    };
    ($ctx:ident, $pair:ident, $($expected_context:pat, $expected_context_type:expr,)+) => {
        match $ctx.peek_semantic_context() {
            $(Some($expected_context) => {}),+
            Some(sc) => return Err(Error::InvalidSemanticContext(Some(sc.clone()), vec![$($expected_context_type,)*], $pair.into())),
            None => return Err(Error::InvalidSemanticContext(None, vec![$($expected_context_type,)*], $pair.into())),
        }
    };
}

macro_rules! assert_no_more_pairs {
    ($pairs:expr) => {
        if let Some(pair) = $pairs.next() {
            return Err(Error::unexpected_token_type(&pair, vec![]));
        }
    };
}

/// Primitive types for variables and arguments.
#[derive(Debug, Copy, Clone, PartialEq)]
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
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Kind {
    Primitive(PrimitiveKind),
    /// Sanitized HTML content.
    HTML,
    /// A sanitized HTML attribute (eg. id="1").
    HTMLAttr,
    /// A sanitized HTML attribute value, that sits inside the quotation marks (eg. true).
    HTMLAttrValue,
}

impl FromPair for Kind {
    /// Creates a [BlockKind] from `Rule::block_type`.
    fn from_pair<'a>(pair: Pair<'a, Rule>) -> Result<Self, Error> {
        process_pair!(pair, (Rule::block_type), {
            return match pair.as_str() {
                "html" => Ok(Kind::HTML),
                other => Err(Error::unexpected_token_value(&pair, other)),
            };
        });
    }
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(Rc<str>),
    Bool(bool),
    // TODO: Add protos?
}

/// An expression.
#[derive(Debug)]
pub enum Expression {
    /// A call to a block with expressions (i.e. arguments), for example {call()}.
    Call(Rc<str>, Vec<Expression>, Kind),
    /// A call to a block with expressions (i.e. arguments), for example {call()}.
    Variable(Rc<str>, Kind),
    /// A literal, eg. "text".
    Literal(Literal),
}

/// Defines a variable and its type.
#[derive(Debug, Clone)]
pub struct Variable {
    /// The name of the variable.
    name: Rc<str>,
    /// The type of data in the variable.
    kind: Kind,
}

#[derive(Debug, Clone)]
pub struct BlockMetadata {
    /// The arguments of this block.
    pub arguments: Vec<Variable>,
    /// The kind of value returned by this block.
    pub kind: Kind,
}

#[derive(Debug, Clone)]
pub enum Entity {
    Block(Rc<BlockMetadata>),
    Variable(Kind),
}

#[derive(Debug, Clone)]
/// A semantically meaningful context, such as being inside an HTML element, that doesn't change the scope.
pub enum SemanticContext {
    /// Package scope: can import things and define templates.
    Package,
    /// Writing HTML.
    HTML,
    /// Inside an HTML tag (i.e. within the <> brackets), along with the tag type.
    HTMLTag(Rc<str>),
    /// Inside the value of an HTML attribute, along with the attribute key.
    HTMLAttribute(Rc<str>),
}

#[derive(Debug)]
pub enum AttrValue {
    Expression(Expression),
    Text(Rc<str>),
}

impl AttrValue {
    fn from_pair<'a>(pair: &Pair<'a, Rule>, ctx: &mut Context) -> Result<Self, Error> {
        validate_context!(
            ctx,
            pair,
            SemanticContext::HTMLAttribute(_),
            SemanticContext::HTMLAttribute(Rc::from("")),
        );
        process_pair!(
            pair,
            (Rule::inner_statement),
            {
                // TODO: handle this properly.
                return Err(Error::unexpected_token_type(
                    pair,
                    vec![Rule::html_attribute_plain_value],
                ));
            },
            (Rule::html_attribute_plain_value),
            {
                // The value is already escaped.
                return Ok(Self::Text(Rc::from(pair.as_str())));
            }
        );
    }
}

#[derive(Debug)]
pub struct AttrKeyValue {
    key: Rc<str>,
    value: Vec<AttrValue>,
}

impl AttrKeyValue {
    fn from_pairs<'a>(
        parent: &Pair<'a, Rule>,
        mut pairs: Pairs<'a, Rule>,
        ctx: &mut Context,
    ) -> Result<Self, Error> {
        let key_pair = pairs.next().ok_or(Error::missing_pair(&parent))?;
        let key = process_pair!(
            key_pair,
            (Rule::inner_statement),
            {
                // Shouldn't happen.
                return Err(Error::unexpected_token_type(
                    &key_pair,
                    vec![Rule::html_attribute_key],
                ));
            },
            (Rule::html_attribute_key),
            { Rc::from(key_pair.as_str()) },
        );
        ctx.push_semantic_context(SemanticContext::HTMLAttribute(Rc::clone(&key)));
        let mut values = vec![];
        while let Some(value_pair) = pairs.next() {
            // TODO: possibly more validation here for characters that need to be escaped.
            values.push(AttrValue::from_pair(&value_pair, ctx)?);
        }
        ctx.pop_semantic_context();
        return Ok(AttrKeyValue {
            key: key,
            value: values,
        });
    }
}

#[derive(Debug)]
pub enum Attr {
    Expression(Expression),
    KeyValue(AttrKeyValue),
}

impl Attr {
    fn from_pair<'a>(pair: Pair<'a, Rule>, ctx: &mut Context) -> Result<Self, Error> {
        let pair_ref = &pair;
        validate_context!(
            ctx,
            pair_ref,
            SemanticContext::HTMLTag(_),
            SemanticContext::HTMLTag(Rc::from("")),
        );
        let attr_pairs =
            process_pair!(pair, (Rule::html_attribute), { pair.clone().into_inner() },);
        let first_pair = attr_pairs
            .clone()
            .next()
            .ok_or(Error::missing_pair(&pair))?;
        process_pair!(
            first_pair,
            (Rule::inner_statement),
            {
                // TODO: handle this properly.
                return Err(Error::unexpected_token_type(
                    &pair,
                    vec![Rule::html_attribute],
                ));
            },
            (Rule::html_attribute_key),
            {
                return Ok(Attr::KeyValue(AttrKeyValue::from_pairs(
                    &first_pair,
                    attr_pairs,
                    ctx,
                )?));
            },
        );
    }
}

#[derive(Debug)]
pub struct TagData {
    /// The name/type of the tag (eg. h1 or p).
    name: Rc<str>,
    /// The attributes on the tag.
    attributes: Vec<Attr>,
}

#[derive(Debug, AsRefStr, EnumIter)]
#[strum(serialize_all = "lowercase")]
/// Void tags, defined at https://developer.mozilla.org/en-US/docs/Glossary/Void_element.
pub enum VoidTagName {
    Area,
    Base,
    Br,
    Col,
    Embed,
    Hr,
    Img,
    Input,
    Keygen,
    Link,
    Meta,
    Param,
    Source,
    Track,
    Wbr,
}

impl VoidTagName {
    fn try_from_tag_name(tag: &str) -> Option<Self> {
        for vTag in Self::iter() {
            if tag.eq_ignore_ascii_case(vTag.as_ref()) {
                return Some(vTag);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct VoidTag {
    name: VoidTagName,
    attributes: Vec<Attr>,
}

impl VoidTag {
    fn from_pairs<'a>(
        parent: &Pair<'a, Rule>,
        mut pairs: Pairs<'a, Rule>,
        ctx: &mut Context,
    ) -> Result<Self, Error> {
        validate_context!(ctx, parent, SemanticContext::HTML, SemanticContext::HTML);
        let tag_pair = pairs.next().ok_or(Error::missing_pair(parent))?;
        let tag_name = process_pair!(tag_pair, (Rule::identifier), { tag_pair.as_str() });
        let void_tag = match VoidTagName::try_from_tag_name(tag_name) {
            Some(void_tag) => void_tag,
            _ => return Err(Error::unexpected_token_value(&tag_pair, tag_name)),
        };
        ctx.push_semantic_context(SemanticContext::HTMLTag(Rc::from(tag_name)));
        let mut attributes = vec![];
        while let Some(attr_pair) = pairs.next() {
            match Attr::from_pair(attr_pair, ctx) {
                Err(error) => ctx.add_error(error),
                Ok(attr) => attributes.push(attr),
            }
        }
        ctx.pop_semantic_context();
        return Ok(VoidTag {
            name: void_tag,
            attributes: attributes,
        });
    }
}

#[derive(Debug)]
pub struct OpenCloseTagName(Rc<str>);

#[derive(Debug)]
pub struct OpenTag {
    name: OpenCloseTagName,
    attributes: Vec<Attr>,
}

impl OpenTag {
    fn from_pairs<'a>(
        parent: &Pair<'a, Rule>,
        mut pairs: Pairs<'a, Rule>,
        ctx: &mut Context,
    ) -> Result<Self, Error> {
        validate_context!(ctx, parent, SemanticContext::HTML, SemanticContext::HTML);
        let tag_pair = pairs.next().ok_or(Error::missing_pair(parent))?;
        let tag_name = Rc::from(process_pair!(tag_pair, (Rule::identifier), {
            tag_pair.as_str()
        },));
        ctx.push_semantic_context(SemanticContext::HTMLTag(Rc::clone(&tag_name)));
        let mut attributes = vec![];
        while let Some(attr_pair) = pairs.next() {
            match Attr::from_pair(attr_pair, ctx) {
                Err(error) => ctx.add_error(error),
                Ok(attr) => attributes.push(attr),
            }
        }
        ctx.pop_semantic_context();
        return Ok(OpenTag {
            name: OpenCloseTagName(tag_name),
            attributes: attributes,
        });
    }
}

#[derive(Debug)]
pub enum HTMLTag {
    /// A void tag, which can't have any content.
    Void(VoidTag),
    /// An open tag, eg. <h1>, which can have content, and normally has a closing tag.
    Open(OpenTag),
    /// A closing tag, eg. </h1>.
    Close(OpenCloseTagName),
}

pub struct HTMLTokenizer;

impl Tokenizer for HTMLTokenizer {
    fn tokenize_into_context<'a>(pair: Pair<'a, Rule>, ctx: &mut Context) -> Result<(), Error> {
        let pair_ref = &pair;
        validate_context!(ctx, pair_ref, SemanticContext::HTML, SemanticContext::HTML);

        // TODO: Process inner_statement
        process_pair!(
            pair,
            (Rule::html_tag_void),
            {
                let tag = match VoidTag::from_pairs(&pair, pair.clone().into_inner(), ctx) {
                    Ok(vt) => vt,
                    Err(error) => return Err(error),
                };
                ctx.push_token(Token::HTMLTag(HTMLTag::Void(tag)));
            },
            (Rule::html_tag_open_or_void),
            {
                let tag = match VoidTag::from_pairs(&pair, pair.clone().into_inner(), ctx) {
                    Ok(vt) => HTMLTag::Void(vt),
                    Err(_) => match OpenTag::from_pairs(&pair, pair.clone().into_inner(), ctx) {
                        Ok(ot) => HTMLTag::Open(ot),
                        Err(ot_error) => return Err(ot_error),
                    },
                };
                ctx.push_token(Token::HTMLTag(tag));
            },
            (Rule::html_tag_close),
            {
                let mut tag_pairs = pair.clone().into_inner();
                let tag_pair = tag_pairs.next().ok_or(Error::missing_pair(&pair))?;
                let tag_name = process_pair!(tag_pair, (Rule::identifier), { tag_pair.as_str() },);
                ctx.push_token(Token::HTMLTag(HTMLTag::Close(OpenCloseTagName(Rc::from(
                    tag_name,
                )))));
                assert_no_more_pairs!(tag_pairs);
            },
            (Rule::html_text),
            {
                // TODO: possibly more validation here for characters that need to be escaped.
                ctx.push_token(Token::HTMLText(Rc::from(pair.as_str())));
            },
        );
        Ok(())
    }
}

/// Core block contents, such as HTML elements, raw text, and other expressions.
#[derive(Debug)]
pub enum Token {
    /// An expression.
    Expression(Expression),
    /// An HTML tag open/void/close.
    HTMLTag(HTMLTag),
    /// HTML-safe text.
    HTMLText(Rc<str>),
    /// Generic text.
    Text(Rc<str>),
}

/// Defines identifiers in a given scope.
#[derive(Debug, Clone)]
pub struct Scope(HashMap<Rc<str>, Entity>);

impl Scope {
    /// Creates a new empty scope.
    pub fn new() -> Scope {
        return Scope(HashMap::new());
    }

    /// Tries to push an entity onto the scope. Fails if the id already exists in this scope.
    pub fn push<'a>(
        &mut self,
        id: Rc<str>,
        entity: Entity,
        pair: &Pair<'a, Rule>,
    ) -> Result<(), Error> {
        if self.0.contains_key(&id) {
            return Err(Error::DuplicateIdentifier(id, pair.into()));
        }
        self.0.insert(id, entity);
        Ok(())
    }
}

#[derive(Debug)]
pub struct Context {
    scopes: Vec<Scope>,
    semantic_contexts: Vec<SemanticContext>,
    outputs: Vec<Token>,
    errors: Vec<Error>,
}

impl Context {
    /// Constructs a new Context with the given initial scopes and semantic context.
    pub fn new(initial_scopes: Vec<Scope>, semantic_context: SemanticContext) -> Self {
        Context {
            scopes: initial_scopes.clone(),
            semantic_contexts: vec![semantic_context],
            outputs: vec![],
            errors: vec![],
        }
    }

    /// Looks for an entity in the current scope with the given id.
    pub fn get_entity(&self, id: &str) -> Option<&Entity> {
        self.scopes.iter().rev().find_map(|scope| scope.0.get(id))
    }

    /// Peeks the current semantic context.
    pub fn peek_semantic_context(&self) -> Option<&SemanticContext> {
        self.semantic_contexts.last()
    }

    /// Pushes a new semantic context.
    pub fn push_semantic_context(&mut self, sc: SemanticContext) {
        self.semantic_contexts.push(sc);
    }

    /// Pops the most recent semantic context, and returns it.
    pub fn pop_semantic_context(&mut self) -> Option<SemanticContext> {
        self.semantic_contexts.pop()
    }

    /// Pushes the given token onto the context.
    pub fn push_token(&mut self, token: Token) {
        self.outputs.push(token);
    }

    /// Adds an error to the context.
    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    /// Gets the tokens and destroys the context.
    pub fn take_tokens_and_destroy(self) -> Vec<Token> {
        self.outputs
    }
}

/// A data type that can be created from a parsed token pair.
pub trait FromPair
where
    Self: Sized,
{
    /// Converts the given pair into this data type.
    fn from_pair<'a>(pair: Pair<'a, Rule>) -> Result<Self, Error>;
}

/// Analyzes some set of parse tokens within the current context, and converts them to output tokens.
pub trait Tokenizer
where
    Self: Sized,
{
    /// Converts the given pair into tokens matching this data type, and adds them (and possibly their children) to the current context.
    fn tokenize_into_context<'a>(pair: Pair<'a, Rule>, ctx: &mut Context) -> Result<(), Error>;
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
pub struct LinePosition(LineCol, Rc<str>);

impl fmt::Display for LinePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}: {}", self.0, self.1);
    }
}

impl<'a, 'b> From<&'a Pair<'b, Rule>> for LinePosition
where
    'b: 'a,
{
    fn from(value: &'a Pair<'b, Rule>) -> Self {
        let loc = value.as_span().start_pos();
        return LinePosition(loc.line_col().into(), Rc::from(loc.line_of()));
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    /// The first line was not a package declaration.
    MissingPackage,
    /// An unexpected token `Rule` was received at line `LinePosition`, when rules in `Vec<Rule>` were expected.
    UnexpectedTokenType(Rule, Vec<Rule>, LinePosition),
    /// An unexpected token value was received from `Rule` at line `LinePosition`.
    UnexpectedTokenValue(Rule, Rc<str>, LinePosition),
    /// A pair was missing where it was expected. Includes the parent rule, and its line position.
    MissingPair(Rule, LinePosition),
    /// An invalid semantic context was encountered. Includes the found context, expected contexts, and line position.
    InvalidSemanticContext(Option<SemanticContext>, Vec<SemanticContext>, LinePosition),
    /// A duplicate identifier was defined in the same scope.
    DuplicateIdentifier(Rc<str>, LinePosition),
}

impl Error {
    pub fn missing_pair<'a>(parent_pair: &Pair<'a, Rule>) -> Error {
        return Self::MissingPair(parent_pair.as_rule(), parent_pair.into());
    }

    pub fn unexpected_token_type<'a>(pair: &Pair<'a, Rule>, expected_rules: Vec<Rule>) -> Error {
        return Self::UnexpectedTokenType(pair.as_rule(), expected_rules, pair.into());
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
            Self::UnexpectedTokenType(found, expected, line) => write!(
                f,
                "received an unexpected token type {:?}; expected {:?} at line {}",
                found, expected, line
            ),
            Self::UnexpectedTokenValue(rule, found, line) => write!(
                f,
                "received an unexpected token value {} for rule {:?} at line {}",
                found, rule, line
            ),
            Self::InvalidSemanticContext(found, expected, line) => write!(
                f,
                "received an invalid semantic context {:?} (expected {:?}) at line {}",
                found, expected, line
            ),
            Self::DuplicateIdentifier(duplicate, line) => write!(
                f,
                "received an duplicate identifier within this scope {} at line {}",
                duplicate, line
            ),
        };
    }
}
