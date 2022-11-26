use std::collections::{HashSet, LinkedList};
use std::fmt::Display;
use std::rc::Rc;

use itertools::Itertools;

use crate::semantics::{
    Block, Expression, HTMLAttr, HTMLAttrValue, HTMLTag, Kind, Package, PrimitiveKind, Token,
};
use crate::translation::{to_snake_case, Translator};

pub struct Rust;

impl Translator for Rust {
    fn translate(pkg: &Package) -> String {
        let mut context = Context::new();

        for block in &pkg.blocks {
            append_block_define(&block, &mut context);
        }
        prepend_imports(&mut context);

        return context.into_string();
    }
}

static BUTTER_LIB: &str = "butter";

struct Context {
    tab_size: isize,
    imports: HashSet<&'static str>,
    lines: LinkedList<String>,
}

impl Context {
    fn new() -> Self {
        Context {
            tab_size: 0,
            imports: HashSet::new(),
            lines: LinkedList::new(),
        }
    }

    fn add_line(&mut self, line: &str) {
        self.lines.push_back(with_tabs(self.tab_size, line));
    }

    fn prepend_line(&mut self, line: String) {
        // We assume no tabs at the front.
        self.lines.push_front(line);
    }

    fn into_string(self) -> String {
        self.lines.into_iter().join("\n")
    }
}

static TAB_SPACES: &str = "    ";

fn with_tabs(num_tabs: isize, s: &str) -> String {
    str::repeat(TAB_SPACES, num_tabs.try_into().unwrap()) + s
}

#[derive(Debug, PartialEq)]
enum RustPrimitive {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
enum RustToken {
    Primitive(RustPrimitive),
    Expression(String),
    Format(String, Vec<RustToken>),
}

impl RustToken {
    /// Merge two rust tokens for printing as output.
    fn merge_for_printing(self, other: RustToken) -> RustToken {
        match self {
            RustToken::Primitive(RustPrimitive::Int(_))
            | RustToken::Primitive(RustPrimitive::Float(_))
            | RustToken::Primitive(RustPrimitive::Bool(_))
            | RustToken::Expression(_) => match other {
                RustToken::Primitive(RustPrimitive::String(txt)) => {
                    RustToken::Format("{}".to_owned() + &txt, vec![self])
                }
                RustToken::Format(frmt_str, mut toks) => {
                    let mut tokens = Vec::with_capacity(toks.len() + 1);
                    tokens.push(self);
                    tokens.append(&mut toks);
                    RustToken::Format("{}".to_owned() + &frmt_str, tokens)
                }
                _ => RustToken::Format("{}{}".to_owned(), vec![self, other]),
            },
            RustToken::Primitive(RustPrimitive::String(ref txt)) => match other {
                RustToken::Primitive(RustPrimitive::String(other_text)) => {
                    RustToken::Primitive(RustPrimitive::String(format!("{txt}{other_text}")))
                }
                RustToken::Format(frmt_str, toks) => {
                    RustToken::Format(format!("{txt}{frmt_str}"), toks)
                }
                _ => RustToken::Format("{}{}".to_owned(), vec![self, other]),
            },
            RustToken::Format(frmt_str, mut toks) => match other {
                RustToken::Primitive(RustPrimitive::String(txt)) => {
                    RustToken::Format(frmt_str + &txt, toks)
                }
                RustToken::Format(other_frmt_str, mut other_toks) => {
                    toks.append(&mut other_toks);
                    RustToken::Format(frmt_str + &other_frmt_str, toks)
                }
                _ => {
                    toks.push(other);
                    RustToken::Format(frmt_str + "{}", toks)
                }
            },
        }
    }
}

impl Display for RustToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RustToken::Primitive(RustPrimitive::Int(i)) => {
                write!(f, "{}", i)
            }
            RustToken::Primitive(RustPrimitive::Float(flt)) => {
                write!(f, "{}", flt)
            }
            RustToken::Primitive(RustPrimitive::Bool(b)) => {
                write!(f, "{}", b)
            }
            RustToken::Primitive(RustPrimitive::String(s)) => {
                write!(f, "\"{}\"", s)
            }
            RustToken::Expression(_expr) => {
                // TODO: Do properly.
                write!(f, "(expr)")
            }
            RustToken::Format(frmt_str, toks) => {
                write!(f, "format!(\"{}\", {})", frmt_str, toks.iter().join(", "))
            }
        }
    }
}

fn prepend_imports(ctx: &mut Context) {
    // TODO: sort imports.
    ctx.prepend_line(format!(
        "use {}::{{{}}};\n",
        BUTTER_LIB,
        itertools::join(&ctx.imports, ", ")
    ));
}

fn append_block_define(define: &Block, ctx: &mut Context) {
    // TODO: Handle arguments.
    let block_def = format!(
        "pub fn {}() -> {} {{",
        to_snake_case(&define.name),
        kind_to_type(&define.metadata.kind, ctx)
    );
    ctx.add_line(block_def.as_ref());

    ctx.tab_size += 1;
    let rust_tokens: Vec<RustToken> = define
        .contents
        .iter()
        .map(|token| token_to_rust(token, ctx))
        .collect();
    append_tokens_as_sanitized_content(rust_tokens, &define.metadata.kind, ctx);
    ctx.tab_size -= 1;

    ctx.add_line("}\n");
}

fn kind_to_type(kind: &Kind, ctx: &mut Context) -> &'static str {
    match kind {
        Kind::Primitive(PrimitiveKind::Int) => "i64",
        Kind::Primitive(PrimitiveKind::Float) => "f64",
        Kind::Primitive(PrimitiveKind::String) => "String",
        Kind::Primitive(PrimitiveKind::Bool) => "bool",
        Kind::HTML => {
            ctx.imports.insert("SanitizedHTML");
            "SanitizedHTML"
        }
        Kind::HTMLAttr => {
            ctx.imports.insert("SanitizedHTML");
            "SanitizedHTMLAttr"
        }
        Kind::HTMLAttrValue => {
            ctx.imports.insert("SanitizedHTMLAttrValue");
            "SanitizedHTMLAttrValue"
        }
    }
}

fn append_tokens_as_sanitized_content(tokens: Vec<RustToken>, kind: &Kind, ctx: &mut Context) {
    match kind {
        Kind::HTML => ctx.add_line("SanitizedHTML::from_string_unsafe("),
        Kind::HTMLAttr => ctx.add_line("SanitizedHTMLAttr::from_string_unsafe("),
        Kind::HTMLAttrValue => ctx.add_line("SanitizedHTMLAttrValue::from_string_unsafe("),
        // TODO: Proper error handling
        _ => panic!(
            "unsupported kind in append_tokens_as_sanitized_html: {:?}",
            kind
        ),
    };
    ctx.tab_size += 1;
    let token = tokens
        .into_iter()
        .reduce(|a, b| a.merge_for_printing(b))
        .unwrap_or(RustToken::Primitive(RustPrimitive::String(String::new())));
    ctx.add_line(&format!("{}", token));
    ctx.tab_size -= 1;
    ctx.add_line(")");
}

fn token_to_rust(token: &Token, ctx: &mut Context) -> RustToken {
    match token {
        Token::Expression(expr) => expression_to_rust(expr, ctx),
        Token::HTMLTag(tag) => html_tag_to_rust(tag, ctx),
        Token::HTMLText(text) => html_text_to_rust(text),
    }
}

fn expression_to_rust(_expr: &Expression, ctx: &mut Context) -> RustToken {
    RustToken::Expression(String::new())
}

fn html_tag_to_rust(tag: &HTMLTag, ctx: &mut Context) -> RustToken {
    let mut format_str = "<".to_owned();
    let mut format_args = vec![];
    let attrs = match tag {
        HTMLTag::Void(vt) => {
            format_str.push_str(&vt.name.as_ref());
            &vt.attributes
        }
        HTMLTag::Open(ot) => {
            format_str.push_str(&ot.name.as_ref());
            &ot.attributes
        }
        HTMLTag::Close(ct) => {
            return RustToken::Primitive(RustPrimitive::String(format!("</{}>", ct)));
        }
    };
    for attr in attrs {
        let values = match attr {
            HTMLAttr::Expression(expr) => {
                format_str.push_str("{}");
                format_args.push(expression_to_rust(expr, ctx));
                continue;
            }
            HTMLAttr::KeyValue(kv) => {
                format_str.push_str(" ");
                format_str.push_str(&kv.key);
                &kv.value
            }
        };
        for (i, value) in values.iter().enumerate() {
            if i == 0 {
                format_str.push_str("=\"");
            } else {
                format_str.push_str(" ");
            }
            match value {
                &HTMLAttrValue::Expression(ref expr) => {
                    format_str.push_str("{}");
                    format_args.push(expression_to_rust(expr, ctx));
                }
                &HTMLAttrValue::Text(ref v) => {
                    format_str.push_str(v);
                }
            }
            if i == 0 {
                format_str.push('"');
            }
        }
    }
    format_str.push_str(">");
    if format_args.is_empty() {
        return RustToken::Primitive(RustPrimitive::String(format_str));
    } else {
        return RustToken::Format(format_str, format_args);
    }
}

fn html_text_to_rust(text: &Rc<str>) -> RustToken {
    RustToken::Primitive(RustPrimitive::String(text.as_ref().to_owned()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::{OpenTag, VoidTag, VoidTagName};

    #[test]
    fn test_html_tag_to_rust_simple() {
        let mut ctx = Context::new();
        let tag = HTMLTag::Void(VoidTag {
            name: VoidTagName::Col,
            attributes: vec![],
        });
        let token = html_tag_to_rust(&tag, &mut ctx);
        assert_eq!(
            token,
            RustToken::Primitive(RustPrimitive::String("<col>".to_owned()))
        );

        let tag = HTMLTag::Open(OpenTag {
            name: Rc::from("div"),
            attributes: vec![],
        });
        let token = html_tag_to_rust(&tag, &mut ctx);
        assert_eq!(
            token,
            RustToken::Primitive(RustPrimitive::String("<div>".to_owned()))
        );

        let tag = HTMLTag::Close(Rc::from("div"));
        let token = html_tag_to_rust(&tag, &mut ctx);
        assert_eq!(
            token,
            RustToken::Primitive(RustPrimitive::String("</div>".to_owned()))
        );
    }
}
