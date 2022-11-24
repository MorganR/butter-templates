use super::base::Expression;
use std::rc::Rc;

#[derive(Debug)]
pub enum AttrValue {
    Expression(Expression),
    Text(Rc<str>),
}

#[derive(Debug)]
pub enum AttrKey {
    Expression(Expression),
    Text(Rc<str>),
}

#[derive(Debug)]
pub struct Attr {
    key: AttrKey,
    value: Vec<AttrValue>,
}

#[derive(Debug)]
pub enum Tag {
    p(Vec<Attr>),
    div(Vec<Attr>),
    h1(Vec<Attr>),
}

#[derive(Debug)]
pub enum HTML {
    Expression(Expression),
    Tag(Tag),
    Text(Rc<str>),
}
