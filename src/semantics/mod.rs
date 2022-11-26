#[macro_use]
mod base;

use pest::iterators::{Pair, Pairs};

use crate::parser::Rule;
use base::{Context, Entity, FromPair, HTMLTokenizer, Scope, SemanticContext, Tokenizer};
use std::path::Path;
use std::rc::Rc;
use std::result::Result;

pub use base::{
    BlockMetadata, Error, Expression, HTMLAttr, HTMLAttrValue, HTMLTag, Kind, OpenTag,
    PrimitiveKind, Token, VoidTag, VoidTagName,
};

/// A template block (i.e. top-level function).
#[derive(Debug)]
pub struct Block {
    /// The name of this block.
    pub name: Rc<str>,
    /// The contents of the block.
    pub contents: Vec<Token>,
    /// Arguments and data type of this block.
    pub metadata: Rc<BlockMetadata>,
}

/// A butter package, corresponding to a single butter file.
#[derive(Debug)]
pub struct Package<'a> {
    /// The filepath of this package.
    pub path: &'a Path,
    /// The name of this package, to be used as the module/namespace/package name in output.
    pub name: Rc<str>,
    /// The blocks defined in this package.
    pub blocks: Vec<Block>,
    // TODO: Add imports.
}

impl<'a> Package<'a> {
    fn new(path: &'a Path, name: Rc<str>, blocks: Vec<Block>) -> Package<'a> {
        return Package {
            path: path,
            name: name,
            blocks: blocks,
        };
    }
}

fn next_pair<'a>(
    parent: &Pair<'a, Rule>,
    pairs: &mut Pairs<'a, Rule>,
) -> Result<Pair<'a, Rule>, Error> {
    return match pairs.next() {
        Some(pair) => Ok(pair),
        _ => Err(Error::missing_pair(parent)),
    };
}

pub fn interpret_package<'a>(path: &'a Path, main: Pair<'a, Rule>) -> Result<Package<'a>, Error> {
    let mut pairs = main.clone().into_inner();
    let pair = next_pair(&main, &mut pairs)?;
    let package_name: Rc<str>;
    if let Rule::package_name = pair.as_rule() {
        package_name = Rc::from(pair.as_str());
    } else {
        return Err(Error::MissingPackage);
    }

    // TODO: imports

    let mut package_scope = Scope::new();
    let mut blocks_to_process: Vec<(Rc<str>, Rc<BlockMetadata>, Pair<'a, Rule>)> = vec![];
    for pair in pairs {
        process_pair!(
            pair,
            (Rule::block_statement),
            {
                let mut block_pairs = pair.into_inner();
                let type_pair = next_pair(&main, &mut block_pairs)?;
                let block_kind = Kind::from_pair(type_pair)?;
                let name_pair = next_pair(&main, &mut block_pairs)?;
                let block_name: Rc<str>;
                process_pair!(name_pair, (Rule::identifier), {
                    block_name = Rc::from(name_pair.as_str());
                });
                // TODO: discover arguments
                let block_args = vec![];
                let block_meta = Rc::new(BlockMetadata {
                    arguments: block_args,
                    kind: block_kind,
                });
                package_scope.push(
                    Rc::clone(&block_name),
                    Entity::Block(Rc::clone(&block_meta)),
                    &name_pair,
                )?;
                blocks_to_process.push((
                    block_name,
                    block_meta,
                    next_pair(&main, &mut block_pairs)?,
                ));
            },
            (Rule::EOI),
            { break },
        );
    }

    let package_scopes = vec![package_scope];
    let mut package_blocks: Vec<Block> = Vec::with_capacity(blocks_to_process.len());
    for (block_name, block_meta, block_contents_pair) in blocks_to_process {
        let mut scopes = package_scopes.clone();
        scopes.push(Scope::new());
        // TODO: push arguments into scope.

        let pairs = block_contents_pair.into_inner();

        let mut context = Context::new(scopes, SemanticContext::HTML);
        for p in pairs {
            // TODO: Keep track of errors.
            HTMLTokenizer::tokenize_into_context(p, &mut context)?;
        }

        package_blocks.push(Block {
            name: block_name,
            contents: context.take_tokens_and_destroy(),
            metadata: block_meta,
        });
    }

    return Ok(Package::new(path, package_name, package_blocks));
}
