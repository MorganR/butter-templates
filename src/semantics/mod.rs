#[macro_use]
mod base;
mod html;

use pest::iterators::{Pair, Pairs};

use crate::parser::Rule;
use base::{BlockKind, Error, FromPair, VariableKind};
use html::HTML;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use std::result::Result;

/// Defines a variable and its type.
#[derive(Debug, Clone)]
struct Variable {
    /// The name of the variable.
    name: Rc<str>,
    /// The type of data in the variable.
    kind: VariableKind,
}

/// A template block (i.e. top-level function).
#[derive(Debug)]
struct Block {
    /// The name of this block.
    name: Rc<str>,
    /// The scope of this block, including its arguments.
    scope: Rc<Scope>,
    /// The contents of the block.
    contents: Vec<html::HTML>,
    /// Arguments and data type of this block.
    metadata: Rc<BlockMetadata>,
}

#[derive(Debug, Clone)]
struct BlockMetadata {
    /// The arguments of this block.
    arguments: Vec<Variable>,
    /// The kind of value returned by this block.
    kind: BlockKind,
}

#[derive(Debug)]
enum Entity {
    Block(Rc<BlockMetadata>),
    Variable(VariableKind),
}

/// Defines identifiers in a given scope.
#[derive(Debug)]
struct Scope {
    /// The identifiers defined in this scope.
    ids: HashMap<Rc<str>, Entity>,
    /// The parent scope. All identifiers from the parent scope are visible in this scope, unless
    /// shadowed by a duplicate identifier in a child scope.
    parent: Option<Rc<Scope>>,
}

impl Scope {
    fn new() -> Scope {
        return Scope {
            ids: HashMap::new(),
            parent: None,
        };
    }

    fn from_parent(parent: Rc<Scope>) -> Scope {
        return Scope {
            ids: HashMap::new(),
            parent: Some(parent),
        };
    }
}

/// A butter package, corresponding to a single butter file.
#[derive(Debug)]
pub struct Package<'a> {
    /// The filepath of this package.
    path: &'a Path,
    /// The name of this package, to be used as the module/namespace/package name in output.
    name: Rc<str>,
    // TODO: Consider removing scope, as it shouldn't be needed outside of semantic analysis.
    /// The base scope, including imports and block statements.
    scope: Rc<Scope>,
    /// The blocks defined in this package.
    blocks: Vec<Block>,
    // TODO: Add imports.
}

impl<'a> Package<'a> {
    fn new(path: &'a Path, name: Rc<str>, scope: Rc<Scope>, blocks: Vec<Block>) -> Package<'a> {
        return Package {
            path: path,
            name: name,
            scope: scope,
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
            Rule::block_statement,
            Rule::block_statement,
            {
                let mut block_pairs = pair.into_inner();
                let type_pair = next_pair(&main, &mut block_pairs)?;
                let block_kind = BlockKind::from_pair(type_pair)?;
                let name_pair = next_pair(&main, &mut block_pairs)?;
                let block_name: Rc<str>;
                process_pair!(name_pair, Rule::identifier, Rule::identifier, {
                    block_name = Rc::from(name_pair.as_str());
                });
                // TODO: discover arguments
                let block_args = vec![];
                let block_meta = Rc::new(BlockMetadata {
                    arguments: block_args,
                    kind: block_kind,
                });
                package_scope.ids.insert(
                    Rc::clone(&block_name),
                    Entity::Block(Rc::clone(&block_meta)),
                );
                blocks_to_process.push((
                    block_name,
                    block_meta,
                    next_pair(&main, &mut block_pairs)?,
                ));
            },
            Rule::EOI,
            Rule::EOI,
            { break }
        );
    }

    let package_scope = Rc::new(package_scope);
    let mut package_blocks: Vec<Block> = Vec::with_capacity(blocks_to_process.len());
    for (block_name, block_meta, block_contents_pair) in blocks_to_process {
        let scope = Rc::new(Scope::from_parent(Rc::clone(&package_scope)));
        // TODO: push arguments into scope.

        let pairs = block_contents_pair.into_inner();
        let contents: Vec<HTML> = Vec::with_capacity(pairs.clone().count());

        for _ in pairs {
            // TODO: push contents into the block.
            // contents.push(HTML::from_pair_with_scope(pair, &mut scope)?);
        }

        package_blocks.push(Block {
            name: block_name,
            scope: scope,
            contents: contents,
            metadata: block_meta,
        });
    }

    return Ok(Package::new(
        path,
        package_name,
        package_scope,
        package_blocks,
    ));
}
