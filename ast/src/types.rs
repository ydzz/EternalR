use serde::{Deserialize};
use std::collections::HashMap;
#[derive(Deserialize,Debug)]
pub struct SourcePos {
   pub line:i32,
   pub col:i32
}

impl SourcePos {
    pub fn new(l:i32,c:i32) -> Self {
        SourcePos {line : l,col : c }
    }
}

#[derive(Deserialize,Debug)]
pub struct SourceSpan {
  #[serde(skip)] 
  pub name:String,
  pub start:SourcePos,
  pub end:SourcePos
}


//type Ann = (SourceSpan, [Comment], Maybe SourceType, Maybe Meta)

#[derive(Debug)]
pub struct Ann (pub SourceSpan);

#[derive(Debug)]
pub struct ImportItem(pub Ann,pub String);

#[derive(Debug)]
pub struct Module {
   pub source_span:SourceSpan, 
   pub name:String,
   pub path:String,
   pub version:String,
   pub comments:Vec<Comment>,
   pub exports: Vec<Ident>,
   pub re_exports:HashMap<String,Vec<Ident>>,
   pub imports:Vec<ImportItem>
}
#[derive(Debug)]
pub enum Comment {
  Line(String),
  Block(String)
}

#[derive(Debug)]
pub enum Ident {
  Ident(String),
  GenIdent(Option<String>,i32),
  UnusedIdent
}
