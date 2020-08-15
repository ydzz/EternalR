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

#[derive(Debug)]
pub struct Ann (pub SourceSpan,pub Option<Meta>);

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
   pub imports:Vec<ImportItem>,
   pub foreign:Vec<Ident>,
   pub decls:Vec<Bind<Ann>>,
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

#[derive(Debug)]
pub enum Meta {
  IsConstructor(ConstructorType,Vec<Ident>),
  IsNewtype,
  IsTypeClassConstructor,
  IsForeign,
  IsWhere
}

#[derive(Debug)]
pub enum ConstructorType {
  ProductType,
  SumType,
}

#[derive(PartialEq,Debug)]
pub enum ProperName {
    TypeName(String),
    ConstructorName(String),
    ClassName(String),
    Namespace(String)
}

#[derive(PartialEq,Debug)]
pub enum OpName {
    ValueOpName(String),
    TypeOpName(String),
    AnyOpName(String)   
}

#[derive(Debug)]
pub enum Literal<A> {
  NumericLiteral(Result<i32,f64>),
  StringLiteral(String),
  CharLiteral(char),
  BooleanLiteral(bool),
  ArrayLiteral(Vec<A>),
  ObjectLiteral(HashMap<String,A>)
}
#[derive(Debug)]
pub enum Expr<A> {
  Literal(A,Literal<Box<Expr<A>>>),
  Constructor(A,ProperName,ProperName,Vec<Ident>),
  Accessor(A,String,Box<Expr<A>>),
  ObjectUpdate(A,Box<Expr<A>>,HashMap<String,Box<Expr<A>>>),
  Abs(A,Ident,Box<Expr<A>>),
  App(A,Box<Expr<A>>,Box<Expr<A>>),
  Var(A,Qualified<Ident>),
  Case(A,Vec<Expr<A>>,Vec<CaseAlternative<A>>),
  Let(A,Vec<Bind<A>>,Box<Expr<A>>)
}


#[derive(Debug)]
pub struct Qualified<A>(pub Option<String>,pub A);

pub type Guard<A> = Expr<A>;

#[derive(Debug)]
pub struct CaseAlternative<A> {
  alternative_binders:Vec<Binder<A>>,
  alternative_result:Result<Vec<(Guard<A>,Expr<A>)>,Expr<A>>
}

#[derive(Debug)]
pub enum Bind<A> {
  NonRec(A,Ident,Box<Expr<A>>),
  Rec(Vec<((A,Ident),Box<Expr<A>>)>)
}

#[derive(Debug)]
pub enum Binder<A> {
  NullBinder(A),
  LiteralBinder(A,Literal<Box<Binder<A>>>),
  VarBinder(A,Ident),
  NamedBinder(A,Ident,Box<Binder<A>>),
  ConstructorBinder(A,Qualified<ProperName>,Qualified<ProperName>,Vec<Binder<A>>)
}