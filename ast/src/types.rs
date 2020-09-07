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
pub struct SourceAnn(pub SourceSpan,pub Vec<Comment>);

pub type SourceType = Type<SourceAnn>;

#[derive(Debug)]
pub struct Ann (pub SourceSpan,pub Option<Meta>,pub Option<Type<()>>);

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

impl Ident {
  pub fn as_str(&self) -> Option<&str>  {
    match self {
      Ident::Ident(s) => Some(s.as_str()),
      _ => None
    }
  }
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


pub fn proper_name_as_str(proper_name:&ProperName) -> &str {
  match proper_name {
    ProperName::TypeName(s) => s.as_str(),
    ProperName::ConstructorName(s) => s.as_str(),
    ProperName::ClassName(s) => s.as_str(),
    ProperName::Namespace(s) => s.as_str()
  }
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
  ObjectLiteral(Vec<(String,A)>)
}
#[derive(Debug)]
pub enum Expr<A> {
  Literal(A,Literal<Box<Expr<A>>>),
  Constructor(A,ProperName,ProperName,Vec<Ident>,Vec<Type<()>>),
  Accessor(A,String,Box<Expr<A>>),
  ObjectUpdate(A,Box<Expr<A>>,Vec<(String,Box<Expr<A>>)>),
  Abs(A,Ident,Box<Expr<A>>),
  App(A,Box<Expr<A>>,Box<Expr<A>>),
  Var(A,Qualified<Ident>),
  Case(A,Vec<Expr<A>>,Vec<CaseAlternative<A>>),
  Let(A,Vec<Bind<A>>,Box<Expr<A>>)
}


#[derive(Debug)]
pub struct Qualified<A>(pub Option<String>,pub A);

impl Qualified<ProperName> {
  pub fn join_name(&self) -> String {
    let mut qual:String = self.0.as_ref().map(|s| {
      let mut news = s.clone();
      news.push('.');
      news
    }  ).unwrap_or_default();
    let proper = proper_name_as_str(&self.1);
    qual.push_str(proper);
    qual
  }
}

pub type Guard<A> = Expr<A>;

#[derive(Debug)]
pub struct CaseAlternative<A> {
  pub alternative_binders:Vec<Binder<A>>,
  pub alternative_result:Result<Vec<(Guard<A>,Expr<A>)>,Expr<A>>
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

pub type Label = String;

#[derive(Debug)]
pub enum Type<A> {
  TUnknown(A,i32),
  TypeVar(A,String),
  TypeLevelString(A,String),
  TypeWildcard(A,Option<String>),
  TypeConstructor(A,Qualified<ProperName>), //ProperName 'TypeName
  TypeOp(A,Qualified<OpName>), //OpName 'TypeOpName
  TypeApp(A,Box<Type<A>>,Box<Type<A>>),
  KindApp(A,Box<Type<A>>,Box<Type<A>>),
  ForAll(A,String,Option<Box<Type<A>>>,Box<Type<A>>,Option<i32>),
  ConstrainedType(A,Constraint<A>,Box<Type<A>>),
  Skolem(A,String,Option<Box<Type<A>>>,i32,i32),
  REmpty(A),
  RCons(A,Label,Box<Type<A>>,Box<Type<A>>),
  KindedType(A,Box<Type<A>>,Box<Type<A>>),
  BinaryNoParensType(A,Box<Type<A>>,Box<Type<A>>,Box<Type<A>>),
  ParensInType(A,Box<Type<A>>)
}

#[derive(Debug)]
pub enum ConstraintData {
  PartialConstraintData(Vec<Vec<String>>,bool)
}

#[derive(Debug)]
pub struct Constraint<A> {
  pub ann:A,
  pub class:Qualified<ProperName>, //ProperName 'ClassName
  pub kind_args:Vec<Type<A>>,
  pub args:Vec<Type<A>>,
  pub data:Option<ConstraintData>
}


pub fn prim_row() -> Qualified<ProperName> {
  Qualified(Some("Prim".to_string()),ProperName::TypeName("Row".to_string()))
}

pub fn prim_function() -> Qualified<ProperName> {
  Qualified(Some("Prim".to_string()),ProperName::TypeName("Function".to_string()))
}