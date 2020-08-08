use std::fmt::{Display,Formatter,self};

#[derive(PartialEq,Clone,Debug)]
pub struct SourcePos {
    pub line:u32,
    pub column:u32
}

impl SourcePos {
    pub fn new(line:u32,column:u32) -> Self {
        SourcePos {
            line,
            column
        }
    }
}
#[derive(PartialEq,Clone,Debug)]
pub struct SourceRange {
    pub  start:SourcePos,
    pub end:SourcePos
}

impl SourceRange {
    pub fn new(start:SourcePos,end:SourcePos) -> SourceRange {
        SourceRange {
            start,
            end
        }
    }
}

#[derive(PartialEq,Clone,Debug)]
pub enum SourceStyle {
    ASCII,
    Unicode
}

#[derive(PartialEq,Clone,Debug)]
pub struct SourceToken {
    pub ann:TokenAnn,
    pub  value:Token
}

impl SourceToken {
    pub fn new(ann:TokenAnn,value:Token) -> SourceToken {
        SourceToken {
            ann,
            value
        }
    }
}

#[derive(PartialEq,Clone,Debug)]
pub struct TokenAnn {
   pub range:SourceRange,
   pub leading_comments:Vec<Comment>,
   pub trailing_comments:Vec<Comment>
}

#[derive(PartialEq,Clone,Debug)]
pub enum Token {
      TokLeftParen
    , TokRightParen
    , TokLeftBrace
    , TokRightBrace
    , TokLeftSquare
    , TokRightSquare
    , TokLeftArrow(SourceStyle)
    , TokRightArrow(SourceStyle)
    , TokLeftFatArrow(SourceStyle)
    , TokRightFatArrow(SourceStyle)
    , TokDoubleColon(SourceStyle)
    , TokForall
    , TokForallU
    , TokEquals
    , TokPipe
    , TokTick
    , TokDot
    , TokComma
    , TokUnderscore
    , TokBackslash
    , TokQualLowerName(Vec<String>,String)
    , TokLowerName(String)
    , TokAdo
    , TokAs
    , TokCase
    , TokClass
    , TokData
    , TokDerive
    , TokDo
    , TokElse
    , TokFalse
    , TokForeign
    , TokHiding
    , TokImport
    , TokIf
    , TokIn
    , TokInfix
    , TokInfixl
    , TokInfixr
    , TokInstance
    , TokKind
    , TokLet
    , TokModule
    , TokNewtype
    , TokNominal
    , TokPhantom
    , TokOf
    , TokRepresentational
    , TokRole
    , TokThen
    , TokTrue
    , TokType
    , TokWhere
    , TokQualUpperName(Vec<String>,String)
    , TokUpperName(String)
    , TokQualOperator(Vec<String>,String)
    , TokOperator(String)
    , TokColon
    , TokOperatorSub   //-
    , TokOperatorAt   //@
    , TokOperatorHash //#
    , TokSymbolDoubleDot
    , TokSymbolName(String)
    , TokQualSymbolName(Vec<String>,String)
    , TokSymbolArr(SourceStyle)
    , TokHole(String)
    , TokChar(String,char)
    , TokString(String,String)
    , TokRawString(String)
    , TokInt(String,i32)
    , TokNumber(String,f64)
    , TokLayoutStart
    , TokLayoutSep
    , TokLayoutEnd
    , TokEof
}

pub fn is_eof(tk:&Token) -> bool {
    match tk {
        Token::TokEof => true,
        _ => false
    }
}


impl Display for SourcePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "col:{},line:{}", self.column,self.line)
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} - {}", self.start,self.end)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::TokLeftParen =>  write!(f, "{}", "("),
            Token::TokRightParen =>  write!(f, "{}", ")"),
            Token::TokLeftBrace =>  write!(f, "{}", "{"),
            Token::TokRightBrace =>  write!(f, "{}", "}"),
            Token::TokLeftSquare =>  write!(f, "{}", "["),
            Token::TokRightSquare =>  write!(f, "{}", "]"),
            Token::TokLeftArrow(SourceStyle::ASCII) =>  write!(f, "{}", "<-"),
            Token::TokLeftArrow(SourceStyle::Unicode) =>  write!(f, "{}", "←"),
            Token::TokRightArrow(SourceStyle::ASCII) =>  write!(f, "{}", "->"),
            Token::TokRightArrow(SourceStyle::Unicode) =>  write!(f, "{}", "→"),
            Token::TokRightFatArrow(SourceStyle::ASCII) => write!(f, "{}", "=>"),
            Token::TokRightFatArrow(SourceStyle::Unicode) => write!(f, "{}", "⇒"),
            Token::TokLeftFatArrow(SourceStyle::ASCII) => write!(f, "{}", "<="),
            Token::TokLeftFatArrow(SourceStyle::Unicode) => write!(f, "{}", "⇐"),
            Token::TokDoubleColon(SourceStyle::ASCII) => write!(f, "{}", "::"),
            Token::TokDoubleColon(SourceStyle::Unicode) => write!(f, "{}", "∷"),
            Token::TokForall => write!(f, "{}", "forall"),
            Token::TokForallU => write!(f, "{}", "∀"),
            Token::TokEquals => write!(f, "{}", "="),
            Token::TokPipe =>  write!(f, "{}", "|"),
            Token::TokTick => write!(f, "{}", "`"),
            Token::TokDot => write!(f, "{}", "."),
            Token::TokComma => write!(f, "{}", ","),
            Token::TokUnderscore => write!(f, "{}", "_"),
            Token::TokBackslash => write!(f, "{}", "\\"),
            Token::TokQualLowerName(qual,name) => {
                let mut str = qual_string(qual);
                str.push_str(name.as_str());
                write!(f, "{}", str.as_str())
            },
            Token::TokLowerName(name) => write!(f, "{}",name),
            Token::TokSymbolDoubleDot => write!(f, "{}", ".."),
            Token::TokAdo =>  write!(f, "{}", "ado"),
            Token::TokAs => write!(f, "{}", "as"),
            Token::TokCase => write!(f, "{}", "case"),
            Token::TokClass => write!(f, "{}", "class"),
            Token::TokData => write!(f, "{}", "data"),
            Token::TokDerive => write!(f, "{}", "derive"),
            Token::TokDo => write!(f, "{}", "do"),
            Token::TokElse => write!(f, "{}", "else"),
            Token::TokFalse => write!(f, "{}", "false"),
            Token::TokForeign => write!(f, "{}", "foreign"),
            Token::TokHiding => write!(f, "{}", "hiding"),
            Token::TokImport => write!(f, "{}", "import"),
            Token::TokIf => write!(f, "{}", "if"),
            Token::TokIn => write!(f, "{}", "in"),
            Token::TokInfix => write!(f, "{}", "infix"),
            Token::TokInfixl => write!(f, "{}", "infixl"),
            Token::TokInfixr => write!(f, "{}", "infixr"),
            Token::TokInstance => write!(f, "{}", "instance"),
            Token::TokKind => write!(f, "{}", "kind"),
            Token::TokLet => write!(f, "{}", "let"),
            Token::TokModule => write!(f, "{}", "module"),
            Token::TokNewtype => write!(f, "{}", "newtype"),
            Token::TokNominal => write!(f, "{}", "nominal"),
            Token::TokPhantom => write!(f, "{}", "phantom"),
            Token::TokOf => write!(f, "{}", "of"),
            Token::TokRepresentational => write!(f, "{}", "representational"),
            Token::TokRole => write!(f, "{}", "role"),
            Token::TokThen => write!(f, "{}", "then"),
            Token::TokTrue => write!(f, "{}", "true"),
            Token::TokType => write!(f, "{}", "type"),
            Token::TokWhere => write!(f, "{}", "where"),
            Token::TokQualUpperName(qual,name) => {
                let mut str = qual_string(qual);
                str.push_str(name.as_str());
                write!(f, "{}", str.as_str())
            },
            Token::TokUpperName(name) => write!(f, "{}", name),
            Token::TokQualOperator(qual,sym) => {
                let mut str = qual_string(qual);
                str.push_str(sym.as_str());
                write!(f, "{}", str.as_str())
            },
            Token::TokOperator(sym) => write!(f, "{}", &sym),
            Token::TokColon => write!(f, "{}", ":"),
            Token::TokOperatorSub =>write!(f, "{}", "-"),
            Token::TokOperatorHash =>write!(f, "{}", "#"),
            Token::TokOperatorAt =>write!(f, "{}", "@"),
            Token::TokQualSymbolName(qual,sym) => {
                let mut str:String = qual_string(qual);
                str.push('(');
                str.push_str(sym.as_str());
                str.push(')');
                write!(f, "{}", str.as_str())
            },
            Token::TokSymbolName(sym) =>  write!(f, "({})", sym.as_str()),
            Token::TokSymbolArr(SourceStyle::Unicode) => write!(f, "{}", "(→)"),
            Token::TokHole(hole) => {
                write!(f, "{}{}", "?",hole)
            },
            Token::TokSymbolArr(SourceStyle::ASCII) => write!(f, "{}", "(->)"),
            Token::TokChar(raw,_) =>  write!(f, "'{}'", raw),
            Token::TokString(raw,_) =>  write!(f, "\"{}\"", raw),
            Token::TokRawString(raw) =>  write!(f, "\"\"\"{}\"\"\"", raw),
            Token::TokInt(raw,_) => write!(f, "{}", raw),
            Token::TokNumber(raw,_) => write!(f, "{}", raw),
            Token::TokLayoutStart => write!(f, "{}", "{"),
            Token::TokLayoutSep => write!(f, "{}", ";"),
            Token::TokLayoutEnd => write!(f, "{}", "}"),
            Token::TokEof => write!(f, "{}", "<eof>")
        }
    }
}

fn qual_string(str_arr:&Vec<String>) -> String {
    let mut ret = String::default();
    for str in str_arr {
        ret.push_str(str.as_str());
        ret.push('.');
    }
    ret
}


#[derive(PartialEq,Clone,Debug)]
pub enum Comment {
    Comment(String),
    Space(usize),
    Line(LineFeed)
}

#[derive(PartialEq,Clone,Debug)]
pub enum LineFeed {
    LF,
    CRLF
}

impl LineFeed {
    pub fn is_line_feed(chr:char) -> bool {
        chr == '\r' || chr == '\n'
    }
}


pub fn print_source_token(tok:&SourceToken) -> String {
    let mut ret = String::default();
    ret.push_str(print_leading_comment(&tok.ann.leading_comments).as_str() );
    let tok_str = format!("{}",tok.value);
    ret.push_str(tok_str.as_str());
    ret.push_str(print_trailing_comment(&tok.ann.trailing_comments).as_str() );
    ret
}

fn print_leading_comment(comments:&Vec<Comment>) -> String {
    let mut ret = String::default();
    for comment in comments {
        match comment {
            Comment::Comment(raw) => ret.push_str(raw.as_str()),
            Comment::Space(n) => {
                for _ in 0..*n {
                    ret.push(' ');
                }
            },
            Comment::Line(LineFeed::LF) => ret.push('\n'),
            Comment::Line(LineFeed::CRLF) => ret.push_str("\r\n"),
        }
    }
    ret
}

fn print_trailing_comment(comments:&Vec<Comment>) -> String {
    let mut ret = String::default();
    for comment in comments {
        match comment {
            Comment::Comment(raw) => ret.push_str(raw.as_str()),
            Comment::Space(n) => {
                for _ in 0..*n {
                    ret.push(' ');
                }
            },
            _ => {}
        }
    }
    ret
}
/*
#[derive(PartialEq,Clone,Debug)]
pub struct Ident(pub String);

#[derive(PartialEq,Clone,Debug)]
pub struct Name<T> {
    pub tok:SourceToken,
    pub value:T
}

#[derive(PartialEq,Clone,Debug)]
pub struct QualifiedName<T:Clone> {
   pub tok:SourceToken,
   pub  module:Option<names::ModuleName>,
   pub name:T
}

#[derive(PartialEq,Clone,Debug)]
pub struct Label {
    pub  tok:SourceToken,
    pub  name:String
}

#[derive(PartialEq,Clone,Debug)]
pub struct Wrapped<T:Clone> {
    pub open:SourceToken,
    pub value:T,
    pub close:SourceToken
}
#[derive(PartialEq,Clone,Debug)]
pub struct Separated<T> {
    pub head:T,
    pub tail:Vec<(SourceToken,T)>
}

#[derive(PartialEq,Clone,Debug)]
pub struct Labeled<A:Clone,B:Clone> {
   pub label:A,
   pub sep:SourceToken,
   pub value:B
}

type Delimited<T> = Wrapped<Option<Separated<T>>>;

type DelimitedNonEmpty<T> = Wrapped<Separated<T>>;

#[derive(PartialEq,Clone,Debug)]
pub enum OneOrDelimited<T:Clone> {
    One(T),
    Many(DelimitedNonEmpty<T>)
}

#[derive(PartialEq,Debug,Clone)]
pub struct Row<A:Clone> {
    pub labels:Option<Separated<Labeled<Label,Box<Type<A>>>>>,
    pub tail:Option<(SourceToken,Box<Type<A>>)>
}

#[derive(PartialEq,Debug,Clone)]
pub enum TypeVarBinding<A:Clone> {
    TypeVarKinded(Wrapped<Labeled<Name<Ident>,Box<Type<A>>>>),
    TypeVarName(Name<Ident>)
}


#[derive(PartialEq,Clone,Debug)]
pub enum Constraint<A:Clone> {
    Constraint(A,QualifiedName<names::ProperName>,Vec<Box<Type<A>>>),
    ConstraintParens(A,Box<Wrapped<Constraint<A>>>)
}

#[derive(PartialEq,Clone,Debug)]
pub enum Type<A:Clone> {
    TypeVar(A,Name<Ident>),
    TypeConstructor(A,QualifiedName<names::ProperName>),
    TypeWildcard(A,SourceToken),
    TypeHole(A,Name<Ident>),
    TypeString(A,SourceToken,String),
    TypeRow(A,Wrapped<Row<A>>),
    TypeRecord(A,Wrapped<Row<A>>),
    TypeForall(A,SourceToken,Vec<TypeVarBinding<A>>,SourceToken,Box<Type<A>>),
    TypeKinded(A,Box<Type<A>>,SourceToken,Box<Type<A>>),
    TypeApp(A,Box<Type<A>>,Box<Type<A>>),
    TypeOp(A,Box<Type<A>>,QualifiedName<names::OpName>,Box<Type<A>>),
    TypeOpName(A,QualifiedName<names::OpName>),
    TypeArr(A,Box<Type<A>>,SourceToken,Box<Type<A>>),
    TypeArrName(A,SourceToken),
    TypeConstrained(A,Box<Constraint<A>>,SourceToken,Box<Type<A>>),
    TypeParens(A,Wrapped<Box<Type<A>>>),
    TypeUnaryRow(A,SourceToken,Box<Type<A>>)
}


#[derive(PartialEq,Clone,Debug)]
pub enum DataMembers<A:Clone> {
    DataAll(A,SourceToken),
    DataEnumerated(A,Delimited<Name<names::ProperName>>)
}

#[derive(PartialEq,Clone,Debug)]
pub enum Export<A:Clone> {
    ExportValue(A,Name<Ident>),
    ExportOp(A,Name<names::OpName>),
    ExportType(A,Name<names::ProperName>,Option<DataMembers<A>>),
    ExportTypeOp(A,SourceToken,Name<names::OpName>),
    ExportClass(A,SourceToken,Name<names::ProperName>),
    ExportKind(A,SourceToken,Name<names::ProperName>),
    ExportModule(A,SourceToken,Name<names::ModuleName>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct ImportDecl<A:Clone> {
    ann:A,
    keyword:SourceToken,
    module:Name<names::ModuleName>,
    names:Option<(Option<SourceToken>,DelimitedNonEmpty<Import<A>>)>,
    qual:Option<(SourceToken,Name<names::ModuleName>)>
}

#[derive(PartialEq,Clone,Debug)]
pub enum Import<A:Clone> {
    ImportValue(A,Name<Ident>),
    ImportOp(A,Name<names::OpName>),
    ImportType(A,Name<names::ProperName>,Option<DataMembers<A>>),
    ImportTypeOp(A,SourceToken,Name<names::OpName>),
    ImportClass(A,SourceToken,Name<names::ProperName>),
    ImportKind(A,SourceToken,Name<names::ProperName>)
}


#[derive(PartialEq,Debug,Clone)]
pub struct Module<A:Clone> {
    ann:A,
    keyword:SourceToken,
    namespace:Name<names::ModuleName>,
    exports:Option<DelimitedNonEmpty<Export<A>>>,
    mod_where:SourceToken,
    imports:Vec<ImportDecl<A>>,
    decls:Vec<Declaration<A>>,
    trailing_comments:Vec<Comment>
}

#[derive(PartialEq,Clone,Debug)]
pub struct DataHead<A:Clone> {
    hdkeyword:SourceToken,
    hdname:Name<names::ProperName>,
    hdvars:Vec<TypeVarBinding<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub struct DataCtor<A:Clone> {
    ctorann:A,
    ctorname:Name<names::ProperName>,
    ctorfields:Vec<Type<A>>,
}

#[derive(PartialEq,Clone,Debug)]
pub struct ClassHead<A:Clone> {
    keyword:SourceToken,
    clssuper:Option<(OneOrDelimited<Constraint<A>>,SourceToken)>,
    name:Name<names::ProperName>,
    vars:Vec<TypeVarBinding<A>>,
    fundeps:Option<(SourceToken,Separated<ClassFundep>)>
}

#[derive(PartialEq,Clone,Debug)]
pub enum ClassFundep {
    FundepDetermined(SourceToken,Vec<Name<Ident>>),
    FundepDetermines(Vec<Name<names::Ident>>,SourceToken,Vec<Name<Ident>>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct InstanceHead<A:Clone> {
    keyword:SourceToken,
    name:Name<Ident>,
    sep:SourceToken,
    constraints:Option<(OneOrDelimited<Constraint<A>>,SourceToken)>,
    class:QualifiedName<names::ProperName>,
    types:Vec<Type<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub enum InstanceBinding<A:Clone> {
    InstanceBindingSignature(A,Labeled<Name<Ident>,Type<A>>),
    InstanceBindingName(A,ValueBindingFields<A>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct Instance<A:Clone> {
    head:InstanceHead<A>,
    body:Option<(SourceToken,Vec<InstanceBinding<A>>)>
}

#[derive(PartialEq,Clone,Debug)]
pub struct ValueBindingFields<A:Clone> {
    name:Name<Ident>,
    binders:Vec<Binder<A>>,
    val_guarded:Guarded<A>,
}

#[derive(PartialEq,Clone,Debug)]
pub struct FixityFields {
    keyword:(SourceToken, Fixity),
    prec:(SourceToken,i32),
    fxtop: FixityOp
}

#[derive(PartialEq,Clone,Debug)]
pub enum Fixity {
    Infix,
    Infixl,
    Infixr
}

#[derive(PartialEq,Clone,Debug)]
pub enum FixityOp {
    FixityValue(QualifiedName<Result<Ident,names::ProperName>>,SourceToken,Name<names::OpName>),
    FixityType(SourceToken,QualifiedName<names::ProperName>,SourceToken,Name<names::OpName>)
}


#[derive(PartialEq,Clone,Debug)]
pub enum Foreign<A:Clone> {
    ForeignValue(Labeled<Name<Ident>,Type<A>>),
    ForeignData(SourceToken,Labeled<Name<names::ProperName>,Type<A>>),
    ForeignKind(SourceToken,Name<names::ProperName>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct Role {
    tok:SourceToken,
    value:roles::Role
}



#[derive(PartialEq,Clone,Debug)]
pub enum Declaration<A:Clone> {
    DeclData(A,DataHead<A>,Option<(SourceToken,Separated<DataCtor<A>>)>),
    DeclType(A,DataHead<A>,SourceToken,Type<A>),
    DeclNewtype(A,DataHead<A>,SourceToken,Name<names::ProperName>,Type<A>),
    DeclClass(A,ClassHead<A>,Option<(SourceToken,Vec<Labeled<Name<Ident>,Type<A>>>)>),
    DeclInstanceChain(A,Separated<Instance<A>>),
    DeclDerive(A,SourceToken,Option<SourceToken>,InstanceHead<A>),
    DeclKindSignature(A,SourceToken,Labeled<Name<names::ProperName>,Type<A>>),
    DeclSignature(A,Labeled<Name<Ident>,Type<A>>),
    DeclValue(A,ValueBindingFields<A>),
    DeclFixity(A,FixityFields),
    DeclForeign(A,SourceToken,SourceToken,Foreign<A>),
    DeclRole(A,SourceToken,SourceToken,Name<names::ProperName>,Vec<Role>)
}

#[derive(PartialEq,Clone,Debug)]
pub enum RecordLabeled<A:Clone> {
    RecordPun(Name<Ident>),
    RecordField(Label,SourceToken,A)
}

#[derive(PartialEq,Clone,Debug)]
pub struct RecordAccessor<A:Clone> {
    expr:Box<Expr<A>>,
    dot:SourceToken,
    path:Separated<Label>
}

#[derive(PartialEq,Clone,Debug)]
pub enum RecordUpdate<A:Clone> {
    RecordUpdateLeaf(Label,SourceToken,Box<Expr<A>>),
    RecordUpdateBranch(Label,DelimitedNonEmpty<Box<RecordUpdate<A>>>),
}

#[derive(PartialEq,Clone,Debug)]
pub struct Lambda<A:Clone> {
    symbol:SourceToken,
    binder:Vec<Binder<A>>,
    arr:SourceToken,
    body:Box<Expr<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub enum Expr<A:Clone> {
    ExprHole(A,Name<Ident>),
    ExprSection(A,SourceToken),
    ExprIdent(A,QualifiedName<Ident>),
    ExprConstructor(A,QualifiedName<names::ProperName>),
    ExprBoolean(A,SourceToken,bool),
    ExprChar(A,SourceToken,String),
    ExprString(A,SourceToken,String),
    ExprNumber(A,SourceToken,Result<i32,f64>),
    ExprArray(A,Delimited<Box<Expr<A>>>),
    ExprRecord(A,Delimited<RecordLabeled<Box<Expr<A>>>>),
    ExprParens(A,Wrapped<Box<Expr<A>>>),
    ExprTyped(A,Box<Expr<A>>,SourceToken,Type<A>),
    ExprInfix(A,Box<Expr<A>>,Wrapped<Box<Expr<A>>>,Box<Expr<A>>),
    ExprOp(A,Box<Expr<A>>,QualifiedName<names::OpName>,Box<Expr<A>>),
    ExprOpName(A,QualifiedName<names::OpName>),
    ExprNegate(A,SourceToken,Box<Expr<A>>),
    ExprRecordAccessor(A,RecordAccessor<A>),
    ExprRecordUpdate(A,Box<Expr<A>>,DelimitedNonEmpty<RecordUpdate<A>>),
    ExprApp(A,Box<Expr<A>>,Box<Expr<A>>),
    ExprLambda(A,Lambda<A>),
    ExprIf(A,IfThenElse<A>),
    ExprCase(A,CaseOf<A>),
    ExprLet(A,LetIn<A>),
    ExprDo(A,DoBlock<A>),
    ExprAdo(A,AdoBlock<A>)
}


#[derive(PartialEq,Clone,Debug)]
pub struct AdoBlock<A:Clone> {
    keyword:SourceToken,
    statements:Vec<DoStatement<A>>,
    adoin:SourceToken,
    result:Box<Expr<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub struct DoBlock<A:Clone> {
    keyword:SourceToken,
    statements:Vec<DoStatement<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub enum DoStatement<A:Clone> {
    DoLet(SourceToken,Vec<LetBinding<A>>),
    DoDiscard(Box<Expr<A>>),
    DoBind(Binder<A>,SourceToken,Box<Expr<A>>)
}

#[derive(PartialEq,Clone,Debug)]
pub enum LetBinding<A:Clone> {
    LetBindingSignature(A,Labeled<Name<Ident>,Type<A>>),
    LetBindingName(A,ValueBindingFields<A>),
    LetBindingPattern(A,Binder<A>,SourceToken,Box<Where<A>>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct Where<A:Clone> {
    expr:Box<Expr<A>>,
    bindings:Option<(SourceToken,Vec<LetBinding<A>>)>
}

#[derive(PartialEq,Clone,Debug)]
pub struct LetIn<A:Clone> {
    keyword:SourceToken,
    bindings:Vec<LetBinding<A>>,
    letin:SourceToken,
    body:Box<Expr<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub struct CaseOf<A:Clone> {
    keyword:SourceToken,
    head: Separated<Box<Expr<A>>>,
    caseof:SourceToken,
    case_branches:Vec<(Separated<Binder<A>>,Guarded<A>)>
}

#[derive(PartialEq,Clone,Debug)]
pub struct IfThenElse<A:Clone> {
   pub iteif:SourceToken,
   pub cond:Box<Expr<A>>,
   pub then:SourceToken,
   pub itetrue:Box<Expr<A>>,
   pub iteelse:SourceToken,
   pub itefalse:Box<Expr<A>>
}

#[derive(PartialEq,Clone,Debug)]
pub enum Binder<A:Clone> {
    BinderWildcard(A,SourceToken),
    BinderVar(A,Name<names::Ident>),
    BinderNamed(A,Name<Ident>,SourceToken,Box<Binder<A>>),
    BinderConstructor(A,QualifiedName<names::ProperName>,Vec<Binder<A>>),
    BinderBoolean(A,SourceToken,bool),
    BinderChar(A,SourceToken,char),
    BinderString(A,SourceToken,String),
    BinderNumber(A,Option<SourceToken>,SourceToken,Result<i32,f64>),
    BinderArray(A,Delimited<Box<Binder<A>>>),
    BinderRecord(A,Delimited<RecordLabeled<Box<Binder<A>>>>),
    BinderParens(A,Wrapped<Box<Binder<A>>>),
    BinderTyped(A,Box<Binder<A>>,SourceToken,Type<A>),
    BinderOp(A,Box<Binder<A>>,QualifiedName<names::OpName>,Box<Binder<A>>)
}

#[derive(PartialEq,Clone,Debug)]
pub struct PatternGuard<A:Clone> {
    binder:Option<(Binder<A>,SourceToken)>,
    expr:Box<Expr<A>>
}
#[derive(PartialEq,Clone,Debug)]
pub struct GuardedExpr<A:Clone> {
    bar:SourceToken,
    patterns:Separated<PatternGuard<A>>,
    sep:SourceToken,
    grdwhere:Where<A>
}
#[derive(PartialEq,Clone,Debug)]
pub enum Guarded<A:Clone> {
    Unconditional(SourceToken,Where<A>),
    Guarded(Vec<GuardedExpr<A>>)
}
*/