
use crate::types::{SourceRange,SourceToken};
#[derive(PartialEq,Debug)]
pub enum ParserErrorType {
    ErrWildcardInType
  , ErrConstraintInKind
  , ErrHoleInType
  , ErrExprInBinder
  , ErrExprInDeclOrBinder
  , ErrExprInDecl
  , ErrBinderInDecl
  , ErrRecordUpdateInCtr
  , ErrRecordPunInUpdate
  , ErrRecordCtrInUpdate
  , ErrTypeInConstraint
  , ErrElseInDecl
  , ErrInstanceNameMismatch
  , ErrUnknownFundep
  , ErrImportInDecl
  , ErrGuardInLetBinder
  , ErrKeywordVar
  , ErrKeywordSymbol
  , ErrQuotedPun
  , ErrToken
  , ErrLineFeedInString
  , ErrAstralCodePointInChar
  , ErrCharEscape
  , ErrNumberOutOfRange
  , ErrLeadingZero
  , ErrExpectedFraction
  , ErrExpectedExponent
  , ErrExpectedHex
  , ErrReservedSymbol
  , ErrCharInGap(char)
  , ErrModuleName
  , ErrQualifiedName
  , ErrEmptyDo
  , ErrLexeme(Option<String>,Vec<String>)
  , ErrEof
  , ErrCustom(String)
}

#[derive(PartialEq,Debug)]
pub enum ParserWarningType {
    WarnDeprecatedRowSyntax
  , WarnDeprecatedForeignKindSyntax
  , WarnDeprecatedConstraintInForeignImportSyntax
  , WarnDeprecatedKindImportSyntax
  , WarnDeprecatedKindExportSyntax
}
#[derive(Debug)]
pub struct ParserErrorInfo<T> {
    pub range:SourceRange,
    pub toks:Vec<SourceToken>,
    pub typ:T
}

pub type ParserError = ParserErrorInfo<ParserErrorType>;
pub type ParserWarning = ParserErrorInfo<ParserWarningType>;