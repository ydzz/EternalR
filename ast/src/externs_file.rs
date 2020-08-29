use serde_derive::{Deserialize, Serialize};
use serde_cbor::{value::from_value,Value};
use crate::types::{SourcePos,SourceSpan,Ident,SourceType};

pub struct ExternsFile {
    version:String,
    module_name:String,
    declarations:Vec<ExternsDeclaration>,
    source_span:SourceSpan
}

pub enum ExternsDeclaration {
    EDType(),
    EDTypeSynonym(),
    EDDataConstructor(),
    EDValue {
        value_name:Ident,
        value_type:SourceType
    },
    EDClass(),
    EDInstance()
}

/*
-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: SourceType
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe SourceType)]
      , edTypeSynonymType         :: SourceType
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: SourceType
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: SourceType
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe SourceType)]
      , edClassMembers            :: [(Ident, SourceType)]
      , edClassConstraints        :: [SourceConstraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, SourceType)]
      , edInstanceKinds           :: [SourceType]
      , edInstanceTypes           :: [SourceType]
      , edInstanceConstraints     :: Maybe [SourceConstraint]
      , edInstanceChain           :: [Qualified Ident]
      , edInstanceChainIndex      :: Integer
      }
  deriving (Show, Generic)
*/

impl Into<ExternsFile> for Value {
    fn into(self) -> ExternsFile {
        let mut array:Vec<Value> = from_value(self).unwrap();
        
        let source_span:SourceSpan = array.pop().unwrap().into();
        let e8 = array.pop().unwrap();
        let e7 = array.pop().unwrap();
        let e6 = array.pop();
        let e5 = array.pop();
        let e4 = array.pop();
        let e3 = array.pop();
        let e2 = array.pop();
        let e1 = array.pop();      
        
        let mut e8_array:Vec<Value> = from_value(e8).unwrap();
        let array2:Vec<Value> = from_value(e8_array.pop().unwrap()).unwrap();

        dbg!(&array2[0]);
        dbg!(&array2[1]);
        dbg!(&array2[2]);
        dbg!(source_span);
        todo!()
    }
}

impl Into<SourcePos> for Value {
    fn into(self) -> SourcePos {
        let mut array:Vec<Value> = from_value(self).unwrap();
        let c:i32 = from_value( array.pop().unwrap() ).unwrap();
        let l:i32 = from_value( array.pop().unwrap() ).unwrap();
        SourcePos::new(l, c)
    }
}

impl Into<SourceSpan> for Value {
    fn into(self) -> SourceSpan {
        let mut array:Vec<Value> = from_value(self).unwrap();
        let e4 = array.pop().unwrap();
        let e3 = array.pop().unwrap();
        let e2 = array.pop().unwrap();
        let end:SourcePos = e4.into();
        let start:SourcePos = e3.into();
        let name:String = from_value(e2).unwrap();
        SourceSpan { name,start,end }
    }
}

impl Into<ExternsDeclaration> for Value {
    fn into(self) -> ExternsDeclaration {
        let mut array:Vec<Value> = from_value(self).unwrap();
        let e_num:i32 = from_value( array.remove(0)).unwrap();
        match e_num {
            _ => todo!()
        }
    }
}
#[test]
fn test_cbor() {
    use std::fs::File;
    let tux_file = File::open("externs.cbor").unwrap();
   
    let ef:Value = serde_cbor::from_reader(&tux_file).unwrap();
    let eff :ExternsFile = ef.into();
    
}