use serde_cbor::{value::from_value,Value};
use crate::types::{self,Type,SourcePos,SourceSpan,Qualified,ProperName,Ident,SourceType,SourceAnn};
use std::collections::HashMap;
#[derive(Debug)]
pub struct ExternsFile {
    version:String,
    module_name:String,
    declarations:Vec<ExternsDeclaration>,
    source_span:SourceSpan
}

impl ExternsFile {
    pub fn decl_type_dic(&self) -> HashMap<&str,&Type<SourceAnn>> {
        let mut dic: HashMap<&str,&Type<SourceAnn>> = HashMap::default();
        for decl in self.declarations.iter() {
            let typ = decl.get_type();
            let name = decl.get_name();
            dic.insert(name, typ);
        }
        dic
    }
}

#[derive(Debug)]
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

impl ExternsDeclaration {
    pub fn get_type(&self) -> &Type<SourceAnn> {
        match self {
            ExternsDeclaration::EDValue {value_type,..} => {
                return value_type
            },
            _ => panic!()
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            ExternsDeclaration::EDValue {value_name,..} => {
                value_name.as_str().unwrap()
            },
            _ => panic!()
        }
    }
}

/*
{ efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
  } 
*/

impl Into<ExternsFile> for Value {
    fn into(self) -> ExternsFile {
        let mut array:Vec<Value> = from_value(self).unwrap();
        
        let source_span:SourceSpan = array.pop().unwrap().into();
        let mut e8_array:Vec<Value> = from_value(array.pop().unwrap()).unwrap();
        let declarations:Vec<ExternsDeclaration> = e8_array.drain(0..).map(|v| v.into() ).collect();
        array.pop(); //efTypeFixities
        array.pop(); //efFixities
        array.pop(); //efImports
        let _exports = array.pop().unwrap(); //efExports
       
        array.pop(); //efModuleName
        array.pop(); //efVersion
       
        ExternsFile {
            version : "1".into(),
            module_name: "Main".into(),
            declarations,
            source_span
        }
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
            0 => {
                todo!()
            },
            3 => {
                let mut id_arr:Vec<Value> = from_value(array.remove(0)).unwrap();
                let main:String = from_value(id_arr.pop().unwrap()).unwrap();
                let value_type:SourceType = array.remove(0).into();
                ExternsDeclaration::EDValue { 
                    value_name:Ident::Ident(main),
                    value_type
                }
            },
            _ => todo!()
        }
    }
}

impl Into<SourceType> for Value {
    fn into(self) -> SourceType {
        let mut array:Vec<Value> = from_value(self).unwrap();
        let e_num:i32 = from_value( array.remove(0)).unwrap();
        match e_num {
            4 => {
                let ann:SourceAnn = array.remove(0).into();
                let qual_name:Qualified<ProperName> = array.remove(0).into();
                types::Type::TypeConstructor(ann,qual_name)
            },
            e_num => {  panic!("todo type {}",e_num)}
        }
    }
}

impl Into<SourceAnn> for Value {
    fn into(self) -> SourceAnn {
        let mut array:Vec<Value> = from_value(self).unwrap();
        array.pop().unwrap();
        let source_span:SourceSpan = array.pop().unwrap().into();
        SourceAnn(source_span,vec![])
    }
}

impl Into<Qualified<ProperName>> for Value {
    fn into(self) -> Qualified<ProperName> {
        let mut array:Vec<Value> = from_value(self).unwrap();
        array.remove(0);
        let mut qual_names:Vec<String> = from_value(array.remove(0)).unwrap();
        let mut name_array:Vec<Value> = from_value(array.remove(0)).unwrap();
        let name:String = from_value(name_array.pop().unwrap()).unwrap();
        Qualified(Some(qual_names.pop().unwrap()),ProperName::ConstructorName(name))
    }
}

pub fn from_reader<R>(r:R) -> ExternsFile where R:std::io::Read {
    let val:Value = serde_cbor::from_reader(r).unwrap();
    val.into()
}

#[test]
fn test_cbor() {
    use std::fs::File;
    let tux_file = File::open("externs.cbor").unwrap();
   
    let ef:Value = serde_cbor::from_reader(&tux_file).unwrap();
    let eff :ExternsFile = ef.into();
    let typ_dic = eff.decl_type_dic();
    dbg!(typ_dic);
}