use serde::{Deserialize,Deserializer,de::{Visitor,MapAccess,Error}};
use std::fmt;
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
  pub name:Option<String>,
  pub start:SourcePos,
  pub end:SourcePos
}


pub struct Module {
    pub name:String,
   pub path:String
}

struct ModuleVisitor;

impl<'a> Visitor<'a> for ModuleVisitor {
    type Value = Module;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
       
        formatter.write_str("`secs` or `nanos`")
    }

   

    fn visit_map<A>(self,mut vmap: A) -> Result<Module, A::Error> where  A: MapAccess<'a>, {
      
       let module = Module {name:String::from("231"),path:String::from("122") };
      
       Ok(module)
    }
}

impl<'a> Deserialize<'a> for Module {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Module",&["moduleName"], ModuleVisitor)
    }
}


/*
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  } deriving (Show)
*/


#[test]
fn test_json() {
    let json = r#"{"moduleName":["Data","Array"]}"#;
    let pos:Module = serde_json::from_str(json).unwrap();
    
}

