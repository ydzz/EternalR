use serde::{Deserialize,Deserializer,de::{Visitor,MapAccess,IgnoredAny}};
use std::fmt;
use crate::types::{Module,Comment,SourceSpan,Ident,Ann,ImportItem};
fn join_module_name(names:Vec<&str>) -> String {
    let mut ret = String::default();
    let mut idx = 0;
    for n in names.iter() {
        ret.push_str(*n);
        if idx < names.len() - 1 {
            ret.push('.');
        }
        idx+=1;
    }
    ret
}
////////////
struct ModuleVisitor;
impl<'a> Visitor<'a> for ModuleVisitor {
    type Value = Module;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse Module error")
    }

    fn visit_map<A>(self,mut vmap: A) -> Result<Module, A::Error> where  A: MapAccess<'a> {
       let mut module_name = String::default();
       let mut version = String::default();
       let mut path = String::default();
       let mut comments = vec![];
       let mut source_span = None;
       let mut exports = vec![];
       let re_exports = std::collections::HashMap::new();
       let mut imports = vec![];
       loop {
          match vmap.next_key() {
              Ok(Some("moduleName")) => {
                  let names = vmap.next_value::<Vec<&str>>()?;
                  module_name = join_module_name(names)
              },
              Ok(Some("builtWith")) => { version = vmap.next_value::<String>()?},
              Ok(Some("modulePath")) => { path = vmap.next_value::<String>()?},
              Ok(Some("comments")) => { comments = vmap.next_value::<Vec<Comment>>()?},
              Ok(Some("imports")) => { imports = vmap.next_value::<Vec<ImportItem>>()?},
              Ok(Some("exports")) => { 
                  let exports_str = vmap.next_value::<Vec<&str>>()?;
                  exports = exports_str.iter().map(|e| Ident::Ident(e.to_string())).collect();
              },
              Ok(Some("sourceSpan")) => {
                  let mut ss:SourceSpan = vmap.next_value()?;
                  ss.name = module_name.clone();
                  source_span = Some(ss); 
                },
              Ok(Some(_)) =>{ let _ = vmap.next_value::<IgnoredAny>();},
              _ => break
          }
       }
       Ok(Module { 
          name:module_name,
          path,
          version,
          source_span:source_span.unwrap(),
          comments,
          exports,
          re_exports,
          imports
        })
    }
}

impl<'a> Deserialize<'a> for Module {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Module",&[""], ModuleVisitor)
    }
}
////////////
struct CommentVisitor;
impl<'a> Visitor<'a> for CommentVisitor {
    type Value = Comment;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse Comment error")
    }

    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        loop {
            match map.next_key() {
                Ok(Some("LineComment")) => { return Ok(Comment::Line(map.next_value::<String>()?)); },
                Ok(Some("BlockComment")) => { return Ok(Comment::Line(map.next_value::<String>()?)); },
                 _ => panic!()
            }
        }
    }
}
impl<'a> Deserialize<'a> for Comment {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Comment",&[""], CommentVisitor)
    }
}

////////////
struct ImportItemVisitor;

impl<'a> Visitor<'a> for ImportItemVisitor {
    type Value = ImportItem;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse ImportItem error")
    }
    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut module_name = String::default();
        let mut ann = None;
        loop {
            match map.next_key() {
                Ok(Some("moduleName")) => {
                    let names = map.next_value::<Vec<&str>>()?;
                    module_name = join_module_name(names)
                },
                Ok(Some("annotation")) => { 
                    ann = Some(map.next_value::<Ann>()?);
                },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        Ok(ImportItem(ann.unwrap(),module_name))
    }
}

impl<'a> Deserialize<'a> for ImportItem {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("ImportItem",&[""], ImportItemVisitor)
    }
}

////////////
struct AnnVisitor;
impl<'a> Visitor<'a> for AnnVisitor {
    type Value = Ann;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse AnnVisitor error")
    }

    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut source_span = None;
        loop {
            match map.next_key() {
                Ok(Some("sourceSpan")) => { source_span = Some(map.next_value::<SourceSpan>()?); },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        Ok(Ann(source_span.unwrap()))
    }
}

impl<'a> Deserialize<'a> for Ann {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Ann",&[""], AnnVisitor)
    }
}


#[test]
fn test_json() {
    let json = std::fs::read_to_string("tests/corefn.json").unwrap();
    let pos:Module = serde_json::from_str(json.as_str()).unwrap();
    dbg!(pos.imports); 
}
