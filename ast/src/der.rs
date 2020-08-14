use serde::{Deserialize,Deserializer,de::{Visitor,MapAccess,IgnoredAny}};
use std::fmt;
use crate::types::{Module,Literal,Comment,SourceSpan,Ident,Ann,ImportItem,Meta,ConstructorType,Bind,Expr};
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
       let mut foreign = vec![];
       let mut decls = vec![];
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
              Ok(Some("decls")) => {  decls = vmap.next_value::<Vec<Bind<Ann>>>()?; },
              Ok(Some("exports")) => { 
                  let exports_str = vmap.next_value::<Vec<&str>>()?;
                  exports = exports_str.iter().map(|e| Ident::Ident(e.to_string())).collect();
              },
              Ok(Some("foreign")) => { 
                foreign =  vmap.next_value::<Vec<&str>>()?.iter().map(|e| Ident::Ident(e.to_string())).collect();
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
          imports,
          foreign,
          decls
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
        let mut meta = None;
        loop {
            match map.next_key() {
                Ok(Some("sourceSpan")) => { source_span = Some(map.next_value::<SourceSpan>()?); },
                Ok(Some("meta")) => { meta = map.next_value::<Option<Meta>>()?; },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        Ok(Ann(source_span.unwrap(),meta))
    }
}

impl<'a> Deserialize<'a> for Ann {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Ann",&[""], AnnVisitor)
    }
}
////////////
struct MetaVisitor;
impl<'a> Visitor<'a> for MetaVisitor {
    type Value = Meta;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse MetaVisitor error")
    }
    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut idents = None;
        let mut constructor_type = None;
        let mut is_constructor = false;
        let mut meta_type = Meta::IsForeign;
        loop {
            match map.next_key() {
                Ok(Some("metaType")) => {
                    let type_str = map.next_value::<String>()?;
                    match type_str.as_str() {
                        "IsForeign" => meta_type = Meta::IsForeign,
                        "IsNewtype" => meta_type = Meta::IsNewtype,
                        "IsTypeClassConstructor" => meta_type = Meta::IsTypeClassConstructor,
                        "IsWhere" => meta_type = Meta::IsWhere,
                        "IsConstructor" => is_constructor = true,
                        _ => ()
                    }
                },
                Ok(Some("identifiers")) => {
                    let arr = map.next_value::<Vec<&str>>()?;
                    idents = Some(arr.iter().map(|s| Ident::Ident(s.to_string())).collect());
                },
                Ok(Some("constructorType")) => {
                    let type_str = map.next_value::<String>()?;
                    match type_str.as_str() {
                        "ProductType" => constructor_type = Some(ConstructorType::ProductType),
                        _ => constructor_type = Some(ConstructorType::SumType),
                    }
                },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        if is_constructor {
            meta_type = Meta::IsConstructor(constructor_type.unwrap(),idents.unwrap());
        }
        Ok(meta_type)
    }
}

impl<'a> Deserialize<'a> for Meta {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Meta",&[""], MetaVisitor)
    }
}
////////////
struct BindVisitor;
impl<'a> Visitor<'a> for BindVisitor {
    type Value = Bind<Ann>;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse BindVisitor error")
    }

    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut bind_type= "";
        let mut ann = None;
        let mut ident = None;
        let mut expr = None;
        let mut rec_items = vec![];
        //let mut 
        loop {
            match map.next_key() {
                Ok(Some("bindType")) => {
                    bind_type = map.next_value::<&str>()?;
                },
                Ok(Some("annotation")) => {
                    ann = Some(map.next_value::<Ann>()?);
                },
                Ok(Some("identifier")) => {
                    ident = Some(Ident::Ident(map.next_value::<String>()?));
                },
                Ok(Some("expression")) => {
                    expr = Some(map.next_value::<Expr<Ann>>()?);
                },
                Ok(Some("binds")) => {
                    rec_items = map.next_value::<Vec<BindRecItem>>()?;
                },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        match bind_type {
            "NonRec" => {
                Ok(Bind::NonRec(ann.unwrap(),ident.unwrap(),Box::new(expr.unwrap()))) 
            },
            _ => {
                let list = rec_items.drain(0..).map(|item| {
                    ((item.ann,item.ident),Box::new(item.expr))
                }).collect();
                Ok(Bind::Rec(list))
            }
        }
    }
}
impl<'a> Deserialize<'a> for Bind<Ann> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Bind",&[""], BindVisitor)
    }
}
////////////
struct ExprVisitor;
impl<'a> Visitor<'a> for ExprVisitor {
    type Value = Expr<Ann>;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse ExprVisitor error")
    }
    
    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut expr_type = "";
        let mut ann = None;
        loop {
            match map.next_key() {
                Ok(Some("type")) =>{ expr_type = map.next_value::<&str>()?;},
                Ok(Some("annotation")) =>{ ann = Some(map.next_value::<Ann>()?);},
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
       //todo
       Ok(Expr::Literal(ann.unwrap(),Literal::StringLiteral(expr_type.to_string())))
    }
}

impl<'a> Deserialize<'a> for Expr<Ann> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("Expr",&[""], ExprVisitor)
    }
}
////////////
struct BindRecItem {
    expr:Expr<Ann>,
    ann:Ann,
    ident:Ident
}
struct BindRecItemVisitor;
impl<'a> Visitor<'a> for BindRecItemVisitor {
    type Value = BindRecItem;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("parse BindRecItemVisitor error")
    }

    fn visit_map<A>(self,mut map: A) -> Result<Self::Value, A::Error> where A: MapAccess<'a>, {
        let mut ann = None;
        let mut ident = "";
        let mut expr = None;
        loop {
            match map.next_key() {
                Ok(Some("annotation")) => {
                    ann = Some(map.next_value::<Ann>()?);
                },
                Ok(Some("identifier")) => {
                    ident = map.next_value::<&str>()?;
                }
                Ok(Some("expression")) => {
                    expr = Some(map.next_value::<Expr<Ann>>()?);
                },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        Ok(BindRecItem {expr: expr.unwrap(),ann:ann.unwrap(),ident:Ident::Ident(ident.to_string())}) 
    }
}

impl<'a> Deserialize<'a> for BindRecItem {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'a> {
        deserializer.deserialize_struct("BindRecItem",&[""], BindRecItemVisitor)
    }

}

#[test]
fn test_json() {
    let json = std::fs::read_to_string("tests/corefn.json").unwrap();
    let pos:Module = serde_json::from_str(json.as_str()).unwrap();
    dbg!(pos.decls); 
}
