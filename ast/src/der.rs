use serde::{Deserialize,Deserializer,de::{Visitor,MapAccess,IgnoredAny}};
use std::fmt;
use crate::types::*;
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
        let mut typ = None;
        loop {
            match map.next_key() {
                Ok(Some("sourceSpan")) => { source_span = Some(map.next_value::<SourceSpan>()?); },
                Ok(Some("meta")) => { meta = map.next_value::<Option<Meta>>()?; },
                Ok(Some("type")) => { typ = map.next_value::<serde_json::Value>().ok(); },
                Ok(Some(_)) =>{ let _ = map.next_value::<IgnoredAny>();},
                _ => break
            }
        }
        Ok(Ann(source_span.unwrap(),meta,ann_type_from_value(&typ.unwrap())))
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
                    let val = map.next_value::<serde_json::Value>()?;
                    expr = expr_from_value(&val);
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


fn qualified_from_value<A>(f:impl Fn(&str) -> A,val:&serde_json::Value) -> Option<Qualified<A>> {
    let val_obejct = val.as_object()?;
    let id_str = val_obejct.get("identifier")?.as_str()?;
    let module_name:Option<String> = val_obejct.get("moduleName")?.as_array()
                                               .and_then(|v| v.iter().map(|sv| sv.as_str()).collect())
                                               .map(|arr| join_module_name(arr) );
   
    Some(Qualified(module_name,f(id_str)))
}

fn literal_from_value<T>(f:impl Fn(&serde_json::Value) -> T,val:&serde_json::Value) ->Option<Literal<T>> {
    let val_object = val.as_object()?;
    let value = val_object.get("value")?;
    match val_object.get("literalType")?.as_str()? {
        "IntLiteral" => Some(Literal::NumericLiteral(Ok(value.as_i64()? as i32))),
        "NumberLiteral" => Some(Literal::NumericLiteral(Err(value.as_f64()?))),
        "StringLiteral" => Some(Literal::StringLiteral(value.as_str()?.to_string())),
        "CharLiteral" => {
            let a = value.as_str()?.chars().next().unwrap();
            Some(Literal::CharLiteral(a))
        }
        "BooleanLiteral" => Some(Literal::BooleanLiteral(value.as_bool()?)),
        "ArrayLiteral" => {
            let arr = value.as_array()?;
            let exprs:Vec<T> = arr.iter().map(f).collect();
            Some(Literal::ArrayLiteral(exprs))
        },
        "ObjectLiteral" => {
            Some(Literal::ObjectLiteral(object_from_value(f,value)?))
        }
        _ => None
    }
}

fn object_from_value<T>(f:impl Fn(&serde_json::Value) -> T,val:&serde_json::Value) -> Option<Vec<(String,T)>>  {
    let arr = val.as_array()?;
    let mut object = vec![];
    for item in arr {
        let item_arr = item.as_array()?;
        let str_key = item_arr[0].as_str()?;
        object.push((str_key.to_string(), f(&item_arr[1])));
    }
    Some(object)
}



fn expr_from_value(val:&serde_json::Value) -> Option<Expr<Ann>> {
    
    let val_object = val.as_object()?;
    let expr_type=  val_object.get("type")?.as_str()?;
    let ann = ann_from_value(val_object.get("annotation")?)?;
   
    match expr_type {
        "Var" => Some(Expr::Var(ann,qualified_from_value(|s| Ident::Ident(s.to_string()),val_object.get("value")?)? )) ,
        "Literal" => {
            let literal = literal_from_value(|v|Box::new(expr_from_value(v).unwrap()),val_object.get("value")?).unwrap();
            Some(Expr::Literal(ann,literal))
        },
        "Constructor" => {
            let proper_name = ProperName::TypeName(val_object.get("typeName")?.as_str()?.to_string());
            let constructor_name = ProperName::ConstructorName(val_object.get("constructorName")?.as_str()?.to_string());
            let fileds:Vec<Ident> = val_object.get("fieldNames")?.as_array()?.iter()
                                   .map(|v| {
                                      Ident::Ident(v.as_str().unwrap().to_string())
                                    }).collect();
            Some(Expr::Constructor(ann,proper_name,constructor_name,fileds))
        },
        "Accessor" => {
            let field = val_object.get("fieldName")?.as_str()?.to_string();
            let expr = expr_from_value(val_object.get("expression")?)? ;
            Some(Expr::Accessor(ann,field,Box::new(expr)))
        },
        "ObjectUpdate" => {
            let expr = expr_from_value(val_object.get("expression")?)? ;
            let updates = object_from_value(|v| Box::new(expr_from_value(v).unwrap()),val_object.get("updates")?)?;
            Some(Expr::ObjectUpdate(ann,Box::new(expr),updates))
        },
        "Abs" => {
            let argument = Ident::Ident(val_object.get("argument")?.as_str()?.to_string());
            let body_expr = expr_from_value(val_object.get("body")?)?;
            Some(Expr::Abs(ann,argument,Box::new(body_expr))) 
        },
        "App" => {
            let abstraction = expr_from_value(val_object.get("abstraction")?)?;
            let argument = expr_from_value(val_object.get("argument")?)?;
            Some(Expr::App(ann,Box::new(abstraction),Box::new(argument)))
        },
        "Case" => {
            let case_exprs:Vec<Expr<Ann>> = val_object.get("caseExpressions")?
                                                      .as_array()?.iter()
                                                      .map(|v| expr_from_value(v).unwrap()).collect();
            
            let case_alter = val_object.get("caseAlternatives")?.as_array()?
                                                                .iter().map(|v| case_alternative_from_value(v).unwrap()).collect();
            Some(Expr::Case(ann,case_exprs,case_alter))
        },
        "Let" => {
            let expr = expr_from_value(val_object.get("expression")?)?;
            let binds = val_object.get("binds")?.as_array()?.iter().map(|v| bind_from_value(v).unwrap()).collect();
            Some(Expr::Let(ann,binds,Box::new(expr)))
        },
         _ => None
       }
}

fn case_alternative_from_value(val:&serde_json::Value) -> Option<CaseAlternative<Ann>> {
    let binders:Vec<Binder<Ann>> = val.get("binders")?.as_array()?.iter().map(|v| binder_from_value(v).unwrap()).collect();
    let is_guarded = val.get("isGuarded")?.as_bool()?;
    if is_guarded {
        let arr = val.get("expressions")?.as_array()?;
        let gexprs:Vec<(Guard<Ann>,Expr<Ann>)> = arr.iter().map(|v| {
           let guard_expr = expr_from_value(v.get("guard").unwrap()).unwrap();
           let expression = expr_from_value(v.get("expression").unwrap()).unwrap();
           (guard_expr,expression)
        }).collect();
        Some(CaseAlternative {
            alternative_binders:binders,
            alternative_result:Ok(gexprs)
        })
    } else {
        let expression = expr_from_value(val.get("expression")?)?;
        Some(CaseAlternative {
            alternative_binders:binders,
            alternative_result:Err(expression)
        })
    }
}

fn binder_from_value(val:&serde_json::Value) -> Option<Binder<Ann>> {
    let binder_type = val.get("binderType")?.as_str()?;
    let ann = ann_from_value(val.get("annotation")?)?;
    match binder_type {
        "NullBinder" => Some(Binder::NullBinder(ann)),
        "VarBinder" => {
            let ident = Ident::Ident(val.get("identifier")?.as_str()?.to_string());
            Some(Binder::VarBinder(ann,ident))
        },
        "LiteralBinder" => {
            let literal = literal_from_value(|f| Box::new(binder_from_value(f).unwrap()) , val.get("literal")?)?;
            Some(Binder::LiteralBinder(ann,literal))
        },
        "ConstructorBinder" => {
            let type_name =  qualified_from_value(|s|ProperName::TypeName(s.to_string()), val.get("typeName")?)?;
            let constructor_name = qualified_from_value(|s| ProperName::ConstructorName(s.to_string()),val.get("constructorName")?)?;
            let binders:Vec<Binder<Ann>> = val.get("binders")?.as_array()?.iter().map(|v| binder_from_value(v).unwrap() ).collect();
            Some(Binder::ConstructorBinder(ann,type_name,constructor_name,binders))
        },
        "NamedBinder" => {
            let ident = Ident::Ident(val.get("identifier")?.as_str()?.to_string());
            let bind = binder_from_value(val.get("binder")?)?;
            Some(Binder::NamedBinder(ann,ident,Box::new(bind)))
        },
        _ => None
    }
}

fn ann_from_value(val:&serde_json::Value) -> Option<Ann> {
    let val_object = val.as_object()?;
    let span = source_span_from_value(val_object.get("sourceSpan")?)? ;
    let meta = val_object.get("meta").and_then(|m| meta_from_value(m));
    Some(Ann(span,meta,ann_type_from_value(val_object.get("type")?)))
}

fn meta_from_value(val:&serde_json::Value) -> Option<Meta> {
    let val_object = val.as_object()?;
    match val_object.get("metaType")?.as_str()? {
        "IsForeign" => Some(Meta::IsForeign),
         "IsNewtype" => Some(Meta::IsNewtype),
         "IsTypeClassConstructor" => Some(Meta::IsTypeClassConstructor),
         "IsWhere" => Some(Meta::IsWhere),
         "IsConstructor" => {
             let identifiers = val_object.get("identifiers")?.as_array()?;
             let idents:Vec<Ident> = identifiers.iter().map(|id| Ident::Ident(id.as_str().unwrap().to_string())).collect();
             let ctpye = if val_object.get("constructorType")?.as_str()? == "ProductType" {
                 ConstructorType::ProductType
             } else {
                 ConstructorType::SumType
             };
             Some(Meta::IsConstructor(ctpye,idents))
         },
        _ => None
    }
}

fn source_span_from_value(val:&serde_json::Value) -> Option<SourceSpan> {
    let val_object = val.as_object()?;
    let start:Vec<i32> = val_object.get("start")?.as_array()?.iter().map(|v| v.as_i64().unwrap() as i32).collect();
    let end:Vec<i32> = val_object.get("end")?.as_array()?.iter().map(|v| v.as_i64().unwrap() as i32).collect();
    Some(SourceSpan {
        name:String::default(),
        start:SourcePos::new(start[0], start[1]),
        end:SourcePos::new(end[0], end[1]),
    })
}

fn bind_from_value(val:&serde_json::Value) -> Option<Bind<Ann>> {
    match val.get("bindType")?.as_str()? {
        "NonRec" => {
            let identifier = Ident::Ident(val.get("identifier")?.as_str()?.to_string());
            let ann = ann_from_value(val.get("annotation")?)?;
            let expr =expr_from_value(val.get("expression")?)?;
            Some(Bind::NonRec(ann,identifier,Box::new(expr)))
        },
        "Rec" => {
            let lst = val.get("binds")?.as_array()?.iter().map(|v| {
                let identifier = Ident::Ident(v.get("identifier").unwrap().as_str().unwrap().to_string());
                let ann = ann_from_value(v.get("annotation").unwrap()).unwrap();
                let expr = expr_from_value(v.get("expression").unwrap()).unwrap();
                ((ann,identifier),Box::new(expr))
            }).collect();
            Some(Bind::Rec(lst))
        },
        _ => None
    }
}

fn ann_type_from_value(val:&serde_json::Value) -> Option<Type<()>> {
    let val_object = val.as_object()?;
    let tag = val_object.get("tag")?.as_str()?;
    let contents = val_object.get("contents");
    match tag {
        "TUnknown" => Some(Type::TUnknown((),contents?.as_i64()? as i32)),
        "TypeVar" => Some(Type::TypeVar((),contents?.as_str()?.to_string())),
        "TypeLevelString" => Some(Type::TypeLevelString((),contents?.as_str()?.to_string())),
        "TypeWildcard" => {
            Some(Type::TypeWildcard((),contents.and_then(|c| c.as_str().map(|s| s.to_string()))))
         },
         "TypeConstructor" => {
            let qual = type_qual_from_value(contents?, |s| ProperName::TypeName(s))?;
            Some(Type::TypeConstructor((),qual))
         },
         "TypeOp" => {
            let qual = type_qual_from_value(contents?, |s| OpName::TypeOpName(s))?;
            Some(Type::TypeOp((),qual))
         },
         "TypeApp" => {
             let arr = contents?.as_array()?;
             let ta = ann_type_from_value(&arr[0])?;
             let tb = ann_type_from_value(&arr[1])?;
             Some(Type::TypeApp((),Box::new(ta),Box::new(tb)))
         },
         "KindApp" => {
            let arr = contents?.as_array()?;
            let ta = ann_type_from_value(&arr[0])?;
            let tb = ann_type_from_value(&arr[1])?;
            Some(Type::KindApp((),Box::new(ta),Box::new(tb)))
         },
         "ForAll" => {
            let arr = contents?.as_array()?;
            match arr.len() {
                3 => {
                    let str = arr[0].as_str()?.to_string();
                    let ty =  ann_type_from_value(&arr[1])?;
                    let e = arr[2].as_i64().map(|n| n as i32);
                    Some(Type::ForAll((),str,None,Box::new(ty),e))
                },
                4 => {
                    let str = arr[0].as_str()?.to_string();
                    let ty =  ann_type_from_value(&arr[1])?;
                    let ty2 =  ann_type_from_value(&arr[1])?;
                    let e = arr[2].as_i64().map(|n| n as i32);
                    Some(Type::ForAll((),str,Some(Box::new(ty)),Box::new(ty2),e))
                },
                _ => None
            }
         },
         "ConstrainedType" => {
             let arr = contents?.as_array()?;
             let con = constraint_from_value(&arr[0])?;
             let ty = ann_type_from_value(&arr[1])?;
             Some(Type::ConstrainedType((),con,Box::new(ty)))
         },
         "Skolem" => {
            let arr = contents?.as_array()?;
            let str = arr[0].as_str()?.to_string();
            let typ = ann_type_from_value(&arr[1]);
            let i = arr[2].as_i64()?;
            let i2 = arr[3].as_i64()?;
            Some(Type::Skolem((),str,typ.map(|t| Box::new(t)),i as i32,i2 as i32))
         }
         "REmpty" =>   Some(Type::REmpty(())),
         "RCons" => {
            let arr = contents?.as_array()?;
            let label = arr[0].as_str()?.to_string();
            let typ = ann_type_from_value(&arr[1])?;
            let typ2 = ann_type_from_value(&arr[2])?;
            Some(Type::RCons((),label,Box::new(typ),Box::new(typ2)))
         },
         "KindedType" => {
            let arr = contents?.as_array()?;
            let ta = ann_type_from_value(&arr[0])?;
            let tb = ann_type_from_value(&arr[1])?;
            Some(Type::KindedType((),Box::new(ta),Box::new(tb)))
         },
         "BinaryNoParensType" => {
            let arr = contents?.as_array()?;
            let ta = ann_type_from_value(&arr[0])?;
            let tb = ann_type_from_value(&arr[1])?;
            let tc = ann_type_from_value(&arr[2])?;
            Some(Type::BinaryNoParensType((),Box::new(ta),Box::new(tb),Box::new(tc)))
         },
         "ParensInType" => {
            let ty = ann_type_from_value(contents?)?;
            Some(Type::ParensInType((),Box::new(ty)))
         },
         "KUnknown" => Some(Type::TUnknown((),contents?.as_i64()? as i32)),
         "Row" => {
            let ty = ann_type_from_value(contents?)?;
            let c =Type::TypeConstructor((),prim_row());
            Some(Type::TypeApp((),Box::new(c),Box::new(ty)))
         },
         "NamedKind" => {
             let qual = type_qual_from_value(contents?,|s| ProperName::TypeName(s))?;
             Some(Type::TypeConstructor((),qual))
         },
         "FunKind" => {
            let arr = contents?.as_array()?;
            let ta = ann_type_from_value(&arr[0])?;
            let tb = ann_type_from_value(&arr[1])?;
            let app = Type::TypeApp((),Box::new(Type::TypeConstructor((),prim_function())),Box::new(ta));
            Some(Type::TypeApp((),Box::new(app),Box::new(tb)))
         }
        _ => None
    }
}

fn constraint_from_value(val:&serde_json::Value) -> Option<Constraint<()>> {
    let class = val.get("constraintClass")?;
    let pname = type_qual_from_value(class, |s| ProperName::ClassName(s))?;
    let args:Vec<_> = val.get("constraintArgs")?.as_array()?.iter().map(|t| ann_type_from_value(t).unwrap() ).collect();
    let kind_args:Vec<_> = val.get("constraintKindArgs")
                            .and_then(|v| v.as_array())
                            .map(|varr| 
                                varr.iter().map(|v| ann_type_from_value(v).unwrap()).collect()
                            ).unwrap_or(vec![]);

    let cdata = val.get("constraintData")?.as_object().map(|obj| {
        let contents_array = obj.get("contents").unwrap().as_array().unwrap();
        let str_list:Vec<Vec<String>> = contents_array[1].as_array().unwrap().iter()
                                        .map(|arr| arr.as_array().unwrap().iter()
                                        .map(|v| v.as_str().unwrap().to_string()).collect() ).collect();
        let b = contents_array[1].as_bool().unwrap();
        ConstraintData::PartialConstraintData(str_list,b)
    });
    Some(Constraint {
        ann:(),
        class:pname,
        kind_args,
        args,
        data:cdata
    })
}

fn type_qual_from_value<T>(val:&serde_json::Value,f:fn(String) -> T) -> Option<Qualified<T>>  {
    let arr = val.as_array()?;
    let qual_arr:Vec<&str> = arr[0].as_array()?.iter().map(|v| v.as_str().unwrap()).collect();
    let qname = join_module_name(qual_arr);
    let name = arr[1].as_str()?.to_string();
    Some(Qualified(Some(qname),f(name))) 
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
                    let value = map.next_value::<serde_json::Value>()?;
                    expr = expr_from_value(&value);
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
 
    let json = std::fs::read_to_string("tests/corefn2.json").unwrap();
    let pos:Module = serde_json::from_str(json.as_str()).unwrap();
    dbg!(&pos);
    let debug_str = format!("{:?}",pos);
    std::fs::write("a.out", debug_str);
    
}
