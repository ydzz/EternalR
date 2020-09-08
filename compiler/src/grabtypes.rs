use ast::types::Module;
use ast::types::{self,Bind,Expr,Ann,Type};
use crate::translate::{Translate,TTypeInfo};
use std::collections::HashMap;
use std::cell::RefCell;
use gluon::base::symbol::Symbol;
use gluon::base::types::{Field,ArcType,Generic};
use gluon::base::kind::Kind;
use gluon::base::types::TypeExt;
use std::sync::Arc;
use gluon::base::types::{Type as VMType};
use gluon::base::ast::{TypedIdent};
#[derive(Default)]
pub struct TypInfoEnv {
   pub type_dic: RefCell<HashMap<String, Arc<TypeInfo>>>
}

impl TypInfoEnv {
    pub fn add_type_info(&self,type_info:TypeInfo) {
        self.type_dic.borrow_mut().insert(type_info.qual_type_name.clone(), Arc::new(type_info));
    }
    
    pub fn get_bool_type(&self) -> Arc<TypeInfo> {
        self.type_dic.borrow().get("prim.Bool").unwrap().clone()
    }

    pub fn bool_constructor(&self,b:bool) -> TypedIdent<Symbol> {
       let type_info = self.get_bool_type();
       let  bool_type = type_info.gluon_type2.as_ref().unwrap().clone();
       match *bool_type {
           VMType::Variant(ref variants) => {
               let sym_name = variants.row_iter().nth(b as usize).unwrap().name.clone();
               TypedIdent {
                   name:sym_name,
                   typ:bool_type.clone()
               }
           },
           _ => panic!()
       }
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    pub qual_type_name:String,
    pub type_name:String,
    pub  gluon_type:ArcType,
    pub  gluon_type2:Option<ArcType>,
    pub type_str_vars:Vec<String>,
    pub type_vars:Vec<Generic<Symbol>>
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn grab_type_info(&self,module:&mut Module)  {
        let mut new_decls:Vec<Bind<Ann>> = vec![];
        let mut cache_map:HashMap<String,Vec<(String,Vec<String>,Vec<Type<()>>) >> = HashMap::new();
        
        for decl in module.decls.drain(0..) {
            match decl {
                Bind::NonRec(ann,ident,expr) => {
                    let take_expr:Expr<Ann> = *expr;
                    match take_expr {
                        Expr::Constructor(_,p0,p1,idents,types) => {
                            let type_name = types::proper_name_as_str(&p0).to_string();
                            let ctor_name = types::proper_name_as_str(&p1);
                            let ident_strs:Vec<String> = idents.iter().map(|i| String::from(i.as_str().unwrap())).collect();
                            if !cache_map.contains_key(&type_name) {
                                cache_map.insert(type_name.clone(), vec![]);
                            }
                            let list = cache_map.get_mut(&type_name).unwrap();
                            list.push((ctor_name.to_string(),ident_strs,types));
                            ()
                        },  
                       e => {
                           new_decls.push(Bind::NonRec(ann,ident,Box::new(e)));
                       }
                    }
                }
                bind => new_decls.push(bind)
            }
        }
        module.decls = new_decls;

        //collect type
        for (k,item) in cache_map.iter() {
            let mut fields = vec![];
            let mut type_vars:Vec<String> = vec![];
            for (ctor_name,names,typs) in item {
                let mut gtypes:Vec<TTypeInfo> = vec![];
                for typ in typs {
                    type_vars.extend(self.search_type_var(typ));
                    gtypes.push(self.translate_type(typ).unwrap());
                } 
                let args:Vec<_> = gtypes.iter().map(|t| t.typ()).collect();
               
                let sym = self.simple_symbol(ctor_name.as_str());
                let field = if names.len() == 0 {
                    Field::new(sym, self.type_cache.opaque())
                }  else {
                    Field::ctor( sym, args)
                };
                fields.push(field);
            }
            let var_type = self.type_cache.variant(fields);
            let mut qual_type_name = module.name.clone();
            qual_type_name.push('.');
            qual_type_name.push_str(k.as_str());
            let mut g_type_vars:Vec<Generic<Symbol>> = vec![];
            for var in type_vars.iter() {
                let sym = self.simple_symbol(var.as_str());
                g_type_vars.push(Generic::new(sym, Kind::typ()) );
            }
            let type_info = TypeInfo { 
                qual_type_name, 
                type_name:k.to_string(),
                gluon_type:var_type,
                gluon_type2:None,
                type_str_vars:type_vars,
                type_vars:g_type_vars
            };
            self.type_env.add_type_info(type_info);
        }
        
    }

    fn search_type_var(&self,typ:&Type<()>) -> Vec<String> {
        let mut vars:Vec<String> = vec![];
        match typ {
            Type::TypeVar(_,var) => {
                vars.push(var.to_string());
                  
            },
            Type::TypeApp(_,a,b) => {
                vars.extend(self.search_type_var(a));
                vars.extend(self.search_type_var(b));   
            }
            _ => ()
        }
       
        vars
    }
}