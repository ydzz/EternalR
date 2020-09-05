use ast::types::Module;
use ast::types::{self,Bind,Expr,Ann,Type};
use crate::translate::{Translate,TTypeInfo};
use std::collections::HashMap;
use gluon::base::types::{Field,ArcType};

pub struct GrabTypeInfo {
    type_dic:HashMap<String,ArcType>
}

impl GrabTypeInfo {
    pub fn add_type(&mut self,name:&str,typ:ArcType) {
        self.type_dic.insert(name.to_string(), typ);
    }
}

impl Default for GrabTypeInfo {
    fn default() -> Self {
        GrabTypeInfo {type_dic : HashMap::new() }
    }
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn grab_type_info(&self,module:&mut Module) -> GrabTypeInfo {
        let mut ret_type_info = GrabTypeInfo::default();
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

        for (k,item) in cache_map.iter() {
            let mut fields = vec![];
            let mut type_vars:Vec<String> = vec![];
            for (ctor_name,names,typs) in item {
                let mut gtypes:Vec<TTypeInfo> = vec![];
                for typ in typs {
                    type_vars.extend(self.search_type_var(typ));
                    gtypes.push(self.translate_type(typ).unwrap());
                } 
                let args:Vec<_> = gtypes.iter().map(|t| t.typ.clone()).collect();
               
                let sym = self.simple_symbol(ctor_name.as_str());
                let field = if names.len() == 0 {
                    Field::new(sym, self.type_cache.opaque())
                }  else {
                    Field::ctor( sym, args)
                };
                fields.push(field);
            }
            let var_type = self.type_cache.variant(fields);
            ret_type_info.add_type(k.as_str(), var_type);
        }
        ret_type_info
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