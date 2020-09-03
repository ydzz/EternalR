use ast::types::Module;
use ast::types::{self,Bind,Expr,Ann,Type};
use crate::translate::Translate;
use std::collections::HashMap;
pub struct GrabTypeInfo {

}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn grab_type_info(&self,module:&mut Module) -> GrabTypeInfo {
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
        
        dbg!(cache_map);
        module.decls = new_decls;
        GrabTypeInfo {}
    }
}