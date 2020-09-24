use ast::types::Module;
use ast::types::{self,Bind,Expr,Ann,Type,Meta,Ident,Literal};
use crate::translate::{Translate,TTypeInfo};
use std::collections::HashMap;
use std::cell::RefCell;
use gluon::base::symbol::Symbol;
use gluon::base::types::{Field,ArcType,Generic};
use gluon::base::kind::{Kind,ArcKind};
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
    pub gluon_type:ArcType,
    pub gluon_type2:Option<ArcType>,
    pub type_str_vars:Vec<String>,
    pub type_vars:Vec<Generic<Symbol>>
}
#[derive(Debug)]
enum TransferKT {
    Type,
    Function
}

pub struct TypeClassInfo {
    args:Vec<Generic<Symbol>>
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn grab_type_info(&self,module:&mut Module)  {
        let mut new_decls:Vec<Bind<Ann>> = vec![];
        let mut cache_map:HashMap<String,Vec<(String,Vec<String>,Vec<Type<()>>) >> = HashMap::new();
        let mut cache_typeclass:HashMap<String,(Vec<String>,Vec<String>)> = HashMap::new();
        for decl in module.decls.drain(0..) {
            match decl {
                Bind::NonRec(bind_ann,ident,expr) => {
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
                        Expr::Abs(ann,id,expr) => {
                            match &ann.1 {
                                Some(Meta::IsTypeClassConstructor) => {
                                   self.set_type_class_info(&bind_ann,&expr,&ident,module.name.as_str(),&mut cache_typeclass);
                                },
                                _ => {
                                    let bexpr = Box::new(Expr::Abs(ann,id,expr));
                                    new_decls.push(Bind::NonRec(bind_ann,ident,bexpr));
                                }
                            }
                        },
                        Expr::App(ann,a,b) => {
                            let bexpr = Box::new(Expr::App(ann,a,b));
                            new_decls.push(Bind::NonRec(bind_ann,ident,bexpr));
                        },
                       e => {
                           new_decls.push(Bind::NonRec(bind_ann,ident,Box::new(e)));
                       }
                    }
                }
                bind => new_decls.push(bind)
            }
        }
        module.decls = new_decls;
       self.collect_data_ctors(cache_map,module);
       self.collect_typeclass(&mut cache_typeclass, &module.decls);
    
    }

    fn collect_data_ctors(&self,cache_map:HashMap<String,Vec<(String,Vec<String>,Vec<Type<()>>) >>,module:&mut Module) {
        //collect type
        for (k,item) in cache_map.iter() {
            let mut fields = vec![];
            let mut type_vars:Vec<String> = vec![];
            for (ctor_name,names,typs) in item {
                let mut gtypes:Vec<TTypeInfo> = vec![];
                for typ in typs {
                    type_vars.extend(self.search_type_var(typ));
                    gtypes.push(self.translate_type(typ,None).unwrap());
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

    fn set_type_class_info(&self,ann:&Ann,expr:&Expr<Ann>,ident:&Ident,module_name: &str,maps:&mut HashMap<String,(Vec<String>,Vec<String>)>) {
        let mut class_var_names:Vec<_> = vec![];
        if let Some(Type::TypeVar(_,strings)) =   ann.2.as_ref() {
            class_var_names = strings.split(",").map(|s| s.to_string()).collect();
        };
        let mut class_func_names:Vec<String> = vec![];
        if let Expr::Literal(_,Literal::ObjectLiteral(lit_vec)) = expr {
            for (func_name,_) in lit_vec {
                class_func_names.push(func_name.to_owned());
            }
        }
        let mut typeclass_name = String::from(module_name);
        typeclass_name.push('.');
        typeclass_name.push_str(self.id2str(ident).unwrap());
        maps.insert(typeclass_name, (class_var_names,class_func_names));
       
        /*
       
        let ann_type = ann.2.as_ref().unwrap();
        let ann_type_var:Vec<String> = match ann_type {
            Type::TypeVar(_,strings) => strings.split(",").map(|s| s.to_string()).collect(),
            _ => panic!("typeclass need type var")
        };

        let typ = self.find_type_class_type(expr);
        let id_name = self.id2str(ident).unwrap();
        module_name.push('.');
        module_name.push_str(id_name);
        let sym_name = self.simple_symbol(module_name.as_str());
        let mut g_type_vars:Vec<Generic<Symbol>> = vec![];
        for var in ann_type_var.iter() {
            let sym = self.simple_symbol(var.as_str());
            g_type_vars.push(Generic::new(sym, Kind::typ()) );
        }
        let arctyp:ArcType = typ.into();
        let vm_type:ArcType = VMType::alias(sym_name,g_type_vars.clone(), arctyp.clone()).into();
        let type_info = TypeInfo {
            qual_type_name:module_name,
            type_name:id_name.to_string(),
            gluon_type:vm_type,
            gluon_type2:Some(arctyp),
            type_str_vars:ann_type_var,
            type_vars:g_type_vars
        };
        dbg!("add typevar {:?}",&type_info);
        self.type_env.add_type_info(type_info);*/
    }


    fn collect_typeclass(&self,maps:&mut HashMap<String,(Vec<String>,Vec<String>)>,decls:&Vec<Bind<Ann>>) {
        let mut typeclass_args_dic:HashMap<String,Vec<Generic<Symbol>>> = HashMap::new();
        for bind_item in decls {
            match bind_item {
                Bind::NonRec(_,bind_ident,expr) => {
                   if let Expr::Abs(ann,ident,_) = &**expr {
                       if let Some(Meta::IsTypeClassMember)  = ann.1 {
                           let ident_str = ident.as_str().unwrap();
                           if maps.contains_key(ident_str) {
                               let func_name = bind_ident.as_str().unwrap();
                               let dic = self.forall_kind_dic(&ann.2.as_ref().unwrap());
                               if typeclass_args_dic.contains_key(ident_str) == false {
                                   let mut args = vec![];
                                   for var_name in maps.get(ident_str).unwrap().0.iter() {
                                      let kind = dic.get(var_name).unwrap();
                                      let generic = Generic::new(self.simple_symbol(var_name), kind.clone());
                                      args.push(generic);
                                   }
                                   typeclass_args_dic.insert(ident_str.to_string(), args);
                               }
                               let func_type = self.translate_type(&ann.2.as_ref().unwrap(),None);
                               dbg!(func_type);
                           }
                       }
                   }
                }
                _ => ()
            }
        }

        dbg!(typeclass_args_dic);
    }

    pub(crate) fn forall_kind_dic<T>(&self,typ:&Type<T>) -> HashMap<String,ArcKind> {
        let mut kind_map = HashMap::new();
        let mut cur_type = typ;
        loop {
            match cur_type {
                Type::ForAll(_,name,vart,t,_) => {
                    let kind = self.translate_kind_type(vart.as_ref().unwrap());
                    kind_map.insert(name.to_string(), kind);
                    cur_type = t
                },
                _ => break
            }
        }
        kind_map
    }

    

    fn translate_kind_type<T>(&self,typ:&Type<T>) -> ArcKind {
       let array = self.flat_kind_type(typ);
       let mut index = 0usize;
       fn parse(array:&Vec<TransferKT>,index:&mut usize) -> ArcKind {
           let cur = &array[*index];
           match cur {
               TransferKT::Type => {
                   *index = *index + 1;
                   Kind::typ()
               },
               TransferKT::Function => parse_function(array,index)
               
           }
       }
       fn parse_function(array:&Vec<TransferKT>,index:&mut usize) -> ArcKind {
           *index = *index + 1;
           let head = parse(array,index);
           let tail = parse(array,index);
           Kind::function(head, tail)
       }
       parse(&array,&mut index)
    }

    fn flat_kind_type<T>(&self,typ:&Type<T>) -> Vec<TransferKT> {
        let mut items:Vec<TransferKT> = vec![];
        let mut cur_type = typ;
        loop {
            match cur_type {
                Type::TypeApp(_,l,r) => {
                    items.extend(self.flat_kind_type(l));
                    cur_type = r;
                },
                Type::TypeConstructor(_,qual) => {
                   let name = types::proper_name_as_str(&qual.1);
                   if name == "Type" {
                       items.push(TransferKT::Type);
                   } else if name == "Function" {
                       items.push(TransferKT::Function);
                   }
                   break;
                },
                _ => break
            }
        }
        items
    }
/*
    fn find_type_class_type(&self,expr:&Expr<Ann>) -> VMType<Symbol> {
        let mut cur_expr = expr;
        let mut fields:Vec<Field<Symbol>> = vec![];
        loop {
            match cur_expr {
                Expr::Abs(_,_,abs) => {
                    cur_expr = abs;
                },
                Expr::Literal(_,lit) => {
                    match lit {
                        Literal::ObjectLiteral(vec) => {
                            for (field_name,filed_expr) in vec {
                                let expr_ref:&Expr<Ann> = filed_expr;
                                match expr_ref {
                                    Expr::Var(ann,_) => {
                                        let field:Field<Symbol> = self.type_class_member_to_field(field_name.as_str(),&ann.2.as_ref().unwrap());
                                        fields.push(field);
                                    },
                                    _ => panic!()
                                }
                            }
                        },
                        _ => panic!()
                    }
                    break;
                }
                _ => panic!()
            }
        }
     
        let row =  VMType::extend_row(fields, self.type_cache.empty_row.clone());
        let record = VMType::Record(row);
        record
    }

    fn type_class_member_to_field(&self,name:&str,typ:&Type<()>) -> Field<Symbol> {
        let type_apps = self.flat_app_type(typ);
        let mut types = vec![];
        for  typ in type_apps {
            match typ {
                Err(_) => panic!(),
                Ok(ty) => {
                    types.push(ty.typ());
                }
            }
        }
        //dbg!(&types);
        let sym = self.simple_symbol(name);
        if types.len() == 1 {
            Field {
                typ:types.remove(0),
                name:sym
            }
        } else {
            let last = types.pop().unwrap();
            let func = self.type_cache.function(types, last);
            Field {
                typ:func,
                name:sym
            }
        }
    }*/
}