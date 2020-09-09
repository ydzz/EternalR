use gluon::vm::core::{Allocator,Pattern,Alternative,Named};
use gluon::base::types::{ArcType,TypeCache,Field,KindedIdent,Type,Generic};
use gluon::base::symbol::{Symbol,Symbols,SymbolData};
use gluon::base::ast::{TypedIdent};
use gluon::base::{kind::Kind};
use gluon::base::pos::{BytePos};
use gluon::vm::core::{Expr as VMExpr,Closure,Literal as VMLiteral,LetBinding};
use std::cell::RefCell;
use gluon::base::pos::{Span,ByteIndex};
use ast::{ExternsFile};
use ast::types::{self,Module,Type as AstType,Literal,Expr,Bind,Ident,Ann,Binder};
use crate::errors::TranslateError;
use gluon::ModuleCompiler;
use crate::utils::*;
use std::sync::Arc;
use gluon::query::{AsyncCompilation};
use crate::grabtypes::TypInfoEnv;


pub struct Translate<'vm,'alloc>{
   pub alloc:&'alloc Allocator<'alloc>,
   pub type_cache:&'vm TypeCache<Symbol,ArcType>,
   pub symbols: RefCell<Symbols>,
   pub type_env:Arc<TypInfoEnv>
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn new(alloc:&'alloc Allocator<'alloc>,type_cache:&'vm TypeCache<Symbol,ArcType>,type_env:Arc<TypInfoEnv>) -> Self {
        Translate {
            alloc,
            type_cache,
            symbols:RefCell::new(Symbols::new()),
            type_env
        }
    }

    pub fn translate(&self,module:&mut Module,externs_file:ExternsFile,mut compiler: ModuleCompiler<'_,'_>) -> Result<(&'alloc VMExpr<'alloc>,ArcType),TranslateError> {
        self.grab_type_info(module);
        let (export_expr,typ):(VMExpr<'alloc>,ArcType) = self.translate_exports(&module.exports, &externs_file)?;
        let mut pre_expr = self.alloc.arena.alloc(export_expr);
        for bind in module.decls.iter().rev() {
           let (let_binding,_) = self.translate_bind_item(bind)?;
           let let_expr = VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding),pre_expr);
           pre_expr = self.alloc.arena.alloc(let_expr);
        }
        let foreign = self.translate_foreign(&module, pre_expr,&mut compiler)?;
        Ok((foreign,typ.clone()))
    }

    fn translate_bind_item(&self,bind:&Bind<Ann>) -> Result<(LetBinding<'alloc>,ArcType),TranslateError> {
        match bind {
            Bind::NonRec(ann,id,expr) => {
                let id_name = self.id2str(id)?;
                let mut expr_info:TExprInfo<'alloc> = self.translate_expr(&expr,id_name)?;
                let span = source_span_to_byte_span(&ann.0);
                let mut name = TypedIdent {
                    name:self.simple_symbol(id_name),
                    typ:expr_info.typ.clone()
                };
                let named = if expr_info.closure.len() == 0 {
                    let expr = expr_info.take_expr();
                    Named::Expr(self.alloc.arena.alloc(expr))
                 } else {
                    name = TypedIdent::new(self.simple_symbol(""));
                    Named::Recursive(expr_info.closure)
                 };
                 let let_binding = LetBinding {
                    name,
                    expr:named,
                    span_start:span.start(),
                };
                Ok((let_binding,expr_info.typ.clone()))
            },
            Bind::Rec(_) => todo!()
        }
    }

    fn translate_foreign(&self,module:&Module,pre_expr:&'alloc VMExpr<'alloc>, compiler:&mut ModuleCompiler<'_,'_>) ->  Result<&'alloc VMExpr<'alloc>,TranslateError>  {
        let mut cur_expr = pre_expr;
        for ident in module.foreign.iter() {
           let ident_name = self.id2str(ident)?;
           if ident_name.starts_with("prim_") {
               continue
           }
           let sym_name = self.simple_symbol(ident_name);
           let name:TypedIdent = TypedIdent::new(sym_name);
           //import
           futures::executor::block_on(compiler.database.import(ident_name.into())).unwrap();
           
           let f_sym_name = self.symbols.borrow_mut().symbol(SymbolData { global:true, location:None, name:ident_name });
           let f_name:TypedIdent = TypedIdent::new(f_sym_name);
           let span = Span::new(BytePos(0), BytePos(0));
           let expr = Named::Expr(self.alloc.arena.alloc(VMExpr::Ident(f_name,span)));
           let let_binding = LetBinding {
             name,
             expr,
             span_start:span.start(),
           };
           let expr = VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding),cur_expr);
           cur_expr = self.alloc.arena.alloc(expr);
        }
        Ok(cur_expr)
     }

    pub fn translate_expr(&self,expr:&Expr<Ann>,bind_name:&str) -> Result<TExprInfo<'alloc>,TranslateError>  {
        match expr {
            Expr::Literal(ann,lit) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let vmexpr = self.translate_literal(lit,ann,typ.typ())?;
                Ok(TExprInfo::new(vmexpr,typ.typ()))
            },
            Expr::Var(ann,qual) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let name = qual.1.as_str().unwrap();
                let ident = TypedIdent {
                    typ:typ.typ(),
                    name:self.simple_symbol(name)
                };
                if name.chars().next().unwrap().is_uppercase() {
                    let data_expr = VMExpr::Data(ident,&[],source_span_to_byte_span(&ann.0).start() );
                    
                    Ok(TExprInfo::new(data_expr, typ.typ()))
                } else {
                    let ident_expr = VMExpr::Ident(ident,source_span_to_byte_span(&ann.0));
                    Ok(TExprInfo::new(ident_expr ,typ.typ()))
                }
                
            },
            Expr::Abs(ann,ident,expr) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let arg_name = ident.as_str().unwrap();
                let mut args:Vec<TypedIdent> = vec![TypedIdent::new2(self.symbols.borrow_mut().simple_symbol(arg_name), typ.args[0].clone())];
               let mut arg_idx = 1;
                let mut cur_expr:&Expr<Ann> = expr;
                loop { 
                    match cur_expr {
                        Expr::Abs(_,ident,expr) => {
                            let arg_name = ident.as_str().unwrap();  
                            args.push(TypedIdent::new2(self.symbols.borrow_mut().simple_symbol(arg_name), typ.args[arg_idx].clone()));
                            cur_expr = expr;
                            arg_idx = arg_idx + 1;
                        },
                        expr => {
                            cur_expr = expr;
                            break
                        } 
                    }
                }
                let typ2 = typ.typ();
                let mut expr_body = self.translate_expr(cur_expr,"")?;
                let pos = source_span_to_byte_span(&ann.0);
                let closure = Closure {
                    pos:pos.start(),
                    name:TypedIdent::new2(Symbol::from(bind_name), typ.typ()),
                    args,
                    expr:self.alloc.arena.alloc(expr_body.take_expr()),  
                };
                let tinfo = TExprInfo::new_closure(typ2,vec![closure]);
                Ok(tinfo)
            }
            Expr::App(ann,a,b) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let mut expr_b = self.translate_expr(b, "")?;
                let mut args = vec![expr_b.take_expr()];
                let mut cur_arg:&Expr<Ann> = a;
                let name_expr;
                loop { 
                    match cur_arg {
                        Expr::App(_,la,lb) => {
                            let mut arg_expr = self.translate_expr(lb, "")?;
                            args.push(arg_expr.take_expr());
                            cur_arg = la;
                        },
                        expr => {
                            let mut texpr = self.translate_expr(expr, "")?;
                            name_expr = texpr.take_expr();
                            break;
                        }
                    }
                }
                let eb = self.alloc.arena.alloc_fixed(args.drain(0..).rev());
                let ea = self.alloc.arena.alloc(name_expr);
                match ea {
                    VMExpr::Ident(id,_) if id.name.as_str().starts_with("prim_") =>  {
                       let prim_name = Translate::replace_prim_name(id.name.as_str());
                       let sym = self.symbols.borrow_mut().simple_symbol(prim_name);
                       let prim_id = TypedIdent::new2(sym, id.typ.clone());
                       let new_expr = VMExpr::Ident(prim_id, source_span_to_byte_span(&ann.0));
                       let new_ea = self.alloc.arena.alloc(new_expr);
                       Ok(TExprInfo::new(VMExpr::Call(new_ea,eb), typ.typ()))
                    },
                    _ => {
                        Ok(TExprInfo::new(VMExpr::Call(ea,eb), typ.typ()))
                    }
                }
                
            },
            Expr::Accessor(ann,name,expr) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let mut expr_info = self.translate_expr(expr, "")?;
                let ident_expr = self.alloc.arena.alloc(expr_info.take_expr());
                let field_sym = self.symbols.borrow_mut().simple_symbol(name.as_str());
                dbg!(&ident_expr);
               
                let pattern = Pattern::Record {
                    typ:expr_info.typ,
                    fields:vec![(TypedIdent::new2(field_sym.clone(),typ.typ()),None)]
                };
                let out_expr = VMExpr::Ident(TypedIdent::new2(field_sym,typ.typ()),source_span_to_byte_span(&ann.0));
               
                let alt = Alternative { pattern, expr: self.alloc.arena.alloc(out_expr) };
                let alt_list = self.alloc.alternative_arena.alloc_fixed(std::iter::once(alt) );
                
                let match_expr = VMExpr::Match(ident_expr,alt_list);
                dbg!(&match_expr);
                Ok(TExprInfo::new(match_expr, typ.typ()))
            },
            Expr::Case(ann,exprs,cases) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let mut pred = self.translate_expr(&exprs[0],"")?;
                let pred_expr = self.alloc.arena.alloc(pred.take_expr());
                let mut alt_arr:Vec<Alternative> = vec![];
                for case_item in cases {
                   let binder = &case_item.binders[0];
                   let pattern:Pattern;
                   match binder {
                    Binder::LiteralBinder(_,lit) => {
                       pattern = self.translate_literal_binder(lit)?;
                    },
                    Binder::NullBinder(_) => {
                        pattern = Pattern::Ident(TypedIdent::new(self.simple_symbol("")));
                    },
                     b => { dbg!(b);todo!()} 
                   }
                   let res_expr = self.translate_expr(case_item.result.as_ref().unwrap_err(), "")?.take_expr();
                   let res_expr_ref = self.alloc.arena.alloc(res_expr);
                   let alt = Alternative {
                    pattern,
                    expr:res_expr_ref
                   };
                   alt_arr.push(alt);
                }
                let alt_arr_ref = self.alloc.alternative_arena.alloc_fixed(alt_arr);
                let match_expr = VMExpr::Match(pred_expr,alt_arr_ref);
                Ok(TExprInfo::new(match_expr, typ.typ()))
            },
            expr => { dbg!(expr); todo!() } 
        }
    }

    fn translate_literal_binder(&self,lit:&Literal<Box<Binder<Ann>>>) -> Result<Pattern,TranslateError> {
        match lit {
            Literal::NumericLiteral(enumber) => {
                match enumber {
                    Ok(inum) => Ok(Pattern::Literal(VMLiteral::Int(*inum as i64))),
                    Err(fnum) => {
                        let val =  ordered_float::NotNan::new(*fnum).map_err(|_| TranslateError::TranslateNotNanFloat)?;
                        Ok(Pattern::Literal(VMLiteral::Float(val)))
                    },
                }
            },
            Literal::CharLiteral(chr) => Ok(Pattern::Literal(VMLiteral::Char(*chr))),
            Literal::StringLiteral(str) => Ok(Pattern::Literal(VMLiteral::String(Box::from(&str[..])))),
            Literal::BooleanLiteral(b) => {
                let bool_var = self.type_env.bool_constructor(*b);
                Ok(Pattern::Constructor(bool_var,vec![]))
            }
            _ => todo!()
        }
    }

    fn translate_literal(&self,lit:&Literal<Box<Expr<Ann>>>,ann:&Ann,typ:ArcType) -> Result<VMExpr<'alloc>,TranslateError> {
        let byte_pos = source_span_to_byte_span(&ann.0);
        match lit {
            Literal::NumericLiteral(enumber) => {
                match enumber {
                    Ok(inum) => Ok(VMExpr::Const(VMLiteral::Int(*inum as i64),byte_pos)) ,
                    Err(fnum) => {
                        let val =  ordered_float::NotNan::new(*fnum).map_err(|_| TranslateError::TranslateNotNanFloat)?;
                        Ok(VMExpr::Const(VMLiteral::Float(val),byte_pos))
                    },
                }
            }
            Literal::StringLiteral(str) => Ok(VMExpr::Const(VMLiteral::String(Box::from(&str[..])),byte_pos)),
            Literal::CharLiteral(chr) => Ok(VMExpr::Const(VMLiteral::Char(*chr),byte_pos)),
            Literal::ArrayLiteral(arr) => {
                let exprs = self.alloc.arena.alloc_fixed(arr.iter().map(|v| self.translate_expr(v,"").unwrap().take_expr()));
                Ok(VMExpr::Data(TypedIdent {
                    typ:typ.clone(),
                    name:self.simple_symbol("")
                },exprs,byte_pos.start()))
            },
            Literal::ObjectLiteral(record_list) => {
                let vm_exprs = record_list.iter().map(|(_,v)| {
                    let mut expr_info = self.translate_expr(v,"").unwrap();
                    expr_info.take_expr()
                });
                let alloc_expr = self.alloc.arena.alloc_fixed(vm_exprs);
                let data = VMExpr::Data(TypedIdent {
                    typ: typ.clone(),
                    name:self.simple_symbol("")
                },alloc_expr,byte_pos.start());
                Ok(data)
            },
            Literal::BooleanLiteral(b) => {
                let type_ident = self.type_env.bool_constructor(*b);
                let data = VMExpr::Data(type_ident,&[],byte_pos.start());
                Ok(data)
            }
        }
    }

    fn translate_exports(&self,exports:&Vec<Ident>,externs_file:&ExternsFile) ->  Result<(VMExpr<'alloc>,ArcType),TranslateError> {
        let type_dic = externs_file.decl_type_dic();
        let mut field_types:Vec<Field<Symbol>> = vec![];
        let mut fields:Vec<VMExpr<'alloc>> = vec![];
        for id in exports {
            let id_name = self.id2str(id)?;
            let decl_type = *type_dic.get(id_name).ok_or(TranslateError::NotFindType)?;
            let typ = self.translate_type(decl_type).map_err(|_| TranslateError::PartialType)?;
            let type_ident:TypedIdent = TypedIdent::new2(self.simple_symbol(id_name),typ.typ());
            let ident = VMExpr::Ident(type_ident,Span::new(ByteIndex(0), ByteIndex(0)));
            fields.push(ident);

            let sym = self.simple_symbol(id_name);
            let field_type = Field::new(sym, typ.typ());
            field_types.push(field_type);
        }
        let typ = self.type_cache.record(vec![], field_types);
        let alloc_expr = self.alloc.arena.alloc_fixed(fields);
        let rec = VMExpr::Data(TypedIdent {
            typ:typ.clone(),
            name:self.simple_symbol(""),
        },alloc_expr,ByteIndex(0));
        Ok((rec,typ.clone()))
    }

    pub(crate) fn translate_type<T>(&self,typ:&AstType<T>) -> Result<TTypeInfo,TransferType>  {
        match &typ {
            AstType::TypeConstructor(_,qual_proper) => {
                let type_name = types::proper_name_as_str(&qual_proper.1);
                match &qual_proper.0 {
                    Some(qual_str) if qual_str == "Prim" => {
                        match type_name {
                          "Int" => Ok(TTypeInfo::new(self.type_cache.int())),
                          "Number" => Ok(TTypeInfo::new(self.type_cache.float())),
                          "String" => Ok(TTypeInfo::new(self.type_cache.string())),
                          "Char"   => Ok(TTypeInfo::new(self.type_cache.char())),
                          "Array"  => Ok(TTypeInfo::new(self.type_cache.array_builtin())),
                          "Record" => Err(TransferType::PartialRecord(type_name.to_string())),
                          "Function" => Err(TransferType::Function),
                          "Boolean" => {
                             let bool_type = self.type_env.get_bool_type().gluon_type.clone();
                             Ok(TTypeInfo::new(bool_type))
                          }
                          _ => Ok(TTypeInfo::new(Type::ident(KindedIdent::new(Symbol::from(type_name)))))
                        }
                    },
                     _ => {
                        let qual_type_name = qual_proper.join_name();
                        let var_len = self.type_env.type_dic.borrow().get(&qual_type_name).unwrap().type_vars.len();
                        let gluon_type = self.type_env.type_dic.borrow().get(&qual_type_name).unwrap().gluon_type.clone();
                        if var_len == 0 {
                            let alias =  Type::alias(self.simple_symbol(type_name), vec![], gluon_type);
                            Ok(TTypeInfo::new(alias))
                        } else {
                            Ok(TTypeInfo::new_env_type(qual_type_name))
                        }
                     } 
                }
            },
            AstType::TypeApp(_,_,tb) => {
                let app_list = self.flat_app_type(typ);
                match &app_list[0] {
                    Err(ref trans_type) => {
                        match trans_type {
                            TransferType::PartialRecord(_) => {
                                 return Ok(TTypeInfo::new(app_list[1].as_ref().unwrap().typ()));
                            },
                            TransferType::Function => {
                                let arc_type = app_list[1].as_ref().unwrap().typ();
                                Err(TransferType::FunctionCtor(arc_type))
                            },
                            TransferType::FunctionCtor(arc_type) => {
                                dbg!(&app_list);
                                let mut flat_types:Vec<ArcType> = app_list.iter().skip(1).map(|v| {
                                    match v {
                                        Err(TransferType::FunctionCtor(at)) => at.clone(),
                                        Ok(t) if t.typ.is_some() => t.typ(),
                                        _ => panic!("translate fuction type error")
                                    }
                                }).collect();
                                flat_types.insert(0, arc_type.clone());
                                let tail = flat_types.pop().unwrap();
                                let ft = self.type_cache.function( flat_types.iter().map(|v|(*v).clone()),tail.clone());
                                let clone_flat_types = flat_types.iter().map(|v|(*v).clone()).collect();
                                Ok(TTypeInfo::new_func(ft, clone_flat_types) )
                            }
                        }
                    },
                    Ok(ta)  => {
                        if let Some(qual_type_name) = ta.cache_type.as_ref() {
                            let gluon_type = self.type_env.type_dic.borrow().get(qual_type_name).unwrap().gluon_type.clone();
                            let type_name = self.type_env.type_dic.borrow().get(qual_type_name).unwrap().type_name.clone();
                            let type_vars = self.type_env.type_dic.borrow().get(qual_type_name).unwrap().type_vars.clone();

                            let alias =  Type::alias(self.simple_symbol(type_name.as_str()), type_vars, gluon_type);
                            dbg!(&alias);
                            Ok(TTypeInfo::new(alias))
                        } else {
                            let typb = self.translate_type(tb)?;
                            Ok(TTypeInfo::new(Type::app(ta.typ(), collect!(typb.typ()))))
                        }
                    }
                }
            }
            AstType::RCons(_,_,_,_) => {
                let mut cur_type:&AstType<T> = typ;
                let mut fields = vec![];
                loop { 
                    match cur_type {
                        AstType::RCons(_,label,head_,tail_) => {
                            let field_type = self.translate_type(head_).unwrap().typ();
                            let name_sym = self.simple_symbol(label.as_str());
                            let field = Field::new(name_sym, field_type);
                            fields.push(field);
                            cur_type = tail_;
                        }
                        _ => break
                    }
                }
                Ok(TTypeInfo::new(self.type_cache.record(vec![], fields)))
            },
            AstType::TypeVar(_,var) => {
               let generic =  Generic::new(self.simple_symbol(var.as_str()), Kind::typ());
               Ok( TTypeInfo::new(Type::generic(generic)))
            },
            _ =>  Ok(TTypeInfo::new(self.type_cache.hole()))
        }
    }

   fn flat_app_type<T>(&self,typ:&AstType<T>) -> Vec<Result<TTypeInfo,TransferType>> {
       let mut cur_type = typ;
       let mut list:Vec<Result<TTypeInfo,TransferType>> = vec![];
       loop { 
           match cur_type {
               AstType::TypeApp(_,head,tail) => {
                   list.push(self.translate_type(head));
                   cur_type = tail;
                },
                _ => {  
                    list.push(self.translate_type(cur_type));
                    break;
                }
            }
        }
        list
   }

    

    fn id2str<'a>(&self,id:&'a Ident) -> Result<&'a str,TranslateError> {
        match id.as_str() {
            Some(s) => Ok(s),
            None => Err(TranslateError::IdentToString)
        }
    }

    fn replace_prim_name(func_name:&str) -> String {
        match func_name {
            "prim_int_add_" => "#Int+".into(),
            _ => func_name.into()
        }
    }

    pub(crate) fn simple_symbol(&self,name:&str) -> Symbol {
        self.symbols.borrow_mut().simple_symbol(name)
    }
}


#[derive(Debug,Clone)]
pub(crate) enum TransferType {
    PartialRecord(String),
    Function,
    FunctionCtor(ArcType)
}
#[derive(Debug)]
pub(crate) struct TTypeInfo {
    pub typ:Option<ArcType>,
    pub args:Vec<ArcType>,
    pub cache_type:Option<String>
}

impl TTypeInfo {
    pub fn new(t:ArcType) -> Self {
        TTypeInfo { 
            typ:Some(t),args:vec![],
            cache_type:None
        }
    }

    pub fn typ(&self) -> ArcType {
        self.typ.as_ref().map(|a| a.clone() ).unwrap()
    }

    pub fn new_func(t:ArcType,args:Vec<ArcType>) -> Self {
        TTypeInfo {
            typ:Some(t),
            args,
            cache_type:None
        }
    }
    pub fn new_env_type(str:String) -> Self {
        TTypeInfo { 
            typ:None,args:vec![],
            cache_type:Some(str)
        }
    } 
}

pub struct TExprInfo<'a> {
    pub typ:ArcType,
    pub expr:Option<VMExpr<'a>>,
    pub closure:Vec<Closure<'a>>
}

impl<'a> TExprInfo<'a> {
    pub fn new(expr:VMExpr<'a>,typ:ArcType) -> Self {
        TExprInfo { expr:Some(expr),typ,closure:vec![] }
    }

    pub fn new_closure(typ:ArcType,closure:Vec<Closure<'a>>) -> Self {
        TExprInfo { expr:None, typ,closure}
    }

    pub fn expr(&self) -> &VMExpr<'a> {
        self.expr.as_ref().unwrap()
    }
    pub fn take_expr(&mut self) -> VMExpr<'a> {
        self.expr.take().unwrap()
    }
}