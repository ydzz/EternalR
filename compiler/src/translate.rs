use gluon::vm::core::{Allocator,Pattern,Alternative,Named};
use gluon::base::types::{ArcType,TypeCache,Field,KindedIdent,Type};
use gluon::base::symbol::{Symbol,Symbols,SymbolData};
use gluon::base::ast::{TypedIdent};
use gluon::base::pos::{BytePos};
use gluon::vm::core::{Expr as VMExpr,Closure,Literal as VMLiteral,LetBinding};
use std::cell::RefCell;
use gluon::base::pos::{Span,ByteIndex};
use ast::{ExternsFile};
use ast::types::{self,Module,Type as AstType,Literal,Expr,Bind,Ident,Ann};
use crate::errors::TranslateError;
use gluon::ModuleCompiler;
use crate::utils::*;
use gluon::query::{AsyncCompilation};

pub struct Translate<'vm,'alloc>{
   pub alloc:&'alloc Allocator<'alloc>,
   pub type_cache:&'vm TypeCache<Symbol,ArcType>,
   pub symbols: RefCell<Symbols>,
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn new(alloc:&'alloc Allocator<'alloc>,type_cache:&'vm TypeCache<Symbol,ArcType>) -> Self {
        Translate {
            alloc,
            type_cache,
            symbols:RefCell::new(Symbols::new())
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
                let vmexpr = self.translate_literal(lit,ann,typ.typ.clone())?;
                Ok(TExprInfo::new(vmexpr,typ.typ))
            },
            Expr::Var(ann,qual) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let name = qual.1.as_str().unwrap();
                let ident = VMExpr::Ident(TypedIdent {
                    typ:typ.typ.clone(),
                    name:self.simple_symbol(name)
                },source_span_to_byte_span(&ann.0));
                Ok(TExprInfo::new(ident ,typ.typ))
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
                let typ2 = typ.typ.clone();
                let mut expr_body = self.translate_expr(cur_expr,"")?;
                let pos = source_span_to_byte_span(&ann.0);
                let closure = Closure {
                    pos:pos.start(),
                    name:TypedIdent::new2(Symbol::from(bind_name), typ.typ),
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
                       Ok(TExprInfo::new(VMExpr::Call(new_ea,eb), typ.typ))
                    },
                    _ => {
                        Ok(TExprInfo::new(VMExpr::Call(ea,eb), typ.typ))
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
                    fields:vec![(TypedIdent::new2(field_sym.clone(),typ.typ.clone()),None)]
                };
                let out_expr = VMExpr::Ident(TypedIdent::new2(field_sym,typ.typ.clone()),source_span_to_byte_span(&ann.0));
               
                let alt = Alternative { pattern, expr: self.alloc.arena.alloc(out_expr) };
                let alt_list = self.alloc.alternative_arena.alloc_fixed(std::iter::once(alt) );
                
                let match_expr = VMExpr::Match(ident_expr,alt_list);
                dbg!(&match_expr);
                Ok(TExprInfo::new(match_expr, typ.typ))
            },
            expr => { dbg!(expr); todo!() } 
        }
    }

    fn translate_literal(&self,lit:&Literal<Box<Expr<Ann>>>,ann:&Ann,typ:ArcType) -> Result<VMExpr<'alloc>,TranslateError> {
        let byte_pos = source_span_to_byte_span(&ann.0);
        match lit {
            Literal::NumericLiteral(rnum) => {
                match rnum {
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
            Literal::BooleanLiteral(_) => unimplemented!()
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
            let type_ident:TypedIdent = TypedIdent::new2(self.simple_symbol(id_name),typ.typ.clone());
            let ident = VMExpr::Ident(type_ident,Span::new(ByteIndex(0), ByteIndex(0)));
            fields.push(ident);

            let sym = self.simple_symbol(id_name);
            let field_type = Field::new(sym, typ.typ.clone());
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

    fn translate_type<T>(&self,typ:&AstType<T>) -> Result<TTypeInfo,TransferType>  {
        match &typ {
            AstType::TypeConstructor(_,proper) => {
                let type_name = types::proper_name_as_str(&proper.1);
                match &proper.0 {
                    Some(qual_str) if qual_str == "Prim" => {
                        match type_name {
                          "Int" => Ok(TTypeInfo::new(self.type_cache.int())),
                          "Number" => Ok(TTypeInfo::new(self.type_cache.float())),
                          "String" => Ok(TTypeInfo::new(self.type_cache.string())),
                          "Char"   => Ok(TTypeInfo::new(self.type_cache.char())),
                          "Array"  => Ok(TTypeInfo::new(self.type_cache.array_builtin())),
                          "Record" => Err(TransferType::PartialTypeConstructor(type_name.to_string())),
                          "Function" => Err(TransferType::Function),
                          _ => Ok(TTypeInfo::new(Type::ident(KindedIdent::new(Symbol::from(type_name)))))
                        }
                    },
                     _ => Ok(TTypeInfo::new(Type::ident(KindedIdent::new(Symbol::from(type_name)))))
                }
            },
            AstType::TypeApp(_,ta,tb) => {
                let head = self.translate_type(ta);
                let tail_vecs = self.collect_app_type(tb);
                match head {
                    Err(ref trans_type) => {
                        match trans_type {
                            TransferType::PartialTypeConstructor(_) => {
                                //if *str == "Record" {
                                   return Ok(TTypeInfo::new(tail_vecs[0].as_ref().unwrap().clone()));
                                //}
                            },
                            TransferType::Function => {
                                let arc_type = tail_vecs[0].as_ref().unwrap().clone();
                                Err(TransferType::FunctionCtor(arc_type))
                            },
                            TransferType::FunctionCtor(arc_type) => {
                                let mut flat_types:Vec<_> = tail_vecs.iter().map(|v| {
                                    match v {
                                        Err(TransferType::FunctionCtor(at)) => at,
                                        Ok(t) => t,
                                        _ => panic!("translate fuction type error")
                                    }
                                }).collect();
                                flat_types.insert(0, arc_type);
                                let tail = flat_types.pop().unwrap();
                                let ft = self.type_cache.function( flat_types.iter().map(|v|(*v).clone()),tail.clone());
                                let clone_flat_types = flat_types.iter().map(|v|(*v).clone()).collect();
                                Ok(TTypeInfo::new_func(ft, clone_flat_types) )
                            }
                        }
                    },
                    Ok(ta)  => {
                        let typb = self.translate_type(tb)?;
                        Ok(TTypeInfo::new(Type::app(ta.typ, collect!(typb.typ))))
                    }
                }
            }
            AstType::RCons(_,_,_,_) => {
                let mut cur_type:&AstType<T> = typ;
                let mut fields = vec![];
                loop { 
                    match cur_type {
                        AstType::RCons(_,label,head_,tail_) => {
                            let field_type = self.translate_type(head_).unwrap().typ;
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
            _ =>  Ok(TTypeInfo::new(self.type_cache.hole()))
        }
    }

    fn collect_app_type<T>(&self,typ:&AstType<T>) -> Vec<Result<ArcType,TransferType>>  {
        let mut cur_type = typ;
        let mut ret_vec = vec![];
        loop { 
            match cur_type {
                AstType::TypeApp(_,head,tail) => {
                    ret_vec.push(self.translate_type(head).map(|v|v.typ));
                    cur_type = tail;
                },
                _ => {  
                    ret_vec.push(self.translate_type(cur_type).map(|v|v.typ));
                    break;
                }
            }
        }
        ret_vec
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

    fn simple_symbol(&self,name:&str) -> Symbol {
        self.symbols.borrow_mut().simple_symbol(name)
    }
}


#[derive(Debug,Clone)]
enum TransferType {
    PartialTypeConstructor(String),
    Function,
    FunctionCtor(ArcType)
}

struct TTypeInfo {
    pub typ:ArcType,
    pub args:Vec<ArcType>
}

impl TTypeInfo {
    pub fn new(t:ArcType) -> Self {
        TTypeInfo { 
            typ:t,args:vec![] 
        }
    }

    pub fn new_func(t:ArcType,args:Vec<ArcType>) -> Self {
        TTypeInfo {
            typ:t,
            args
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