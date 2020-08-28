use ast::types::{self,Bind,Ann,Expr,Literal,Module,Type};
use gluon::vm::core::{Expr as VMExpr,Literal as VMLiteral,self as vmcore,Named,Allocator,Alternative,Pattern};
use gluon::base::{ast as gast,pos::{BytePos,Span,ByteIndex},types as gt,symbol::{Symbol,SymbolData,Symbols}};
use std::sync::Arc;
use std::cell::RefCell;
use gluon::ModuleCompiler;
use crate::utils::*;
use gluon::query::{AsyncCompilation};
pub struct Translate<'a> {
    type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>,
    pub alloc:Arc<Allocator<'a>>,
    dummy_symbol:gast::TypedIdent,
    symbols: RefCell<Symbols>,
    compiler:RefCell<ModuleCompiler<'a,'a>>
}

#[derive(Debug,Clone)]
enum TransferType {
    PartialTypeConstructor(String),
    Function,
    FunctionCtor(gt::ArcType)
}

struct TTypeInfo {
    pub typ:gt::ArcType,
    pub args:Vec<gt::ArcType>
}

impl TTypeInfo {
    pub fn new(t:gt::ArcType) -> Self {
        TTypeInfo { 
            typ:t,args:vec![] 
        }
    }

    pub fn new_func(t:gt::ArcType,args:Vec<gt::ArcType>) -> Self {
        TTypeInfo {
            typ:t,
            args
        }
    }
}

pub struct TExprInfo<'a> {
    pub typ:gt::ArcType,
    pub expr:Option<VMExpr<'a>>,
    pub closure:Vec<vmcore::Closure<'a>>
}

impl<'a> TExprInfo<'a> {
    pub fn new(expr:VMExpr<'a>,typ:gt::ArcType) -> Self {
        TExprInfo { expr:Some(expr),typ,closure:vec![] }
    }
    pub fn new_closure(typ:gt::ArcType,closure:Vec<vmcore::Closure<'a>>) -> Self {
        TExprInfo { expr:None, typ,closure}
    }

    pub fn expr(&self) -> &VMExpr<'a> {
        self.expr.as_ref().unwrap()
    }
    pub fn take_expr(&mut self) -> VMExpr<'a> {
        self.expr.take().unwrap()
    }
}

impl<'a> Translate<'a> {
    pub fn new(type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>,compiler:ModuleCompiler<'a,'a>) -> Self {
        Translate {
            type_cache,
            alloc:Arc::new(Allocator::new()),
            dummy_symbol: gast::TypedIdent {
                typ:type_cache.hole(),
                name:Symbol::from(""),
            },
            symbols:RefCell::new(Symbols::new()),
            compiler:RefCell::new(compiler)
        }
    }

    pub fn translate_module(&'a self,module:&Module) -> Result<&'a VMExpr<'a>,TranslateError> {
        let export_module = VMExpr::Const(VMLiteral::Int(114514),Span::new(ByteIndex(0), ByteIndex(0)));
        
        
        let mut pre_expr = self.alloc.arena.alloc(export_module);
        for bind in module.decls.iter().rev() {
            let expr = self.translate_bind(bind, pre_expr)?;
            pre_expr = self.alloc.arena.alloc(expr);
        }
        let foreign_expr = self.translate_foreign(module,pre_expr);
        Ok(foreign_expr)
    }

    fn translate_foreign(&'a self,module:&Module,pre_expr:&'a VMExpr<'a>) -> &'a VMExpr<'a> {
       let mut cur_expr = pre_expr;
       for ident in module.foreign.iter() {
          let ident_name = ident.as_str().unwrap();
          if ident_name.starts_with("prim_") {
              continue
          }
          let sym_name = self.symbols.borrow_mut().simple_symbol(ident_name);
          let name:gast::TypedIdent = gast::TypedIdent::new(sym_name);
          //import
          futures::executor::block_on(self.compiler.borrow_mut().database.import(ident_name.into())).unwrap();
          
          
          let f_sym_name = self.symbols.borrow_mut().symbol(SymbolData {
              global:true,
              location:None,
              name:ident_name
          });
          let f_name:gast::TypedIdent = gast::TypedIdent::new(f_sym_name);
          let span = Span::new(BytePos(0), BytePos(0));
          let expr = Named::Expr(self.alloc.arena.alloc(VMExpr::Ident(f_name,span)));
          let let_binding = vmcore::LetBinding {
            name,
            expr,
            span_start:span.start(),
          };
          let expr = VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding),cur_expr);
          cur_expr = self.alloc.arena.alloc(expr);
       }
       cur_expr
    }

    pub fn translate_bind(&'a self,bind:&Bind<Ann>,pre_expr:&'a VMExpr<'a>) -> Result<VMExpr<'a>,TranslateError>  {
        match bind {
            Bind::NonRec(ann,id,expr) => {
                let id_name = match id {
                    types::Ident::Ident(str) => str.to_owned(),
                    _ => "".to_string()
                };
                let mut expr_info:TExprInfo<'a> = self.translate_expr(&expr,id_name.as_str())?;
                let span = source_span_to_byte_span(&ann.0);
               
                let mut name = gast::TypedIdent {
                    name:self.symbols.borrow_mut().simple_symbol(id_name),
                    typ:expr_info.typ.clone()
                };
                 let named = if expr_info.closure.len() == 0 {
                    let expr = expr_info.take_expr();
                    Named::Expr(self.alloc.arena.alloc(expr))
                 } else {
                    name = gast::TypedIdent::new(self.symbols.borrow_mut().simple_symbol("") );
                   Named::Recursive(expr_info.closure)
                 };
                 let let_binding = vmcore::LetBinding {
                    name,
                    expr:named,
                    span_start:span.start(),
                };
                Ok(VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding),pre_expr))
            }
            Bind::Rec(_) => todo!()
        }
    }

    pub fn translate_expr(&'a self,expr:&Expr<Ann>,bind_name:&str) -> Result<TExprInfo,TranslateError>  {
        match expr {
            Expr::Literal(ann,lit) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let vmexpr = self.translate_literal(lit,ann,&typ.typ)?;
                Ok(TExprInfo::new(vmexpr,typ.typ))
            },
            Expr::Var(ann,qual) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let name = qual.1.as_str().unwrap();
                let ident = VMExpr::Ident(gast::TypedIdent {
                    typ:typ.typ.clone(),
                    name:self.symbols.borrow_mut().simple_symbol(name)
                },source_span_to_byte_span(&ann.0));
                Ok(TExprInfo::new(ident ,typ.typ))
            },
            Expr::Abs(ann,ident,expr) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let arg_name = ident.as_str().unwrap();
                let mut args:Vec<gast::TypedIdent> = vec![gast::TypedIdent::new2(self.symbols.borrow_mut().simple_symbol(arg_name), typ.args[0].clone())];
               let mut arg_idx = 1;
                let mut cur_expr:&Expr<Ann> = expr;
                loop { 
                    match cur_expr {
                        Expr::Abs(_,ident,expr) => {
                            let arg_name = ident.as_str().unwrap();  
                            args.push(gast::TypedIdent::new2(self.symbols.borrow_mut().simple_symbol(arg_name), typ.args[arg_idx].clone()));
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
                let closure = vmcore::Closure {
                    pos:pos.start(),
                    name:gast::TypedIdent::new2(Symbol::from(bind_name), typ.typ),
                    args,
                    expr:self.alloc.arena.alloc(expr_body.take_expr()),  
                };
                let tinfo = TExprInfo::new_closure(typ2,vec![closure]);
                Ok(tinfo)
            }
            Expr::App(ann,a,b) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let mut expr_b = self.translate_expr(b, "")?;
                let mut args:Vec<VMExpr<'a>> = vec![expr_b.take_expr()];
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
                let eb:&'a [VMExpr<'a>] = self.alloc.arena.alloc_fixed(args.drain(0..).rev());
                let ea = self.alloc.arena.alloc(name_expr);
                match ea {
                    VMExpr::Ident(id,_) if id.name.as_str().starts_with("prim_") =>  {
                       let prim_name = Translate::replace_prim_name(id.name.as_str());
                       let sym = self.symbols.borrow_mut().simple_symbol(prim_name);
                       let prim_id = gast::TypedIdent::new2(sym, id.typ.clone());
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
                    fields:vec![(gast::TypedIdent::new2(field_sym.clone(),typ.typ.clone()),None)]
                };
                let out_expr = VMExpr::Ident(gast::TypedIdent::new2(field_sym,typ.typ.clone()),source_span_to_byte_span(&ann.0));
               
                let alt = Alternative { pattern, expr: self.alloc.arena.alloc(out_expr) };
                let alt_list = self.alloc.alternative_arena.alloc_fixed(std::iter::once(alt) );
                
                let match_expr = VMExpr::Match(ident_expr,alt_list);
                dbg!(&match_expr);
                Ok(TExprInfo::new(match_expr, typ.typ))
            },
            expr => { dbg!(expr); todo!() } 
        }
    }

    fn translate_literal(&'a self,lit:&Literal<Box<Expr<Ann>>>,ann:&Ann,typ:&gt::ArcType) -> Result<VMExpr<'a>,TranslateError> {
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
                Ok(VMExpr::Data(gast::TypedIdent {
                    typ:typ.clone(),
                    name:Symbol::from("")
                },exprs,byte_pos.start()))
            },
            Literal::ObjectLiteral(record_list) => {
                let vm_exprs = record_list.iter().map(|(_,v)| {
                    let mut expr_info = self.translate_expr(v,"").unwrap();
                    expr_info.take_expr()
                });
                let alloc_expr = self.alloc.arena.alloc_fixed(vm_exprs);
                let data = VMExpr::Data(gast::TypedIdent {
                    typ: typ.clone(),
                    name:self.dummy_symbol.name.clone()
                },alloc_expr,byte_pos.start());
                Ok(data)
            },
            Literal::BooleanLiteral(_) => unimplemented!()
        }
    }

    fn translate_type<'b>(&'a self,typ:&'b Type<()>) -> Result<TTypeInfo,TransferType>  {
        match &typ {
            Type::TypeConstructor(_,proper) => {
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
                         _ => Ok(TTypeInfo::new(gt::Type::ident(gt::KindedIdent::new(Symbol::from(type_name)))))
                       }
                   },
                   _ => Ok(TTypeInfo::new(gt::Type::ident(gt::KindedIdent::new(Symbol::from(type_name)))))
               }
            },
            Type::TypeApp(_,ta,tb) => {
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
                        Ok(TTypeInfo::new(gt::Type::app(ta.typ, collect!(typb.typ))))
                    } ,
                }             
            },
            Type::RCons(_,_,_,_) => {
                let mut cur_type:&Type<()> = typ;
                let mut fields = vec![];
                loop { 
                    match cur_type {
                        Type::RCons(_,label,head_,tail_) => {
                            let field_type = self.translate_type(head_).unwrap().typ;
                            let name_sym = self.symbols.borrow_mut().simple_symbol(label.as_str());
                            let field = gt::Field::new(name_sym, field_type);
                            fields.push(field);
                            cur_type = tail_;
                        }
                        _ => break
                    }
                }
                Ok(TTypeInfo::new(self.type_cache.record(vec![], fields)))
            }
            _ => Ok(TTypeInfo::new(self.type_cache.hole()))
        }
    }

    fn collect_app_type<'b>(&'a self,typ:&Type<()>) -> Vec<Result<gt::ArcType,TransferType>>  {
        let mut cur_type = typ;
        let mut ret_vec = vec![];
        loop { 
            match cur_type {
                Type::TypeApp(_,head,tail) => {
                    ret_vec.push(self.translate_type(head).map(|v|v.typ) );
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

    fn replace_prim_name(func_name:&str) -> String {
        match func_name {
            "prim_int_add_" => "#Int+".into(),
            _ => func_name.into()
        }
    }

}

#[derive(Debug)]
pub enum  TranslateError {
    TranslateNotNanFloat,
    TypeError
}


fn source_span_to_byte_span(source_span:&types::SourceSpan) -> Span<BytePos>  {
    let start = source_pos_to_byte_pos(&source_span.start);
    let end =source_pos_to_byte_pos(&source_span.end);
    Span::new(start, end)
}

fn source_pos_to_byte_pos(source_pos:&types::SourcePos) -> BytePos  {
    let uline = source_pos.line as u32;
    let ucol = source_pos.col as u32;
    let hight16 = (uline << 16) & 0xffff0000;
    let low16 = ucol & & 0x0000ffff;
    let value = hight16 | low16;
    BytePos(value)
}