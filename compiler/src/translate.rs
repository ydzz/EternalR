use ast::types::{self,Bind,Ann,Expr,Literal,Module,Type};
use gluon::vm::core::{Expr as VMExpr,Literal as VMLiteral,self as vmcore,Named,Allocator};
use gluon::base::{ast as gast,pos::{BytePos,Span,ByteIndex},types as gt,symbol::Symbol};
use std::sync::Arc;
use crate::utils::*;
pub struct Translate<'a> {
    type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>,
    alloc:Arc<Allocator<'a>>,
    dummy_symbol:gast::TypedIdent
}

#[derive(Debug,Clone)]
enum TransferType {
    PartialTypeConstructor(String),
    Function,
    FunctionCtor(gt::ArcType)
}

impl<'a> Translate<'a> {
    pub fn new(type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>) -> Self {
        Translate {
            type_cache,
            alloc:Arc::new(Allocator::new()),
            dummy_symbol: gast::TypedIdent {
                typ:type_cache.hole(),
                name:Symbol::from(""),
            }
        }
    }

    pub fn translate_module(&'a self,module:&Module) -> Result<&'a VMExpr<'a>,TranslateError> {
        let export_module = VMExpr::Const(VMLiteral::Int(114514),Span::new(ByteIndex(0), ByteIndex(0)));
        
        
        let mut pre_expr = self.alloc.arena.alloc(export_module);
        for bind in module.decls.iter().rev() {
            let expr = self.translate_bind(bind, pre_expr)?;
            pre_expr = self.alloc.arena.alloc(expr);
        }
        Ok(pre_expr)
    }

    pub fn translate_bind(&'a self,bind:&Bind<Ann>,pre_expr:&'a VMExpr<'a>) -> Result<VMExpr<'a>,TranslateError>  {
        match bind {
            Bind::NonRec(ann,id,expr) => {
                let (expr,ty) = self.translate_expr(&expr)?;
                let vm_expr:&'a VMExpr = self.alloc.arena.alloc(expr);
                let span = source_span_to_byte_span(&ann.0);
                let id_name = match id {
                    types::Ident::Ident(str) => str.to_owned(),
                    _ => "".to_string()
                };
                let name = gast::TypedIdent {
                    name:Symbol::from(id_name),
                    typ:ty
                };
                let named = Named::Expr(&vm_expr);
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

    pub fn translate_expr(&'a self,expr:&Expr<Ann>) -> Result<(VMExpr<'a>,gt::ArcType),TranslateError>  {
        match expr {
            Expr::Literal(ann,lit) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                let expr = self.translate_literal(lit,ann,&typ)?;
                Ok((expr,typ))
            },
            Expr::Abs(ann,_ident,_expr) => {
                let typ = self.translate_type(ann.2.as_ref().unwrap()).map_err(|_| TranslateError::TypeError)?;
                dbg!(typ);
                todo!()
            }
            _ => todo!()
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
                let exprs = self.alloc.arena.alloc_fixed(arr.iter().map(|v| self.translate_expr(v).unwrap().0));
                Ok(VMExpr::Data(gast::TypedIdent {
                    typ:typ.clone(),
                    name:Symbol::from("")
                },exprs,byte_pos.start()))
            },
            Literal::ObjectLiteral(record_list) => {
                let vm_exprs = record_list.iter().map(|(_,v)| {
                    let (vm_expr,_) = self.translate_expr(v).unwrap();
                    vm_expr
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

    fn translate_type<'b>(&'a self,typ:&'b Type<()>) -> Result<gt::ArcType,TransferType>  {
        match &typ {
            Type::TypeConstructor(_,proper) => {
               let type_name = types::proper_name_as_str(&proper.1);
               match &proper.0 {
                   Some(qual_str) if qual_str == "Prim" => {
                       match type_name {
                         "Int" => Ok(self.type_cache.int()),
                         "Number" => Ok(self.type_cache.float()),
                         "String" => Ok(self.type_cache.string()),
                         "Char"   => Ok(self.type_cache.char()),
                         "Array"  => Ok(self.type_cache.array_builtin()),
                         "Record" => Err(TransferType::PartialTypeConstructor(type_name.to_string())),
                         "Function" => Err(TransferType::Function),
                         _ => Ok(gt::Type::ident(gt::KindedIdent::new(Symbol::from(type_name))))
                       }
                   },
                   _ => Ok(gt::Type::ident(gt::KindedIdent::new(Symbol::from(type_name))))
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
                                   return Ok(tail_vecs[0].as_ref().unwrap().clone());
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
                                Ok(self.type_cache.function( flat_types.iter().map(|v|(*v).clone()),tail.clone()))
                            }
                        }
                    },
                    Ok(ta)  =>  Ok(gt::Type::app(ta, collect!(self.translate_type(tb)?))),
                }             
            },
            Type::RCons(_,_,_,_) => {
                let mut cur_type:&Type<()> = typ;
                let mut fields = vec![];
                loop { 
                    match cur_type {
                        Type::RCons(_,label,head_,tail_) => {
                            let field_type = self.translate_type(head_).unwrap();
                            let field = gt::Field::new(Symbol::from(label.as_str()), field_type);
                            fields.push(field);
                            cur_type = tail_;
                        }
                        _ => break
                    }
                }
                Ok(self.type_cache.record(vec![], fields))
            }
            _ => Ok(self.type_cache.hole())
        }
    }

    
    fn collect_app_type<'b>(&'a self,typ:&Type<()>) -> Vec<Result<gt::ArcType,TransferType>>  {
        let mut cur_type = typ;
        let mut ret_vec = vec![];
        loop { 
            match cur_type {
                Type::TypeApp(_,head,tail) => {
                    ret_vec.push(self.translate_type(head));
                    cur_type = tail;
                },
                _ => {  
                    ret_vec.push(self.translate_type(cur_type));
                    break;
                }
            }
        }
        ret_vec
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


#[test]
fn test_trans() {
   use gluon::{ThreadExt};
   use ast::types::{Module};
   use gluon::compiler_pipeline::{CompileValue};
   use gluon::vm::compiler::{Compiler};
   use gluon::base::symbol::*;
   use gluon::base::source::{FileMap};
   let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
   let module:Module = serde_json::from_str(first_purs_string.as_str()).unwrap();

   //dbg!(&module);

   let thread = gluon::new_vm();
   let type_cache = thread.global_env().type_cache();

   let trans =  Translate::new(type_cache);
   let vm_expr = trans.translate_module(&module);
   let core_expr = vm_expr.ok().unwrap();
   
   let mut symbols = Symbols::new();
   let sym_modules = SymbolModule::new("".into(), &mut symbols);
   let globals = &thread.global_env().get_globals().type_infos;
   thread.get_database_mut().implicit_prelude(false);
   let vm_state = thread.global_env();
   let source = FileMap::new("".to_string().into(), "".to_string());
   let mut compiler = Compiler::new(&globals,&vm_state,sym_modules,&source,"test".into(),true);
   
   
   //dbg!(&core_expr);
   let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(&core_expr).unwrap();
   //dbg!(&compiled_module);
   let metadata = std::sync::Arc::new(gluon::base::metadata::Metadata::default());
   let com:CompileValue<()> = CompileValue {
       expr:(),
       core_expr:gluon::vm::core::interpreter::Global {
        value: gluon::vm::core::freeze_expr( &trans.alloc, core_expr),
        info: Default::default(),
    },
       typ:type_cache.hole(),
       metadata,
       module:compiled_module
   };

   use gluon::compiler_pipeline::{Executable};
   let val = futures::executor::block_on( com.run_expr(&mut thread.clone().module_compiler(&mut thread.get_database()),thread.clone(),"","",()));
   dbg!(val.unwrap().value);
}
