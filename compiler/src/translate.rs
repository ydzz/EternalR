use ast::types::{self,Bind,Ann,Expr,Literal,Module,Type};
use gluon_vm::core::{Expr as VMExpr,Literal as VMLiteral,self as vmcore,Named,Allocator};
use gluon_base::{ast as gast,pos::{BytePos,Span,ByteIndex},types as gt,symbol::Symbol};
use std::sync::Arc;
use crate::utils::*;
pub struct Translate<'a> {
    type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>,
    alloc:Arc<Allocator<'a>>,
    dummy_symbol:gast::TypedIdent
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
            Literal::ObjectLiteral(record_map) => {
                let record_type = self.type_cache.record(vec![], vec![]);
                todo!()
            },
            _ => todo!()
        }
    }

    
    fn translate_type<'b>(&'a self,typ:&'b Type<()>) -> Result<gt::ArcType,&'b str>  {
        match &typ {
            Type::TypeConstructor(_,proper) => {
               match &proper.0 {
                   Some(qual_str) if qual_str == "Prim" => {
                        self.translate_prim_type( types::proper_name_as_str(&proper.1)) 
                   },
                   _ => Ok(self.type_cache.hole())
               }
            },
            Type::TypeApp(_,ta,tb) => {
                let ca = self.translate_type(ta);
                match ca {
                    Ok(ta) => {
                        let cb = self.translate_type(tb).unwrap();
                        let app:gt::ArcType = gt::Type::app(ta, collect![cb]);
                        return Ok(app);
                    },
                    Err("Record") => {
                        self.collect_record(tb);
                        todo!()
                    },
                    _ => todo!()
                }               
            }
            _ => Ok(self.type_cache.hole())
        }
    }

    fn collect_record(&'a self,typ:&Type<()>) {
        loop { 
            match typ {
                Type::RCons(_,_,_,_) => {
                    todo!()
                },
                _ => { break; }
            }
        }
    }

    fn translate_prim_type<'b>(&'a self,type_name:&'b str) -> Result<gt::ArcType,&'b str> {
        match type_name {
            "Int" => Ok(self.type_cache.int()),
            "Number" => Ok(self.type_cache.float()),
            "String" => Ok(self.type_cache.string()),
            "Char" => Ok(self.type_cache.char()),
            "Array" => Ok(self.type_cache.array_builtin()),
            "Record" => {
                Err(type_name)
            },
            _ => Ok(self.type_cache.hole())
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


#[test]
fn test_trans() {
   use gluon::{ThreadExt};
   use ast::types::{Module};
   use gluon::compiler_pipeline::{ExecuteValue,CompileValue};
   use gluon_vm::compiler::{Compiler};
   use gluon_base::symbol::*;
   use gluon_base::source::{FileMap};
   use gluon_vm::{types::TypeInfos,vm::GlobalVmState};
   let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
   let module:Module = serde_json::from_str(first_purs_string.as_str()).unwrap();

   dbg!(&module);

   let thread = gluon::new_vm();
   let type_cache = thread.global_env().type_cache();

   let trans =  Translate::new(type_cache);
   let vm_expr = trans.translate_module(&module);
   let core_expr = vm_expr.ok().unwrap();
   
   let mut symbols = Symbols::new();
   let mut sym_modules = SymbolModule::new("".into(), &mut symbols);
   let globals = &thread.global_env().get_globals().type_infos;
   let vm_state = thread.global_env();
   let source = FileMap::new("".to_string().into(), "".to_string());
   let mut compiler = Compiler::new(&globals,&vm_state,sym_modules,&source,"test".into(),true);
   
   
   dbg!(&core_expr);
   let compiled_module:gluon_vm::compiler::CompiledModule = compiler.compile_expr(&core_expr).unwrap();
   dbg!(&compiled_module);
   let metadata = std::sync::Arc::new(gluon::base::metadata::Metadata::default());
   let com:CompileValue<()> = CompileValue {
       expr:(),
       core_expr:gluon_vm::core::interpreter::Global {
        value: gluon_vm::core::freeze_expr( &trans.alloc, core_expr),
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
