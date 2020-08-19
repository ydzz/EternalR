use ast::types::{self,Bind,Ann,Expr,Literal,Module};
use gluon_vm::core::{Expr as VMExpr,Literal as VMLiteral,self as vmcore,Named,Allocator};
use gluon_base::{ast as gast,pos::{BytePos,Span,ByteIndex},types as gt,symbol::Symbol};
use std::sync::Arc;
pub struct Translate<'a> {
    type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>,
    alloc:Arc<Allocator<'a>> 
}

impl<'a> Translate<'a> {
    pub fn new(type_cache:&'a gt::TypeCache<Symbol,gt::ArcType>) -> Self {
        Translate {
            type_cache,
            alloc:Arc::new(Allocator::new())
        }
    }

    pub fn translate_module(&'a self,module:&Module) -> Result<&'a VMExpr<'a>,TranslateError> {
        let export_module = VMExpr::Const(VMLiteral::Int(666),Span::new(ByteIndex(0), ByteIndex(0)));
        
        
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
                let vm_expr:&'a VMExpr = self.alloc.arena.alloc(translate_expr(&expr)?);
                let span = source_span_to_byte_span(&ann.0);
                let id_name = match id {
                    types::Ident::Ident(str) => str.to_owned(),
                    _ => "".to_string()
                };
                let name = gast::TypedIdent {
                    name:Symbol::from(id_name),
                    typ:self.type_cache.int()
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
}

pub enum  TranslateError {
    TranslateNotNanFloat
}





fn translate_expr<'a>(expr:&Expr<Ann>) -> Result<VMExpr<'a>,TranslateError>  {
    match expr {
        Expr::Literal(ann,lit) => {
            let vm_lit = translate_literal(lit)?;
            let byte_pos = source_span_to_byte_span(&ann.0);
            Ok(VMExpr::Const(vm_lit,byte_pos)) 
        },
        _ => todo!()
    }
}

fn translate_literal<'a,T>(lit:&Literal<Box<T>>) -> Result<VMLiteral,TranslateError> {
    match lit {
        Literal::NumericLiteral(rnum) => {
            match rnum {
                Ok(inum) => Ok(VMLiteral::Int(*inum as i64)),
                Err(fnum) => {
                    let val =  ordered_float::NotNan::new(*fnum).map_err(|_| TranslateError::TranslateNotNanFloat)?;
                    Ok(VMLiteral::Float(val))
                },
            }
        }
        Literal::StringLiteral(str) => Ok(VMLiteral::String(Box::from(&str[..]))),
        Literal::CharLiteral(chr) => Ok(VMLiteral::Char(*chr)),
        _ => todo!()
    }
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

   let thread = gluon::new_vm();
   let type_cache = thread.global_env().type_cache();

   let trans =  Translate::new(type_cache);
   let vm_expr = trans.translate_module(&module);
   let core_expr = vm_expr.ok().unwrap();
   
   let mut symbols = Symbols::new();
   let mut sym_modules = SymbolModule::new("".into(), &mut symbols);
   let globals = TypeInfos::new();
   let vm_state = GlobalVmState::new();
   let source = FileMap::new("".to_string().into(), "".to_string());
   let mut compiler = Compiler::new(&globals,&vm_state,sym_modules,&source,"test".into(),false);
   
   dbg!(&core_expr);
   let module:gluon_vm::compiler::CompiledModule = compiler.compile_expr(&core_expr).unwrap();
   dbg!(&module);
   let metadata = std::sync::Arc::new(gluon::base::metadata::Metadata::default());
   let com:CompileValue<()> = CompileValue {
       expr:(),
       core_expr:gluon_vm::core::interpreter::Global {
        value: gluon_vm::core::freeze_expr( &trans.alloc, core_expr),
        info: Default::default(),
    },
       typ:type_cache.hole(),
       metadata,
       module
   };

   use gluon::compiler_pipeline::{Executable};
   let val = futures::executor::block_on( com.run_expr(&mut thread.clone().module_compiler(&mut thread.get_database()),thread.clone(),"","",()));
  
}
