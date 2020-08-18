use ast::types::{self,Bind,Ann,Expr,Literal,Module};
use gluon_vm::core::{Expr as VMExpr,Literal as VMLiteral,self as vmcore};
use gluon_base::{pos::{BytePos,Span}};

pub enum  TranslateError {
    TranslateNotNanFloat
}

fn translate_module<'a>(module:&Module) {

} 

pub fn translate_bind<'a>(bind:&Bind<Ann>,pre_expr:&'a VMExpr<'a>) -> Result<VMExpr<'a>,TranslateError>  {
    match bind {
        Bind::NonRec(ann,id,expr) => {
            let vm_expr:VMExpr = translate_expr(&expr)?;
            let let_binding = vmcore::LetBinding {
                name:todo!(),
                expr:todo!(),
                span_start:todo!(),
            };
            Ok(VMExpr::Let(&let_binding,pre_expr))
        }
        Bind::Rec(_) => todo!()
    }
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
                    let val =  ordered_float::NotNan::new(*fnum)
                                              .map_err(|_| TranslateError::TranslateNotNanFloat)?;
                    Ok(VMLiteral::Float(val))
                },
            }
        }
        Literal::StringLiteral(str) => {
            todo!()
        }
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
   use ast::types::{Module};
   let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
   let module:Module = serde_json::from_str(first_purs_string.as_str()).unwrap();
   for decl in module.decls {
       dbg!(&decl);
      //let vm_expr = translate_bind(&decl,todo!());
   }
}
