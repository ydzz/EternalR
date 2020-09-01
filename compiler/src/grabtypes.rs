use ast::types::Module;
use ast::types::{self,Bind,Expr,Ann};

pub struct GrabTypeInfo {

}

pub fn grab_type_info(module:&Module) -> GrabTypeInfo {
    for decl in module.decls.iter() {
        match decl {
            Bind::NonRec(ann,id,expr) => {
                let expr_ref:&Expr<Ann> = expr;
                match expr_ref {
                    Expr::Constructor(ann,p0,p1,idents) => {
                        let type_name = types::proper_name_as_str(p0);
                        let ctor_name = types::proper_name_as_str(p1);
                        let ident_strs:Vec<String> = idents.iter().map(|i| String::from(i.as_str().unwrap())).collect();
                       
                        ()
                    },
                    
                    Expr::Literal(_, _) => {}
                    Expr::Accessor(_, _, _) => {}
                    Expr::ObjectUpdate(_, _, _) => {}
                    Expr::Abs(_, _, _) => {}
                    Expr::App(_, _, _) => {}
                    Expr::Var(_, _) => {}
                    Expr::Case(_, _, _) => {}
                    Expr::Let(_, _, _) => {}
                }
            }
            _ => ()
        }
    }
    todo!()
}



#[test]
fn test_type() {
   let source = std::fs::read_to_string("../tests/output/Main/corefn.json").unwrap();
   let module:Module = serde_json::from_str(source.as_str()).unwrap();

   grab_type_info(&module);
}