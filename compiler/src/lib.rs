pub mod translate;
pub mod utils;
#[macro_use]
extern crate collect_mac;

#[test]
fn test_first() {
   use gluon::check::typecheck::Typecheck;
   use gluon::base::symbol::{SymbolModule,Symbols};
   use gluon::{ThreadExt};


   let code_str = r#"
      let constFunc a b:Int -> Int -> Int = b
      constFunc 1 2
   "#;
  
   
   let mut thread = gluon::new_vm();
   thread.get_database_mut().implicit_prelude(false);

   let val = thread.run_expr::<i32>("",code_str);
   println!("eval value {:?}",val);
   /*
   use gluon::{ThreadExt};
   use gluon::{base::{symbol::{Symbols,SymbolModule}}};
   let code_str = r#"
      let arr = [1,2,3]
      in arr
   "#;
   
   let thread = gluon::new_vm();

   
  
   let globals = TypeInfos::new();
   let vm_state = GlobalVmState::new();
   let source = FileMap::new("".to_string().into(), "".to_string());

   let mut compiler = Compiler::new(&globals,&vm_state,sym_modules,&source,"test".into(),false);
   let module = compiler.compile_expr(&core_expr).unwrap();
   dbg!(module);

*/
   
   /*
   gluon::base::mk_ast_arena!(arena);
  

   let mut symbols = Symbols::new();
   let mut sym_modules = SymbolModule::new("".into(), &mut symbols);
   let ret:_ = gluon_parser::parse_expr(arena.borrow(), &mut sym_modules, &type_cache, code_str);
  
  
   match ret {
      Ok(expr) => {
         
         //dbg!(&expr);
         println!("================================");
         let env = thread.get_env();
         let translator = gluon::vm::core::Translator::new(&env);
         let mut core_expr = translator.translate_expr(&expr);
         dbg!(&core_expr);
         println!("================================");
         let globals = TypeInfos::new();
         let vm_state = GlobalVmState::new();
         let source = FileMap::new("".to_string().into(), "".to_string());
         let mut compiler = Compiler::new(&globals,&vm_state,
                                           sym_modules,
                               &source,"test".into(),false);
         let module = compiler.compile_expr(&core_expr).unwrap();
         dbg!(module);
        
      },
      Err(errs) => {
         for err in errs {
            dbg!(err.value);
         }
      }
   }*/
}

