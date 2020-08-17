pub mod translate;
use gluon_vm::{types::TypeInfos,vm::{GlobalVmState},compiler::Compiler};
use gluon_base::{symbol::{SymbolModule,Symbols},source::{FileMap}};


#[test]
fn test_first() {
   
   //env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).init();
   
   
   use gluon::{ThreadExt};
   use gluon::{base::{symbol::{Symbols,SymbolModule}}};
   let code_str = r#"
   let arr:Array Int = [1,2]
   arr "#;

   let thread = gluon::new_vm();
   
  
   //thread.load_script("in",code_str);
  
   gluon::base::mk_ast_arena!(arena);
   let type_cache = gluon::base::types::TypeCache::new();   

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
   }
}
