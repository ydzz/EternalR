pub mod translate;
pub mod utils;
mod errors;
mod grabtypes;

#[macro_use]
extern crate collect_mac;


#[test]
fn test_first() {
   use gluon::check::typecheck::Typecheck;
   use gluon::base::symbol::{SymbolModule,Symbols};
   use gluon::{ThreadExt,Thread,RootedThread};
   use gluon::compiler_pipeline::*;

   let code_str = r#"
      let constFunc a b:Int -> Int -> Int = b
      constFunc 1 111
   "#;
  
   
   let mut thread = gluon::new_vm();
   thread.get_database_mut().implicit_prelude(false);
   let l = thread.load_script("F",code_str).unwrap();
   println!("end load");
   let v:i32 = thread.get_global("F").unwrap();
   dbg!(v);
   //let mut buffer = Vec::new();
   let mut string = String::default();
   let mut buffer2 = std::fs::File::create("foo.txt").unwrap();
   let mut serializer = serde_json::Serializer::new(&mut buffer2);
   
   futures::executor::block_on( thread.thread().compile_to_bytecode("test", code_str, &mut serializer) ) .unwrap();
}

