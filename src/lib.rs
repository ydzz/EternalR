use gluon::{RootedThread};
use ast::types::{Module};
use er_compiler::translate::Translate;
use gluon::base::{types::{ArcType},symbol::{SymbolModule,Symbols}};
use gluon::vm::compiler::{Compiler,CompilerEnv};
use gluon::base::source::{FileMap};
use gluon::{ThreadExt};
use gluon::vm::api::{FunctionRef};
use gluon::query::{AsyncCompilation};

use gluon::compiler_pipeline::{CompileValue};
#[macro_use]
extern crate gluon;
pub struct EternalR {
    thread:RootedThread
}

impl EternalR {
    pub fn new() -> EternalR {
        let thread = gluon::new_vm();
        thread.get_database_mut().set_implicit_prelude(false);
        //thread.run_io(true);
        EternalR {
            thread
        }
    }

    pub fn run_purs_cf(&mut self,source:&str) {
        let global = futures::executor::block_on( async {
            let mut db = self.thread.get_database();
            let mut compiler =  self.thread.module_compiler(&mut db);
            let sym = compiler.database.import("log_message".into()).await.unwrap();
           
            sym
         });
         
        let ast_module:Module = serde_json::from_str(source).unwrap();
        let translate = Translate::new(self.thread.global_env().type_cache());
        let vm_expr = translate.translate_module(&ast_module,global);
        let core_expr = vm_expr.ok().unwrap();

        dbg!(core_expr);

        let mut symbols = Symbols::new();
        let sym_modules = SymbolModule::new("".into(), &mut symbols);
      
        let vm_state = self.thread.global_env();
        let source = FileMap::new("".to_string().into(), source.to_string());
       
     
        let mut db = self.thread.get_database();
        let env = db.as_env();
        let mut compiler = Compiler::new(&env,&vm_state,sym_modules,&source,"test".into(),true);
        
        let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(&core_expr).unwrap();
        dbg!(&compiled_module);
        let metadata = std::sync::Arc::new(gluon::base::metadata::Metadata::default());
        let compile_value:CompileValue<()> = CompileValue {
            expr:(),
            core_expr:gluon::vm::core::interpreter::Global {
                        value: gluon::vm::core::freeze_expr( &translate.alloc, core_expr),
                        info: Default::default(),
                        },
            typ:self.thread.global_env().type_cache().hole(),
            metadata,
            module:compiled_module
        };

        use gluon::compiler_pipeline::{Executable};
        let val = futures::executor::block_on( 
            compile_value.run_expr(
                &mut self.thread.clone().module_compiler(&mut self.thread.get_database()),self.thread.clone(),"","",()
            )
        );
        dbg!(&val.unwrap().value);
    }
}



use gluon::{new_vm,vm::api::IO,vm::thread::Thread,vm};
use gluon::import::{add_extern_module};



fn log_message(x: i32) -> i32 {
    println!("log message {}",x);
    x   
}

fn load_factorial(vm: &Thread) -> vm::Result<vm::ExternModule> {
    vm::ExternModule::new(vm, primitive!(1, log_message))
}

#[test]
fn test_run() {
    let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
    let mut er = EternalR::new();
    add_extern_module(&er.thread, "log_message", load_factorial);
  
    er.run_purs_cf(first_purs_string.as_str());
}



#[test]
fn test_gluon() {
    let vm = new_vm();
    let script = r#"
        let log_message = import! log_message
        log_message 2
    "#;
    add_extern_module(&vm, "log_message", load_factorial);
  
    vm.get_database_mut().set_implicit_prelude(false);
    vm.run_io(true);
    
    let val = vm.run_expr::<i32>("Fuck", script).unwrap();
   
   
    dbg!(val.0);
}

