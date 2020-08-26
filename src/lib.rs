use gluon::{RootedThread};
use ast::types::{Module};
use er_compiler::translate::Translate;
use gluon::base::{symbol::{SymbolModule,Symbols}};
use gluon::vm::compiler::{Compiler};
use gluon::base::source::{FileMap};
use gluon::{ThreadExt};
use gluon::compiler_pipeline::{CompileValue};
#[macro_use]
extern crate gluon;
pub struct EternalR {
    thread:RootedThread
}

impl EternalR {
    pub fn new() -> EternalR {
        let thread = gluon::new_vm();
        EternalR {
            thread
        }
    }

    pub fn run_purs_cf(&self,source:&str) {
        let ast_module:Module = serde_json::from_str(source).unwrap();
        let translate = Translate::new(self.thread.global_env().type_cache());
        let vm_expr = translate.translate_module(&ast_module);
        let core_expr = vm_expr.ok().unwrap();

        let mut symbols = Symbols::new();
        let sym_modules = SymbolModule::new("".into(), &mut symbols);
        let globals = &self.thread.global_env().get_globals().type_infos;
        let vm_state = self.thread.global_env();
        let source = FileMap::new("".to_string().into(), source.to_string());
        let mut compiler = Compiler::new(&globals,&vm_state,sym_modules,&source,"test".into(),true);
        let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(&core_expr).unwrap();

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

#[test]
fn test_run() {
    let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
    let er = EternalR::new();
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

fn log_message(x: i32) -> i32 {
    println!("log message {}",x);
    x   
}

fn load_factorial(vm: &Thread) -> vm::Result<vm::ExternModule> {
    vm::ExternModule::new(vm, primitive!(1, log_message))
}