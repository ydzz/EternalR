use gluon::{RootedThread};
use ast::types::{Module};
use ast::{from_reader};
use er_compiler::translate::Translate;
use gluon::base::{symbol::{SymbolModule,Symbols}};
use gluon::vm::compiler::{Compiler,CompiledModule};
use gluon::base::source::{FileMap};
use gluon::{ThreadExt};
mod prim;
use std::sync::Arc;
use gluon::compiler_pipeline::{CompileValue};
use gluon::vm::api::{OpaqueValue,Hole,Getable,Record,FunctionRef};
use gluon::vm::{thread::RootedValue,Result as VMResult,core::Expr};
use gluon::compiler_pipeline::{Executable,ExecuteValue};
use gluon::vm::core::{Allocator};
use std::io::Read;
#[macro_use]
extern crate gluon;
pub struct EternalR {
    thread:RootedThread
}

impl<'a> EternalR {
    pub fn new() -> EternalR {
        let thread = gluon::new_vm();
        thread.get_database_mut().set_implicit_prelude(false);
        //thread.run_io(true);
        EternalR {
            thread
        }
    }

    pub fn load_vm_expr<'alloc,R>(&self,source:&str,externs:R,alloc:&'alloc Allocator<'alloc>) -> &'alloc Expr<'alloc> where R:Read {
        let externs_file = from_reader(externs);
        let db = &mut self.thread.get_database();
        let compiler = self.thread.module_compiler(db);
        let trans = Translate::new(alloc, self.thread.global_env().type_cache());

        let ast_module:Module = serde_json::from_str(source).unwrap();
        let core_expr:&'alloc Expr<'alloc> = trans.translate(ast_module, externs_file,compiler).unwrap();
        core_expr
    }

    pub fn compile_vm_expr<'alloc>(&self,vm_expr:&'alloc Expr<'alloc>) -> CompiledModule {
        let mut symbols = Symbols::new();
        let sym_modules = SymbolModule::new("".into(), &mut symbols);
      
        let vm_state = self.thread.global_env();
        let source = FileMap::new(String::default(), String::default());
    
        let db = self.thread.get_database();
        let env =  db.as_env();
        let mut compiler = Compiler::new(&env,&vm_state,sym_modules,&source,"test".into(),true);
        let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(vm_expr).unwrap();
        compiled_module
    }

    pub fn load_compile_module(&self,module_name:&str,compiled_module:&CompiledModule) {
        self.thread.load_script(module_name, "input");
    }

    pub fn run_purs_cf<R,T>(&'a mut self,source:&str,externs:R) -> T where R:std::io::Read,T: for<'value> Getable<'a,'value> {
        let alloc = Arc::new(Allocator::new());
        let vm_expr = self.load_vm_expr(source, externs, &alloc);
        //dbg!(core_expr);
        let compiled_module:CompiledModule = self.compile_vm_expr(vm_expr);
        //dbg!(&compiled_module);
        
        let metadata = std::sync::Arc::new(gluon::base::metadata::Metadata::default());
        let compile_value:CompileValue<()> = CompileValue {
            expr:(),
            core_expr:gluon::vm::core::interpreter::Global {
                        value: gluon::vm::core::freeze_expr( &alloc, vm_expr),
                        info: Default::default(),
                        },
            typ:self.thread.global_env().type_cache().hole(),
            metadata,
            module:compiled_module
        };

        
        let val = futures::executor::block_on( 
            compile_value.run_expr(
                &mut self.thread.clone().module_compiler(&mut self.thread.get_database()),self.thread.clone(),"","",()
            )
        ).unwrap();

        T::from_value(&self.thread, val.value.get_variant())
    }
}



use gluon::{new_vm,vm::thread::Thread,vm};
use gluon::import::{add_extern_module};


fn log_int(x: i32) -> i32 {
    eprintln!("log message {}",x);
    x   
}

fn load_int(vm: &Thread) -> vm::Result<vm::ExternModule> {
    vm::ExternModule::new(vm, primitive!(1, log_int))
}

#[test]
fn test_run() {
    let first_purs_string = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
    let externs = std::fs::File::open("tests/output/Main/externs.cbor").unwrap();
    let mut er = EternalR::new();
    add_extern_module(&er.thread, "log_int", load_int);
    prim::add_int_prim(&er.thread);
    let val: OpaqueValue<&Thread, Hole> = er.run_purs_cf(first_purs_string.as_str(),externs);
    
}

#[test]
fn test_gluon() {
    let vm = new_vm();
    let script = r#"
        let record = {varA = 12345 }
        let pi:Int = 123
        let const a b:Int -> Int -> Int = b
        {record , const,pi }
    "#;
    add_extern_module(&vm, "log_message", load_int);
    vm.get_database_mut().set_implicit_prelude(false);
    vm.run_io(true);
    vm.load_script("fuck", script).unwrap();
    
    //let val = vm.run_expr::<OpaqueValue<&Thread, Hole>>("Fuck", script).unwrap();
    let mut f:FunctionRef<fn(i32,i32) -> i32>  = vm.get_global("fuck.const").unwrap();
    let nn = f.call(1i32,2i32);

    dbg!(nn);
   
}

