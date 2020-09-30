use gluon::vm::api::de::{De};
use gluon::{RootedThread};
use ast::types::{Module};
use ast::{from_reader};
use er_compiler::translate::Translate;
use gluon::base::types::{ArcType};
use gluon::base::{symbol::{SymbolModule,Symbols}};
use gluon::base::metadata::{Metadata};
use gluon::vm::compiler::{Compiler,CompiledModule};
use gluon::base::source::{FileMap};
use gluon::{ThreadExt};
mod prim;
use std::sync::Arc;
use gluon::compiler_pipeline::{CompileValue,Module as ByteModule};
use gluon::vm::api::{OpaqueValue,Hole,Getable,Record,FunctionRef};
use gluon::vm::{thread::RootedValue,Result as VMResult,core::Expr};
use gluon::compiler_pipeline::{Executable,ExecuteValue};
use gluon::vm::core::{Allocator};
use std::io::Read;
use er_compiler::grabtypes::TypInfoEnv;
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

    pub fn load_vm_expr<'alloc,R>(&self,source:&str,externs:R,alloc:&'alloc Allocator<'alloc>) -> (&'alloc Expr<'alloc>,ArcType,Module) where R:Read {
        let externs_file = from_reader(externs);
        let db = &mut self.thread.get_database();
        let compiler = self.thread.module_compiler(db);
        let env = Arc::new(TypInfoEnv::default());
        let mut trans = Translate::new(alloc, self.thread.global_env().type_cache(),env);

        let mut ast_module:Module = serde_json::from_str(source).unwrap();
        let (core_expr,typ):(&'alloc Expr<'alloc>,ArcType) = trans.translate(&mut ast_module, externs_file,compiler).unwrap();
        (core_expr,typ,ast_module)
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

    pub fn compile_byte_module<R>(&self,source:&str,externs:R) -> ByteModule  where R:Read {
        let alloc = Arc::new(Allocator::new());
        let (vm_expr,typ,_m) = self.load_vm_expr(source, externs, &alloc);
        let compiled_module = self.compile_vm_expr(vm_expr);
        let byte_module = ByteModule {
            typ,
            module:compiled_module,
            metadata:Arc::new(Metadata::default())
        };
        byte_module
    } 

    pub fn load_core_fn<R>(&self,source:&str,externs:R) where R:Read {
        let alloc = Arc::new(Allocator::new());
        let (vm_expr,typ,module) = self.load_vm_expr(source, externs, &alloc);
        dbg!(vm_expr);
        let compiled_module = self.compile_vm_expr(vm_expr);
       
        let metadata = Arc::new(Metadata::default());
        let compile_value:CompileValue<()> = CompileValue {
            expr:(),
            core_expr:gluon::vm::core::interpreter::Global {
                        value: gluon::vm::core::freeze_expr( &alloc, vm_expr),
                        info: Default::default(),
                        },
            typ:typ.clone(),
            metadata:metadata.clone(),
            module:compiled_module
        };

        let val = futures::executor::block_on( 
            compile_value.run_expr(
                &mut self.thread.clone().module_compiler(&mut self.thread.get_database()),self.thread.clone(),"","",()
            )
        ).unwrap();
        
        self.thread.get_database_mut().set_global(module.name.as_str(), typ, metadata, &val.value);
    }
    /*
    pub fn run_purs_cf<R,T>(&'a mut self,source:&str,externs:R) -> T where R:std::io::Read,T: for<'value> Getable<'a,'value> {
        T::from_value(&self.thread, val.value.get_variant())
    }*/
}



use gluon::{new_vm,vm::thread::Thread,vm};
use gluon::import::{add_extern_module};


fn log_int() -> i32 {
    eprintln!("hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh log int: {}",444);
    66666  
}

fn load_int(vm: &Thread) -> vm::Result<vm::ExternModule> {
    vm::ExternModule::new(vm, primitive!(0, log_int))
}



#[test]
fn test_run() {
    let source = std::fs::read_to_string("tests/output/Main/corefn.json").unwrap();
    let externs = std::fs::File::open("tests/output/Main/externs.cbor").unwrap();
    let er = EternalR::new();
    add_extern_module(&er.thread, "log_int", load_int);
    prim::add_int_prim(&er.thread);
    
    er.load_core_fn(source.as_str(), externs);

    let num:i32 = er.thread.get_global("Main.main").unwrap();
    println!("Main.main is: {}",num);
}

#[test]
fn test_gluon() {
    use gluon::vm::api::de::{De};
    let vm = new_vm();
    let script = r#"
      let {__io_pure} = import! io
     
      let io'pure = __io_pure
      [io'pure 111]
    "#;
    add_extern_module(&vm, "io", load_io_module);
    add_extern_module(&vm, "other", load_other_module);
    vm.get_database_mut().set_implicit_prelude(false);
    vm.run_io(true);
   
    let _val = vm.run_expr::<OpaqueValue<&Thread,Hole>>("Fuck", script).unwrap().0;
    dbg!(_val);
    
    //println!("gluon: {}",val);
    //let mut f:FunctionRef<fn(i32,i32) -> i32>  = vm.get_global("fuck.const").unwrap();
    //let nn = f.call(1i32,2i32);

    //dbg!(nn);
   
}


fn __println(s: &str) -> IO<()> {
    println!("{}", s);
    IO::Value(())
}


use gluon::vm::types::*;
use gluon::vm::api::{IO,generic::{A,B},TypedBytecode};
fn load_other_module(vm: &Thread) -> vm::Result<vm::ExternModule> {
    type Wrap = fn(A) -> IO<A>;
    let wrap = vec![Pop(1), Return];
    vm::ExternModule::new(vm, record! {
        __otherfn => TypedBytecode::<Wrap>::new("io.__io_pure", 2, wrap.clone()),
    })
}

fn load_io_module(vm: &Thread) -> vm::Result<vm::ExternModule> {
    type FlatMap = fn(fn(A) -> IO<B>, IO<A>) -> IO<B>;
    type Wrap = fn(A) -> IO<A>;
    let flat_map = vec![
        // [f, m, ()]       Initial stack
        Call(1),     // [f, m_ret]       Call m ()
        PushInt(0),  // [f, m_ret, ()]   Add a dummy argument ()
        TailCall(2), /* [f_ret]          Call f m_ret () */
        Return,
    ];
    let wrap = vec![Pop(1), Return];
    vm::ExternModule::new(vm, record! {
        __io_bind => TypedBytecode::<FlatMap>::new("io.__io_bind", 3, flat_map),
        __io_pure => TypedBytecode::<Wrap>::new("io.__io_pure", 2, wrap.clone()),
        __println => TypedBytecode::<Wrap>::new("io.__println", 2, wrap),
    })
}