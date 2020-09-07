use std::io::Read;
use crate::grabtypes::TypInfoEnv;
use crate::translate::{Translate};
use ast::{from_reader};
use ast::types::Module;
use gluon::vm::core::{Allocator};
use std::sync::Arc;
use gluon::base::types::{ArcType};
use gluon::vm::core::Expr;
use gluon::vm::thread::Thread;
use gluon::ThreadExt;
use gluon::base::symbol::{Symbols,SymbolModule};
use gluon::base::source::{FileMap};
use gluon::vm::compiler::{Compiler as VMCompiler,CompiledModule};

pub struct  Compiler {
    type_env:Arc<TypInfoEnv>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { type_env: Arc::new(TypInfoEnv::default()) }
    }

    pub fn compile<R>(&self,corefn:&str,externs:R,thread:&Thread) where R:Read {
        let alloc = Arc::new(Allocator::new());
        let externs_file = from_reader(externs);
        let db = &mut thread.get_database();
        let compiler = thread.module_compiler(db);
        let trans = Translate::new(&alloc, thread.global_env().type_cache(),self.type_env.clone());

        //core expr
        let mut ast_module:Module = serde_json::from_str(corefn).unwrap();
        let (vm_expr,_typ):(&Expr,ArcType) = trans.translate(&mut ast_module, externs_file,compiler).unwrap();
        dbg!(&vm_expr);
        //byte module
        let mut symbols = Symbols::new();
        let sym_modules = SymbolModule::new("".into(), &mut symbols);
      
        let vm_state = thread.global_env();
        let source = FileMap::new(String::default(), String::default());
    
        let db = thread.get_database();
        let env =  db.as_env();
        let mut compiler = VMCompiler::new(&env,&vm_state,sym_modules,&source,"test".into(),true);
        let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(vm_expr).unwrap();

        dbg!(compiled_module);
    }
}

#[test]
fn test_compile() {
    let source = std::fs::read_to_string("../tests/output/Main/corefn.json").unwrap();
    let externs = std::fs::File::open("../tests/output/Main/externs.cbor").unwrap();
    let compiler = Compiler::new();
    let thread = gluon::new_vm();
    compiler.compile(source.as_str(), externs, &thread);
}