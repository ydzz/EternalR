use std::io::Read;
use crate::grabtypes::{TypInfoEnv,TypeInfo};
use crate::translate::{Translate};
use ast::{from_reader};
use ast::types::Module;
use gluon::vm::core::{Allocator};
use std::sync::Arc;
use gluon::base::types::{ArcType,TypeCache,Field,Type};
use gluon::vm::core::{Expr};
use gluon::vm::thread::Thread;
use gluon::ThreadExt;
use gluon::base::symbol::{Symbols,SymbolModule,Symbol};
use gluon::base::source::{FileMap};
use gluon::vm::compiler::{Compiler as VMCompiler};
use pretty::{self};


pub struct  Compiler {
    type_env:Arc<TypInfoEnv>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { type_env: Arc::new(TypInfoEnv::default()) }
    }

    pub fn compile<R>(&self,corefn:&str,externs:R,thread:&Thread) where R:Read {
        let type_cahce =  thread.global_env().type_cache();
        self.buildin_prim(type_cahce,thread);

        let alloc = Arc::new(Allocator::new());
        let externs_file = from_reader(externs);
        let db = &mut thread.get_database();
        let compiler = thread.module_compiler(db);
        let mut trans = Translate::new(&alloc,type_cahce,self.type_env.clone());

        //core expr
        let mut ast_module:Module = serde_json::from_str(corefn).unwrap();
        let (vm_expr,_typ):(&Expr,ArcType) = trans.translate(&mut ast_module, externs_file,compiler).unwrap();
        let new = pretty::Arena::<()>::new();
        
        let doc = vm_expr.pretty(&new,gluon::vm::core::pretty::Prec::Top);
        let ss:String = doc.1.pretty(100).to_string();
        std::fs::write("out.glu", ss).unwrap();
        
        //byte module
        let mut symbols = Symbols::new();
        let sym_modules = SymbolModule::new("".into(), &mut symbols);
      
        let vm_state = thread.global_env();
        let source = FileMap::new(String::default(), String::default());
    
        let db = thread.get_database();
        let env =  db.as_env();
        let mut compiler = VMCompiler::new(&env,&vm_state,sym_modules,&source,"test".into(),true);
        let compiled_module:gluon::vm::compiler::CompiledModule = compiler.compile_expr(vm_expr).unwrap();
        //dbg!(&compiled_module);

       
       
        use crate::gluon::vm::thread::ThreadInternal;
        let closure = thread.global_env().new_global_thunk(&thread, compiled_module).unwrap();
        let vm1 = thread.clone();
        let exec_value = futures::executor::block_on(vm1.call_thunk_top(&closure)).unwrap();
        let field = exec_value.get_field("main").unwrap();
        let ret = futures::executor::block_on(vm1.execute_io_top(field.get_variant())).unwrap();
        dbg!(ret);
    }

   


    fn buildin_prim(&self,type_cache:&TypeCache<Symbol,ArcType>,thread:&Thread) {
        let mut symbols = Symbols::new();
        let true_sym = symbols.simple_symbol("True");
        let false_sym = symbols.simple_symbol("False");
        let bool_sym = symbols.simple_symbol("Bool");
        let true_field = Field::new(true_sym, type_cache.opaque());
        let false_field = Field::new(false_sym, type_cache.opaque());
        let bool_var = type_cache.variant(vec![true_field,false_field]);
        let alias = Type::alias(bool_sym, vec![], bool_var.clone());

        self.type_env.add_type_info(TypeInfo {
            qual_type_name:"prim.Bool".to_string(),
            type_name:"Bool".to_string(),
            gluon_type:alias,
            gluon_type2:Some(bool_var),
            type_str_vars:vec![],
            type_vars:vec![]
        });

        
        let env = thread.get_env();
        let alias = env.find_type_info("std.io.IO").unwrap();
       
        let arc_alias:ArcType<Symbol> = alias.into_type();
       
        
        self.type_env.add_type_info(TypeInfo {
            qual_type_name:"Main.IO".to_string(),
            type_name:"IO".to_string(),
            gluon_type: arc_alias.clone(),
            gluon_type2:None,
            type_str_vars:vec!["a".to_string()],
            type_vars:arc_alias.params().into()
        });
    }
}

#[test]
fn test_compile() {
    let source = std::fs::read_to_string("../tests/output/Main/corefn.json").unwrap();
    let externs = std::fs::File::open("../tests/output/Main/externs.cbor").unwrap();
    let compiler = Compiler::new();
    let thread = gluon::new_vm();

    use gluon::import::{add_extern_module};
   
    add_extern_module(&thread, "io", load_io_module);

    compiler.compile(source.as_str(), externs, &thread);
}


fn println(s: &str) -> IO<i32> {
    eprintln!("{}", s);
    IO::Value(0)
}

use gluon::vm::{self,api};
use gluon::vm::types::*;
use gluon::vm::api::{IO,generic::{A,B},TypedBytecode,primitive};
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
        bind => TypedBytecode::<FlatMap>::new("bind", 3, flat_map),
        pure => TypedBytecode::<Wrap>::new("pure", 2, wrap.clone()),
        println => primitive!(1,println),
    })
}