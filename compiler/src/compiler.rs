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
use gluon::vm::compiler::{Compiler as VMCompiler,CompiledModule};
use pretty::{self,BoxAllocator,DocBuilder};
pub struct  Compiler {
    type_env:Arc<TypInfoEnv>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { type_env: Arc::new(TypInfoEnv::default()) }
    }

    pub fn compile<R>(&self,corefn:&str,externs:R,thread:&Thread) where R:Read {
        let type_cahce =  thread.global_env().type_cache();
        self.buildin_prim(type_cahce);

        let alloc = Arc::new(Allocator::new());
        let externs_file = from_reader(externs);
        let db = &mut thread.get_database();
        let compiler = thread.module_compiler(db);
        let mut trans = Translate::new(&alloc,type_cahce,self.type_env.clone());

        //core expr
        let mut ast_module:Module = serde_json::from_str(corefn).unwrap();
        let (vm_expr,_typ):(&Expr,ArcType) = trans.translate(&mut ast_module, externs_file,compiler).unwrap();
        let new = pretty::Arena::<()>::new();
        let mut doc = vm_expr.pretty(&new,gluon::vm::core::pretty::Prec::Top);
        let ss:String = doc.1.pretty(100).to_string();
        std::fs::write("out.glu", ss);
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


        use gluon::base::metadata::{Metadata};
        let metadata = Arc::new(Metadata::default());
        use gluon::compiler_pipeline::{CompileValue,};
        use gluon::compiler_pipeline::Executable;
        let compile_value:CompileValue<()> = CompileValue {
            expr:(),
            core_expr:gluon::vm::core::interpreter::Global {
                        value: gluon::vm::core::freeze_expr( &alloc, vm_expr),
                        info: Default::default(),
                        },
            typ:_typ.clone(),
            metadata:metadata.clone(),
            module:compiled_module
        };

        let val = futures::executor::block_on( 
            compile_value.run_expr(
                &mut thread.clone().module_compiler(&mut thread.get_database()),thread.clone(),"","",()
            )
        ).unwrap();
        dbg!(&val.value);
    }

   


    fn buildin_prim(&self,type_cache:&TypeCache<Symbol,ArcType>) {
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

