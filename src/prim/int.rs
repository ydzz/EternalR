use gluon::vm::{self,thread::Thread,ExternModule};
use gluon::import::add_extern_module;

pub fn add_int_prim(thread:&Thread) {
    add_extern_module(thread, "prim_int", make_int_prim);
}

fn make_int_prim(thread:&Thread) -> vm::Result<ExternModule> {
    ExternModule::new(thread, record! {
        
    })
}