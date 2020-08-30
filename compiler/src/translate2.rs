use gluon::vm::core::{Allocator};
use gluon::base::types::{ArcType,TypeCache,Field};
use gluon::base::symbol::{Symbol,Symbols};
use gluon::vm::core::{Expr as VMExpr};
use std::cell::RefCell;
use ast::{ExternsFile};
use ast::types::{Module,Bind,Ident,Ann};
use crate::errors::TranslateError;
use std::sync::Arc;
pub struct Translate<'vm,'alloc> {
   pub alloc:Arc<Allocator<'alloc>>,
   type_cache:&'vm TypeCache<Symbol,ArcType>,
   symbols: RefCell<Symbols>,
}

impl<'vm,'alloc> Translate<'vm,'alloc> {
    pub fn new(alloc:Arc<Allocator<'alloc>>,type_cache:&'vm TypeCache<Symbol,ArcType>) -> Self {
        Translate {
            alloc,
            type_cache,
            symbols:RefCell::new(Symbols::new())
        }
    }

    pub fn translate(&self,module:Module,externs_file:ExternsFile) -> Result<&'alloc VMExpr<'alloc>,TranslateError> {
        let export_expr = self.translate_exports(&module.exports, &externs_file);
        todo!()
    }

    fn translate_exports(&self,exports:&Vec<Ident>,externs_file:&ExternsFile) ->  Result<&'alloc VMExpr<'alloc>,TranslateError> {
        let type_dic = externs_file.decl_type_dic();
        let mut field_types:Vec<Field<Symbol>> = vec![];
        let mut fields:Vec<VMExpr> = vec![];
        for id in exports {
            let id_name = self.id2str(id)?;
            let typ = *type_dic.get(id_name).ok_or(TranslateError::NotFindType)?;
            
        }
        todo!()
    }



    fn id2str<'a>(&self,id:&'a Ident) -> Result<&'a str,TranslateError> {
        match id.as_str() {
            Some(s) => Ok(s),
            None => Err(TranslateError::IdentToString)
        }
    }
}
