use crate::errors::TranslateError;
use crate::grabtypes::TypInfoEnv;
use crate::utils::*;
use ast::types::SourceSpan;
use ast::types::{
    self, proper_name_as_str, Ann, Bind, Binder, Expr, Ident, Literal, Module, Type as AstType,
};
use ast::ExternsFile;
use gluon::base::ast::TypedIdent;
use gluon::base::kind::Kind;
use gluon::base::pos::BytePos;
use gluon::base::pos::{ByteIndex, Span};
use gluon::base::symbol::{Symbol, SymbolData, Symbols};
use gluon::base::types::{ArcType, Field, Generic, KindedIdent, Type, TypeCache,BuiltinType};
use gluon::query::AsyncCompilation;
use gluon::base::kind::{ArcKind};
use std::collections::HashMap;
use gluon::vm::core::{Allocator, Alternative, Named, Pattern};
use gluon::vm::core::{Closure, Expr as VMExpr, LetBinding, Literal as VMLiteral};
use gluon::ModuleCompiler;
use std::cell::RefCell;
use std::sync::Arc;
use std::collections::HashSet;

pub struct Translate<'vm, 'alloc> {
    pub alloc: &'alloc Allocator<'alloc>,
    pub type_cache: &'vm TypeCache<Symbol, ArcType>,
    pub symbols: RefCell<Symbols>,
    pub type_env: Arc<TypInfoEnv>,

    pub cur_typeclass_args:HashSet<String>
}

impl<'vm, 'alloc> Translate<'vm, 'alloc> {
    pub fn new(
        alloc: &'alloc Allocator<'alloc>,
        type_cache: &'vm TypeCache<Symbol, ArcType>,
        type_env: Arc<TypInfoEnv>,
    ) -> Self {
        Translate {
            alloc,
            type_cache,
            symbols: RefCell::new(Symbols::new()),
            type_env,
            cur_typeclass_args:HashSet::new()
        }
    }

    pub fn translate(&mut self,module: &mut Module,externs_file: ExternsFile,mut compiler: ModuleCompiler<'_, '_>) 
    -> Result<(&'alloc VMExpr<'alloc>, ArcType), TranslateError> {
        self.grab_type_info(module);
        let (export_expr, typ): (VMExpr<'alloc>, ArcType) = self.translate_exports(&module.exports, &externs_file)?;
        let mut pre_expr = self.alloc.arena.alloc(export_expr);
        for bind in module.decls.iter().rev() {
            let (let_binding, _) = self.translate_bind_item(bind)?;
            let let_expr = VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding), pre_expr);
            pre_expr = self.alloc.arena.alloc(let_expr);
        }
        let foreign = self.translate_foreign(&module, pre_expr, &mut compiler)?;
        Ok((foreign, typ.clone()))
    }

    fn translate_bind_item(&self,bind: &Bind<Ann>) -> Result<(LetBinding<'alloc>, ArcType), TranslateError> {
        match bind {
            Bind::NonRec(ann, id, expr) => {
                let id_name = self.id2str(id)?;
                let mut expr_info: TExprInfo<'alloc> = self.translate_expr(&expr, id_name)?;
                let span = source_span_to_byte_span(&ann.0);
                let mut name = TypedIdent {
                    name: self.simple_symbol(id_name),
                    typ: expr_info.typ.clone(),
                };
                let named = if expr_info.closure.len() == 0 {
                    let expr = expr_info.take_expr();
                    Named::Expr(self.alloc.arena.alloc(expr))
                } else {
                    name = TypedIdent::new(self.simple_symbol(""));
                    Named::Recursive(expr_info.closure)
                };
                let let_binding = LetBinding {
                    name,
                    expr: named,
                    span_start: span.start(),
                };
                Ok((let_binding, expr_info.typ.clone()))
            }
            Bind::Rec(_) => todo!(),
        }
    }

    fn search_field(&self,typ:&ArcType<Symbol>,name:&str) -> Option<Field<Symbol>> {
        if let Type::Record(row) = &**typ {
            if let Type::ExtendRow {fields,..} = &**row {
                let fields_arr:&Vec<Field<Symbol>> = fields;
                for field in fields_arr {
                    if field.name.as_str() == name {
                        return Some(field.clone())
                    }
                }
            }
        }

        None
    }
    fn translate_foreign(
        &self,
        module: &Module,
        pre_expr: &'alloc VMExpr<'alloc>,
        compiler: &mut ModuleCompiler<'_, '_>,
    ) -> Result<&'alloc VMExpr<'alloc>, TranslateError> {
        let zero_span = Span::new(BytePos(0), BytePos(0));
        let mut cur_expr = pre_expr;
        let mut module_imports:HashMap<String,Vec<String>> = HashMap::new();
        for ident in module.foreign.iter() {
            let ident_name = self.id2str(ident)?;
            if ident_name.starts_with("primcore'") {
                continue;
            }
            let (module_name,func_name) = split_last(ident_name, '\'');
            if !module_imports.contains_key(module_name) {
                module_imports.insert(module_name.to_string(), vec![]);
            }
            let func_list = module_imports.get_mut(module_name).unwrap();
            func_list.push(func_name.to_string());
        }
       
        for (m_name,list) in module_imports.iter() {
            let import_ident = futures::executor::block_on(compiler.database.import(m_name.clone())).unwrap();
            let module_type = import_ident.typ.clone();
            let mut fields:Vec<(TypedIdent,Option<Symbol>)> = vec![];
            for fname in list {
               let field = self.search_field(&module_type, fname.as_str()).unwrap();
               let typed_ident = TypedIdent::new2(field.name, field.typ);
               let mut clone_name = m_name.clone();
               clone_name.push('\'');
               clone_name.push_str(fname.as_str());
               let let_sym_name = self.simple_symbol(clone_name.as_str());
               fields.push((typed_ident,Some(let_sym_name)));
            }
           let pattern = Pattern::Record {
                typ:module_type,
                fields
            };
            let alt_arr = vec![Alternative {pattern, expr:cur_expr }];
            let alt = self.alloc.alternative_arena.alloc_fixed(alt_arr);
            let match_ident = self.alloc.alloc(VMExpr::Ident(import_ident.clone(),zero_span));
            let match_expr = VMExpr::Match(match_ident,alt);
            cur_expr = self.alloc.alloc(match_expr); 
        }
        Ok(cur_expr)
    }

    pub fn translate_expr(
        &self,
        expr: &Expr<Ann>,
        bind_name: &str,
    ) -> Result<TExprInfo<'alloc>, TranslateError> {
        match expr {
            Expr::Literal(ann, lit) => {
                let lit_type = if let Some(v) = ann.2.as_ref() {
                    let t = self.translate_type(v,None).map_err(|_| TranslateError::TypeError)?;
                    t.typ()
                } else {
                    self.type_cache.hole()
                };
                let vmexpr = self.translate_literal(lit, ann, lit_type.clone())?;
                Ok(TExprInfo::new(vmexpr, lit_type, &ann.0))
            }
            Expr::Var(ann, qual) => {
                let typ;
                if ann.2.as_ref().is_none() {
                    typ = TTypeInfo::new(self.type_cache.hole());
                } else {
                    typ = self
                        .translate_type(ann.2.as_ref().unwrap(),None)
                        .map_err(|_| TranslateError::TypeError)?;
                }

                let name = qual.1.as_str().unwrap();
                let ident = TypedIdent {
                    typ: typ.typ(),
                    name: self.simple_symbol(name),
                };
                if name.chars().next().unwrap().is_uppercase() {
                    let data_expr = VMExpr::Data(ident, &[], source_span_to_byte_span(&ann.0).start());
                    Ok(TExprInfo::new(data_expr, typ.typ(), &ann.0))
                } else {
                    let ident_expr = VMExpr::Ident(ident, source_span_to_byte_span(&ann.0));
                    Ok(TExprInfo::new(ident_expr, typ.typ(), &ann.0))
                }
            }
            Expr::Abs(ann, ident, abs_expr) => {
                if let Some(types::Meta::IsTypeClassMember) = &ann.1 {
                    let type_class_name = self.id2str(ident)?;
                    return self.gen_typeclass_member(bind_name, type_class_name, ann, expr,None);
                }
                let typ = self.translate_type(ann.2.as_ref().unwrap(),None).map_err(|_| TranslateError::TypeError)?;
                let arg_name = ident.as_str().unwrap();
                let mut args: Vec<TypedIdent> = vec![TypedIdent::new2(
                    self.symbols.borrow_mut().simple_symbol(arg_name),
                    typ.args[0].clone(),
                )];
                
                let mut cur_expr: &Expr<Ann> = abs_expr;
                let mut idx = 1;
                loop {
                    match cur_expr {
                        Expr::Abs(_, ident, expr) => {
                            let arg_name = ident.as_str().unwrap();
                            args.push(TypedIdent::new2(
                                self.symbols.borrow_mut().simple_symbol(arg_name),
                                typ.args[idx].clone(),
                            ));
                            cur_expr = expr;
                        }
                        expr => {
                            cur_expr = expr;
                            break;
                        }
                    }
                    idx += 1;
                }
                let typ2 = typ.typ();
                let mut expr_body = self.translate_expr(cur_expr, "")?;
                let pos = source_span_to_byte_span(&ann.0);
                let closure = Closure {
                    pos: pos.start(),
                    name: TypedIdent::new2(self.simple_symbol(bind_name) , typ.typ()),
                    args,
                    expr: self.alloc.arena.alloc(expr_body.take_expr()),
                };
            
                let tinfo = TExprInfo::new_closure(typ2, vec![closure], &ann.0);

                Ok(tinfo)
            }
            Expr::App(ann, a, b) => {
                if let Some(types::Meta::IsTypeClassConstructor) = &ann.1 {
                    return self.gen_type_class_instance(a, b, bind_name,None);
                }
                let app_type = if let Some(v) = ann.2.as_ref() {
                    let typ = self.translate_type(ann.2.as_ref().unwrap(),None).map_err(|_| TranslateError::TypeError)?;
                    typ.typ()
                } else {
                    self.type_cache.hole()
                };
               
                let mut expr_b = self.translate_expr(b, "")?;
                let expr2 = if expr_b.closure.len() == 0 {
                    expr_b.take_expr()
                } else {
                    self.gen_closure_let(ann, expr_b)
                };
                let mut args:Vec<VMExpr<'alloc>> = vec![expr2];
                let mut cur_arg: &Expr<Ann> = a;
                let ename_expr;
              
                loop {
                    match cur_arg {
                        Expr::App(ann, la, lb) => {
                            let mut arg_expr = self.translate_expr(lb, "")?;
                            if arg_expr.closure.len() > 0 {
                                let let_expr:VMExpr<'alloc> = self.gen_closure_let(ann,arg_expr);
                                args.push(let_expr);
                            } else {
                                args.push(arg_expr.take_expr());
                            }
                            cur_arg = la;
                        },
                        Expr::Var(_,qual) => {
                          
                            let name = qual.1.as_str().unwrap_or_default();
                           
                            if name.chars().next().unwrap().is_uppercase() {
                                ename_expr = Err(qual.1.as_str().unwrap_or_default());
                            } else {
                                let mut texpr = self.translate_expr(cur_arg, "")?;
                                ename_expr = Ok(texpr.take_expr());
                            }
                            break;
                        },
                        expr => {
                            let mut texpr = self.translate_expr(expr, "")?;
                            ename_expr = Ok(texpr.take_expr());
                            break;
                        }
                    }
                }
                let args = self.alloc.arena.alloc_fixed(args.drain(0..).rev());

                match ename_expr {
                    Ok(name_expr) => {
                        let ea = self.alloc.arena.alloc(name_expr);
                        match ea {
                            VMExpr::Ident(id, _) if id.name.as_str().starts_with("primcore'") => {
                                let prim_name = Translate::replace_prim_name(id.name.as_str());
                                let sym = self.symbols.borrow_mut().simple_symbol(prim_name);
                                let prim_id = TypedIdent::new2(sym, id.typ.clone());
                                let new_expr = VMExpr::Ident(prim_id, source_span_to_byte_span(&ann.0));
                                let new_ea = self.alloc.arena.alloc(new_expr);
                                Ok(TExprInfo::new(VMExpr::Call(new_ea, args), app_type.clone(), &ann.0))
                            }
                            _ => Ok(TExprInfo::new(VMExpr::Call(ea, args),app_type.clone(), &ann.0)),
                        }
                    },
                    Err(type_ctor_name) => {
                      
                        let ident = TypedIdent::new2(self.simple_symbol(type_ctor_name), app_type.clone());
                        let data_expr = VMExpr::Data(ident, args, source_span_to_byte_span(&ann.0).start());
                        Ok(TExprInfo::new(data_expr, app_type, &ann.0))
                    }
                }
            }
            Expr::Accessor(ann, name, expr) => {
                let typ = self
                    .translate_type(ann.2.as_ref().unwrap(),None)
                    .map_err(|_| TranslateError::TypeError)?;
                let mut expr_info = self.translate_expr(expr, "")?;
                let ident_expr = self.alloc.arena.alloc(expr_info.take_expr());
                let field_sym = self.symbols.borrow_mut().simple_symbol(name.as_str());
             

                let pattern = Pattern::Record {
                    typ: expr_info.typ,
                    fields: vec![(TypedIdent::new2(field_sym.clone(), typ.typ()), None)],
                };
                let out_expr = VMExpr::Ident(
                    TypedIdent::new2(field_sym, typ.typ()),
                    source_span_to_byte_span(&ann.0),
                );

                let alt = Alternative {
                    pattern,
                    expr: self.alloc.arena.alloc(out_expr),
                };
                let alt_list = self
                    .alloc
                    .alternative_arena
                    .alloc_fixed(std::iter::once(alt));

                let match_expr = VMExpr::Match(ident_expr, alt_list);
                //dbg!(&match_expr);
                Ok(TExprInfo::new(match_expr, typ.typ(), &ann.0))
            }
            Expr::Case(ann, exprs, cases) => {
                let typ = self
                    .translate_type(ann.2.as_ref().unwrap(),None)
                    .map_err(|_| TranslateError::TypeError)?;
                let mut pred = self.translate_expr(&exprs[0], "")?;
                let pred_expr = self.alloc.arena.alloc(pred.take_expr());
                let mut alt_arr: Vec<Alternative> = vec![];
                for case_item in cases {
                    let binder = &case_item.binders[0];
                    let pattern: Pattern;
                    match binder {
                        Binder::LiteralBinder(_, lit) => {
                            pattern = self.translate_literal_binder(lit)?;
                        }
                        Binder::NullBinder(_) => {
                            pattern = Pattern::Ident(TypedIdent::new(self.simple_symbol("")));
                        },
                        Binder::ConstructorBinder(_,t,c,lst) => {
                            let type_name = t.join_name(|s| proper_name_as_str(s).to_string());
                            let data_type = self.type_env.type_dic.borrow().get(&type_name).unwrap().clone();
                            let cname = proper_name_as_str(&c.1);
                            let type_ident = TypedIdent::new2(self.simple_symbol(cname),data_type.gluon_type.clone());
                            let mut var_binds:Vec<TypedIdent> = vec![];
                            for var_bind in lst {
                                if let Binder::VarBinder(_,ident) = var_bind {
                                    //dbg!(ann);
                                    let t = self.type_cache.hole();//self.translate_type(ann.2.as_ref().unwrap(),None).unwrap().typ();
                                    let ident = TypedIdent::new2(self.simple_symbol(ident.as_str().unwrap()), t);
                                    var_binds.push(ident);
                                }
                            }
                            pattern = Pattern::Constructor(type_ident,var_binds);
                        },
                        b => {
                            dbg!(b);
                            todo!()
                        }
                    }
                    let res_expr = self
                        .translate_expr(case_item.result.as_ref().unwrap_err(), "")?
                        .take_expr();
                    let res_expr_ref = self.alloc.arena.alloc(res_expr);
                    let alt = Alternative {
                        pattern,
                        expr: res_expr_ref,
                    };
                    alt_arr.push(alt);
                }
                let alt_arr_ref = self.alloc.alternative_arena.alloc_fixed(alt_arr);
                let match_expr = VMExpr::Match(pred_expr, alt_arr_ref);
                Ok(TExprInfo::new(match_expr, typ.typ(), &ann.0))
            },
            Expr::Let(ann,bind,expr) => {
                let mut texpr = self.translate_expr(expr, "")?;
                let vm_expr = texpr.take_expr();
                let mut pre_expr = self.alloc.alloc(vm_expr);
                let mut len = 0;
                let mut first_expr = None;
                for bind_item in bind.iter().rev() {
                    let (let_binding,_) = self.translate_bind_item(&bind_item)?;
                    let let_expr = VMExpr::Let(self.alloc.let_binding_arena.alloc(let_binding),pre_expr);
                    if len == bind.len() - 1 {
                        first_expr = Some(let_expr);
                    } else {
                        pre_expr = self.alloc.arena.alloc(let_expr);
                        len+=1;
                    }
                    
                }
                
                Ok(TExprInfo::new(first_expr.unwrap(), texpr.typ, &ann.0)) 
            },
            expr => {
                dbg!(expr);
                todo!()
            }
        }
    }


    fn gen_closure_let(&self,ann:&Ann,expr_info:TExprInfo<'alloc>) -> VMExpr<'alloc> {
        let byte_span = source_span_to_byte_span(&ann.0);
        let named = Named::Recursive(expr_info.closure);
        let name = TypedIdent::new2(self.simple_symbol(""), expr_info.typ);
        let let_binding = LetBinding {
            name: name.clone(),
            expr: named,
            span_start: byte_span.start(),
        };
        let let_ref =  self.alloc.let_binding_arena.alloc(let_binding);
        let ident_ref = self.alloc.arena.alloc(VMExpr::Ident(name,byte_span));
        VMExpr::Let(let_ref,ident_ref)
    }

    fn translate_literal_binder(
        &self,
        lit: &Literal<Box<Binder<Ann>>>,
    ) -> Result<Pattern, TranslateError> {
        match lit {
            Literal::NumericLiteral(enumber) => match enumber {
                Ok(inum) => Ok(Pattern::Literal(VMLiteral::Int(*inum as i64))),
                Err(fnum) => {
                    let val = ordered_float::NotNan::new(*fnum)
                        .map_err(|_| TranslateError::TranslateNotNanFloat)?;
                    Ok(Pattern::Literal(VMLiteral::Float(val)))
                }
            },
            Literal::CharLiteral(chr) => Ok(Pattern::Literal(VMLiteral::Char(*chr))),
            Literal::StringLiteral(str) => {
                Ok(Pattern::Literal(VMLiteral::String(Box::from(&str[..]))))
            }
            Literal::BooleanLiteral(b) => {
                let bool_var = self.type_env.bool_constructor(*b);
                Ok(Pattern::Constructor(bool_var, vec![]))
            }
            _ => todo!(),
        }
    }

    pub fn translate_literal(
        &self,
        lit: &Literal<Box<Expr<Ann>>>,
        ann: &Ann,
        typ: ArcType,
    ) -> Result<VMExpr<'alloc>, TranslateError> {
        let byte_pos = source_span_to_byte_span(&ann.0);
        match lit {
            Literal::NumericLiteral(enumber) => match enumber {
                Ok(inum) => Ok(VMExpr::Const(VMLiteral::Int(*inum as i64), byte_pos)),
                Err(fnum) => {
                    let val = ordered_float::NotNan::new(*fnum)
                        .map_err(|_| TranslateError::TranslateNotNanFloat)?;
                    Ok(VMExpr::Const(VMLiteral::Float(val), byte_pos))
                }
            },
            Literal::StringLiteral(str) => Ok(VMExpr::Const(
                VMLiteral::String(Box::from(&str[..])),
                byte_pos,
            )),
            Literal::CharLiteral(chr) => Ok(VMExpr::Const(VMLiteral::Char(*chr), byte_pos)),
            Literal::ArrayLiteral(arr) => {
                let exprs = self.alloc.arena.alloc_fixed(
                    arr.iter()
                        .map(|v| self.translate_expr(v, "").unwrap().take_expr()),
                );
                Ok(VMExpr::Data(
                    TypedIdent {
                        typ: typ.clone(),
                        name: self.simple_symbol(""),
                    },
                    exprs,
                    byte_pos.start(),
                ))
            }
            Literal::ObjectLiteral(record_list) => {
                let vm_exprs = record_list.iter().map(|(_, v)| {
                    let mut expr_info = self.translate_expr(v, "").unwrap();
                    expr_info.take_expr()
                });
                let alloc_expr = self.alloc.arena.alloc_fixed(vm_exprs);
                let data = VMExpr::Data(
                    TypedIdent {
                        typ: typ.clone(),
                        name: self.simple_symbol(""),
                    },
                    alloc_expr,
                    byte_pos.start(),
                );
                Ok(data)
            }
            Literal::BooleanLiteral(b) => {
                let type_ident = self.type_env.bool_constructor(*b);
                let data = VMExpr::Data(type_ident, &[], byte_pos.start());
                Ok(data)
            }
        }
    }

    fn translate_exports(&self,exports: &Vec<Ident>,externs_file: &ExternsFile) -> Result<(VMExpr<'alloc>, ArcType), TranslateError> {
        let type_dic = externs_file.decl_type_dic();
        let mut field_types: Vec<Field<Symbol>> = vec![];
        let mut fields: Vec<VMExpr<'alloc>> = vec![];
        for id in exports {
            let id_name = self.id2str(id)?;
            let decl_type = *type_dic.get(id_name).ok_or(TranslateError::NotFindType)?;
            let typ = self
                .translate_type(decl_type,None)
                .map_err(|_| TranslateError::PartialType)?;
            let type_ident: TypedIdent = TypedIdent::new2(self.simple_symbol(id_name), typ.typ());
            let ident = VMExpr::Ident(type_ident, Span::new(ByteIndex(0), ByteIndex(0)));
            fields.push(ident);

            let sym = self.simple_symbol(id_name);
            let field_type = Field::new(sym, typ.typ());
            field_types.push(field_type);
        }
        let typ = self.type_cache.record(vec![], field_types);
        let alloc_expr = self.alloc.arena.alloc_fixed(fields);
        let rec = VMExpr::Data(
            TypedIdent {
                typ: typ.clone(),
                name: self.simple_symbol(""),
            },
            alloc_expr,
            ByteIndex(0),
        );
        Ok((rec, typ.clone()))
    }

    pub(crate) fn translate_type<T>(&self, typ: &AstType<T>,type_var_env:Option<&HashMap<String,ArcKind>>) -> Result<TTypeInfo, TransferType> {
        match &typ {
            AstType::TypeConstructor(_, qual_proper) => {
                let type_name = types::proper_name_as_str(&qual_proper.1);
                match &qual_proper.0 {
                    Some(qual_str) if qual_str == "Prim" => match type_name {
                        "Int" => Ok(TTypeInfo::new(self.type_cache.int())),
                        "Number" => Ok(TTypeInfo::new(self.type_cache.float())),
                        "String" => Ok(TTypeInfo::new(self.type_cache.string())),
                        "Char" => Ok(TTypeInfo::new(self.type_cache.char())),
                        "Array" => Ok(TTypeInfo::new(self.type_cache.array_builtin())),
                        "Record" => Err(TransferType::PartialRecord(type_name.to_string())),
                        "Function" => Err(TransferType::Function),
                        "Boolean" => {
                            let bool_type = self.type_env.get_bool_type().gluon_type.clone();
                            Ok(TTypeInfo::new(bool_type))
                        }
                        _ => Ok(TTypeInfo::new(Type::ident(KindedIdent::new(Symbol::from(
                            type_name,
                        ))))),
                    },
                    _ => {
                        let qual_type_name =
                            qual_proper.join_name(|q| proper_name_as_str(q).to_string());
                        //dbg!(&qual_type_name);
                        let var_len = self
                            .type_env
                            .type_dic
                            .borrow()
                            .get(&qual_type_name)
                            .unwrap()
                            .type_vars
                            .len();
                        let gluon_type = self
                            .type_env
                            .type_dic
                            .borrow()
                            .get(&qual_type_name)
                            .unwrap()
                            .gluon_type
                            .clone();
                        if var_len == 0 {
                            let alias =
                                Type::alias(self.simple_symbol(type_name), vec![], gluon_type);
                            Ok(TTypeInfo::new(alias))
                        } else {
                            Ok(TTypeInfo::new_env_type(qual_type_name))
                        }
                    }
                }
            }
            AstType::TypeApp(_, _, _) => {
                let app_list = self.flat_app_type(typ,type_var_env);
                //dbg!(&app_list);
                let mut idx = 0;
                let app_type = self.take_type_app(&app_list,&mut idx);
                let args = self.take_type_app_args(app_type.clone());
                Ok(TTypeInfo::new_func(app_type,args))
            }
            AstType::RCons(_, _, _, _) => {
                let mut cur_type: &AstType<T> = typ;
                let mut fields = vec![];
                loop {
                    match cur_type {
                        AstType::RCons(_, label, head, tail) => {
                            let field_type = self.translate_type(head,type_var_env).unwrap().typ();
                            let name_sym = self.simple_symbol(label.as_str());
                            let field = Field::new(name_sym, field_type);
                            fields.push(field);
                            cur_type = tail;
                        }
                        _ => break,
                    }
                }
                Ok(TTypeInfo::new(self.type_cache.record(vec![], fields)))
            }
            AstType::TypeVar(_, var) => {
                let kind = if let Some(dic) = type_var_env {
                    dic.get(var).unwrap().clone()
                } else {
                    Kind::typ()
                };
                let generic = Generic::new(self.simple_symbol(var.as_str()), kind);
                Ok(TTypeInfo::new(Type::generic( generic)))
            }
            AstType::ForAll(_, _, _, _, _) => self.flat_forall(typ,type_var_env),
            AstType::ConstrainedType(_, _constraint, e_type) => self.translate_type(e_type,type_var_env),
            AstType::KindApp(_,_,_) => {
                panic!("todo kindapp")
            },
            AstType::KindedType(_,_,_) => panic!("todo kindedtype"),
            AstType::Skolem(_,name,typ,_,_) => {
               let t = typ.as_ref().unwrap();
               let arc_kind = self.translate_kind_type(&**t);
               let generic = Generic::new(self.simple_symbol(name), arc_kind);
               Ok(TTypeInfo::new(Type::generic(generic))) 
            },
            AstType::TUnknown(_,_) => panic!("todo Unknown"),
            AstType::TypeLevelString(_,_) =>  panic!("todo TypeLevelString"),
            AstType::TypeWildcard(_,_) =>  panic!("todo TypeWildcard"),
            AstType::TypeOp(_,_) => panic!("todo TypeOp"),
            AstType::REmpty(_) => panic!("todo REmpty"),
            AstType::BinaryNoParensType(_,_,_,_) => panic!("todo BinaryNoParensType"),
            AstType::ParensInType(_,_) => panic!("todo ParensInType"),
        }
    }

    pub(crate) fn flat_app_type<T>(
        &self,
        typ: &AstType<T>,
        type_var_env:Option<&HashMap<String,ArcKind>>
    ) -> Vec<Result<TTypeInfo, TransferType>> {
        let mut cur_type = typ;
        let mut list: Vec<Result<TTypeInfo, TransferType>> = vec![];
        loop {
            match cur_type {
                AstType::TypeApp(_, head, tail) => {
                    list.extend(self.flat_app_type(head,type_var_env));
                    cur_type = tail;
                },
                _ => {
                    list.push(self.translate_type(cur_type,type_var_env));
                    break;
                }
            }
        }
        list
    }

    pub(crate) fn take_type_app(&self,app_list: &Vec<Result<TTypeInfo, TransferType>>,idx:&mut usize) -> ArcType {
        //Int -> Int -> Int => Function -> Int -> Function -> Int -> Int
        //(Int -> Int) -> Int => Function -> Function -> Int -> Int -> Int
        match &app_list[*idx] {
            Err(TransferType::Function) => {
                self.take_function(app_list,idx)
            },
            Err(TransferType::PartialRecord(_)) => {
                let record_idx = *idx + 1;
                *idx += 2;
                app_list[record_idx].as_ref().unwrap().typ()
            }
            Ok(ty) => {
                if let Some(env_type_name) = ty.env_type.as_ref() {
                    self.take_type_ctor(app_list, idx,env_type_name.as_str())
                } else {
                    match &**ty.typ.as_ref().unwrap() {
                        Type::Generic(g) => {
                            self.take_var_ctor(g,app_list, idx)
                        }
                        _ => {
                            *idx += 1;
                            ty.typ()
                        }
                    }
                    
                }
            },
        }
    }



    fn take_var_ctor(&self,generic:&Generic<Symbol>,app_list: &Vec<Result<TTypeInfo, TransferType>>,idx:&mut usize) -> ArcType {
        *idx += 1;
        let kind_array = to_kind_array(&generic.kind);
        if kind_array == KindArray::Type {
            return Type::generic(generic.clone())
        }
        // Function Function a  b Function  f a Function fc a b  f b
        if let KindArray::KindArray(arr) = kind_array {
            let mut kind_arr:Vec<ArcType> = vec![];
            //println!("{:?} idx:{}",generic,*idx);
            //println!("{:?}",arr);
            for _ in 0..arr.len() -1 {
                let arc_type = self.take_type_app(app_list, idx);
               
                //let arc_kind = self.get_arc_type_kind(&arc_type);
                kind_arr.push(arc_type);     
            }
            return Type::app(Type::generic(generic.clone()), kind_arr.into());
        }

        todo!()
    } 

    fn take_type_ctor(&self,app_list:&Vec<Result<TTypeInfo, TransferType>>,idx:&mut usize,type_name:&str) -> ArcType {
        *idx += 1;
        let env_type = self.type_env.type_dic.borrow().get(type_name).unwrap().clone();
        let type_sym_name = self.simple_symbol(env_type.qual_type_name.as_str());
        let alias = Type::alias(type_sym_name, env_type.type_vars.clone(), env_type.gluon_type.clone());
        let var_len = env_type.type_vars.len();       
        let mut args = vec![];
        for _ in 0..var_len {
            let arg_type = self.take_type_app(app_list, idx);
            args.push(arg_type);
        }
        let app = Type::app(alias, args.into());
        //dbg!(&app);
        app
    }

    pub(crate) fn take_function(&self,app_list:&Vec<Result<TTypeInfo, TransferType>>,idx:&mut usize) -> ArcType {
        *idx += 1;
        let arg = self.take_type_app(app_list, idx);
        let ret = self.take_type_app(app_list, idx);
        self.type_cache.function(Some(arg), ret)
    }
    
    fn take_type_app_args(&self,app_list:ArcType) -> Vec<ArcType> {
        //Int -> Int -> Int -> Int => App(Int,App(Int,App(Int,Int)))
        //(Int -> Int) -> Int => App(App(Int,Int),Int)
        //((Int -> Int) -> Int) -> Int => App(App(App(Int,Int),Int),Int)
        let mut cur_app_list = &app_list;
        let mut args:Vec<ArcType> = vec![];
        loop {
            match &**cur_app_list {
                Type::Function(_,arg,ret) => {
                    args.push(arg.clone());
                    cur_app_list = ret;
                }
                _ => {
                    args.push(cur_app_list.clone());
                    break;
                }
            }
        }
        args
    }


    pub(crate) fn flat_forall<T>(&self, typ: &AstType<T>,_:Option<&HashMap<String,ArcKind>>) -> Result<TTypeInfo, TransferType> {
        let dic = self.forall_kind_dic(typ) ;
        let mut names: Vec<Generic<Symbol>> = vec![];
        let mut cur_type = typ;
        loop {
            match cur_type {
                AstType::ForAll(_, name, _, next, _) => {
                    if self.cur_typeclass_args.contains(name) == false {
                        let kind = dic.get(name).unwrap();
                        let g = Generic::new(self.simple_symbol(name), kind.clone());
                        names.push(g);
                    }
                    cur_type = next;
                }
                t => {
                    cur_type = t;
                    break;
                }
            }
        }

        let vm_type = self.translate_type(cur_type,Some(dic).as_ref());
        let t = vm_type.unwrap().typ();
        let forall = Type::forall(names, t);
        Ok(TTypeInfo::new(forall)) 
    }

    fn gen_typeclass_member(
        &self,
        name: &str,
        typeclass_name: &str,
        ann: &Ann,
        _expr: &Expr<Ann>,
        type_var_env:Option<&HashMap<String,ArcKind>>
    ) -> Result<TExprInfo<'alloc>, TranslateError> {
        let span = source_span_to_byte_span(&ann.0);

        let func_name_sym = self.simple_symbol(name);
        let typeclass_info = self.type_env.type_dic.borrow().get(typeclass_name).unwrap().clone();
        let class_type = typeclass_info.gluon_type.clone();

        let ret_type = self.translate_type(ann.2.as_ref().unwrap(),type_var_env).unwrap().typ();
        let garr: Vec<ArcType> = typeclass_info
            .type_vars
            .clone()
            .drain(0..)
            .map(|g| Type::generic(g))
            .collect();
        let dict_sym = self.simple_symbol("dict");
        let type_app: ArcType = Type::app(class_type.clone(), garr.into());

        let func_type = self.type_cache.function(vec![type_app.clone()], ret_type);
        let forall_type = Type::forall(typeclass_info.type_vars.clone(), func_type);
        //match expr
        let pattern = Pattern::Record {
            typ: class_type.clone(),
            fields: vec![(
                TypedIdent::new2(func_name_sym.clone(), type_app.clone()),
                None,
            )],
        };
        let out_expr = VMExpr::Ident(
            TypedIdent::new2(func_name_sym.clone(), type_app.clone()),
            span,
        );
        let alt = Alternative {
            pattern,
            expr: self.alloc.arena.alloc(out_expr),
        };
        let alt_list = self
            .alloc
            .alternative_arena
            .alloc_fixed(std::iter::once(alt));
        let arg = VMExpr::Ident(TypedIdent::new2(dict_sym.clone(), type_app.clone()), span);
        let arg_ref = self.alloc.arena.alloc(arg);
        let match_expr = VMExpr::Match(arg_ref, alt_list);

        let closure = Closure {
            pos: span.start(),
            name: TypedIdent::new2(func_name_sym, forall_type.clone()),
            args: vec![TypedIdent::new2(dict_sym, type_app.clone())],
            expr: self.alloc.arena.alloc(match_expr),
        };

        let tinfo = TExprInfo::new_closure(forall_type, vec![closure], &ann.0);
        Ok(tinfo)
    }

    fn gen_type_class_instance(&self,body: &Expr<Ann>,arg: &Expr<Ann>,bind_name: &str,type_var_env:Option<&HashMap<String,ArcKind>>) -> Result<TExprInfo<'alloc>, TranslateError> {
        let mut cur_body = body;
        let info = self.translate_expr(arg, "")?;
        
        let mut teinfos: Vec<TExprInfo> = vec![info];
        let mut typeclass_name: String = String::default();
        let mut types: Vec<ArcType> = vec![];
        let mut var_ann = None;
        loop {
            match cur_body {
                Expr::App(ann, body, app_arg) => {
                    let mut gen_name = bind_name.to_string();
                    let span = source_span_to_byte_span(&ann.0);
                    gen_name.push_str(span.start().to_string().as_str());
                    let info = self.translate_expr(app_arg, gen_name.as_str())?;
                   
                    teinfos.push(info);
                    cur_body = body;
                }
                Expr::TypedVar(ann, qual, tys) => {
                    var_ann = Some(ann.clone());
                    typeclass_name = qual.join_name(|id| id.as_str().unwrap().to_string());
                    for ty in tys {
                        let tt = self.translate_type(ty, type_var_env).unwrap();
                        let tty = if let Some(env_name) = tt.env_type {
                           self.type_env.type_dic.borrow().get(&env_name).unwrap().gluon_type.clone()
                        } else {
                            tt.typ()
                        };
                        types.push(tty);
                    }
                    break;
                }
                _ => break,
            }
        }
        let typeclass_type = self.type_env.type_dic.borrow().get(typeclass_name.as_str()).unwrap().clone();
        let mut field_arr = vec![];
        let zero_span = Span::new(BytePos(0), BytePos(0));
        for mut info in teinfos.drain(0..) {
            if info.closure.len() == 0 {
                let expr = info.take_expr();
                let expr_ref = self.alloc.arena.alloc(expr);
                let named = Named::Expr( expr_ref);
                let name = TypedIdent {
                    name: self.simple_symbol(""),
                    typ: info.typ.clone(),
                };
                
                let let_bind = LetBinding {
                    name:name.clone(),
                    expr: named,
                    span_start: ByteIndex(info.start),
                };
                let let_bind_ref = self.alloc.let_binding_arena.alloc(let_bind);
                
                let field_id_expr = self.alloc.arena.alloc(VMExpr::Ident(name, zero_span));
                let let_expr = VMExpr::Let(let_bind_ref, field_id_expr);
                
                field_arr.push(let_expr);
            } else {
                let func_type = info.closure[0].name.typ.clone();
                let closure_name = info.closure[0].name.name.clone();
                let named = Named::Recursive(info.closure);
    
                let field_ident = TypedIdent::new2(closure_name, func_type);
                let let_bind = LetBinding {
                    name: field_ident.clone(),
                    expr: named,
                    span_start: ByteIndex(info.start),
                };
                let let_bind_ref = self.alloc.let_binding_arena.alloc(let_bind);
                
                let field_id_expr = self.alloc.arena.alloc(VMExpr::Ident(field_ident, zero_span));
                let let_expr = VMExpr::Let(let_bind_ref, field_id_expr);
                field_arr.push(let_expr);
            }
           
        }
        let app_type = Type::app(typeclass_type.gluon_type.clone(), types.into());
        //let bind_name_sym = self.simple_symbol(bind_name);
        //let bind_id = TypedIdent::new2(bind_name_sym, app_type.clone());

        let data_id = TypedIdent::new2(
            self.simple_symbol("<record>"),
            typeclass_type.gluon_type2.as_ref().unwrap().clone(),
        );
        let lets = self.alloc.arena.alloc_fixed(field_arr);
        let data_expr = VMExpr::Data(data_id, lets, ByteIndex(0));
        //dbg!(&data_expr);
        Ok(TExprInfo::new(
            data_expr,
            app_type,
            &var_ann.as_ref().unwrap().0,
        ))
    }

    pub fn id2str<'a>(&self, id: &'a Ident) -> Result<&'a str, TranslateError> {
        match id.as_str() {
            Some(s) => Ok(s),
            None => Err(TranslateError::IdentToString),
        }
    }

    fn replace_prim_name(func_name: &str) -> String {
        match func_name {
            "primcore'int_add" => "#Int+".into(),
            "primcore'int_sub" => "#Int-".into(),
            "primcore'int_eq"  => "#Int==".into(),
            "primcore'int_less"  => "#Int<".into(),
            _ => func_name.into(),
        }
    }

    pub(crate) fn simple_symbol(&self, name: &str) -> Symbol {
        
        let ret = self.symbols.borrow_mut().simple_symbol(name);
        if name == "unwrap" {
            println!("fire unwrap:{:?}",&ret);
        }
        ret
    }


   
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TransferType {
    PartialRecord(String),
    Function,
}
#[derive(Debug)]
pub(crate) struct TTypeInfo {
    pub typ: Option<ArcType>,
    pub args: Vec<ArcType>,
    pub env_type: Option<String>,
}

impl TTypeInfo {
    pub fn new(t: ArcType) -> Self {
        TTypeInfo {
            typ: Some(t),
            args: vec![],
            env_type: None,
        }
    }

    pub fn typ(&self) -> ArcType {
        self.typ.as_ref().map(|a| a.clone()).unwrap()
    }

    pub fn new_func(t: ArcType, args: Vec<ArcType>) -> Self {
        TTypeInfo {
            typ: Some(t),
            args,
            env_type: None,
        }
    }
    pub fn new_env_type(str: String) -> Self {
        TTypeInfo {
            typ: None,
            args: vec![],
            env_type: Some(str),
        }
    }
}

pub struct TExprInfo<'a> {
    pub typ: ArcType,
    pub expr: Option<VMExpr<'a>>,
    pub closure: Vec<Closure<'a>>,
    pub start: u32,
}

impl<'a> TExprInfo<'a> {
    pub fn new(expr: VMExpr<'a>, typ: ArcType, span: &SourceSpan) -> Self {
        let byte_span = source_span_to_byte_span(span);
        let start: u32 = byte_span.start().0;
        TExprInfo {
            expr: Some(expr),
            typ,
            closure: vec![],
            start,
        }
    }

    pub fn new_closure(typ: ArcType, closure: Vec<Closure<'a>>, span: &SourceSpan) -> Self {
        let byte_span = source_span_to_byte_span(span);
        let start: u32 = byte_span.start().0;
        TExprInfo {
            expr: None,
            typ,
            closure,
            start,
        }
    }

    pub fn expr(&self) -> &VMExpr<'a> {
        self.expr.as_ref().unwrap()
    }
    pub fn take_expr(&mut self) -> VMExpr<'a> {
        self.expr.take().unwrap()
    }
}

#[derive(Debug,PartialEq)]
enum KindArray {
    Type,
    KindArray(Vec<KindArray>)
}

fn to_kind_array(kind:&ArcKind) -> KindArray {
    let mut cur_kind:&Kind = &**kind;
    let mut array:Vec<KindArray> = vec![];
    loop {
        match cur_kind {
            Kind::Function(l,r) => {
                match &**l {
                    Kind::Type => {
                        array.push(KindArray::Type);
                    },
                    Kind::Function(_,_) => {
                        array.push(to_kind_array(l));
                    }
                    _ => panic!()
                }
                cur_kind = r;
            },
            Kind::Type => {
                array.push(KindArray::Type);
                break;
            }
            ,_ => panic!()
        }
    }
    if array.len() > 1 {
        return KindArray::KindArray(array)
    }
    if array.len() == 1 {
        match &array[0] {
            KindArray::Type => {
                return KindArray::Type
            },
            KindArray::KindArray(_) => {
                return array.remove(0)
            }
        }
    }
    panic!()
}