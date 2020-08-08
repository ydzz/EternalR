use crate::lex::{Lexer};
use hime_redist::lexers::{Lexer as ILexer,TokenKernel,ContextProvider};
use hime_redist::symbols::{Symbol};
use hime_redist::text::{Text};
use hime_redist::tokens::{TokenRepository};
use hime_redist::errors::ParseErrors;

pub struct HimeLex<'a> {
   lex: Lexer<'a>,
   index:u32,
   is_run:bool
}

impl<'a> HimeLex<'a> {
    pub fn new(source:&'a str) -> HimeLex {
        HimeLex {
            lex:Lexer::new(source),
            index:0,
            is_run:false
        }
    }
}

impl<'a> ILexer<'a> for HimeLex<'a> {
    fn get_terminals(&self) -> &'static [Symbol] {
        todo!()
    }

   
    fn get_input(&self) -> &Text {
        todo!()
    }

   
    fn get_output(&self) -> &TokenRepository<'a> {
        todo!()
    }

    
    fn get_errors(&mut self) -> &mut ParseErrors {
        todo!();
    }

   
    fn get_recovery_distance(&self) -> usize {
        todo!()
    }

   
    fn set_recovery_distance(&mut self, _distance: usize) {
        todo!()
    }

    
    fn get_next_token(&mut self, _contexts: &dyn ContextProvider) -> Option<TokenKernel> {
        if !self.is_run {
            self.lex.lex();
            self.is_run = true;
        }
        self.index += 1;
        None
    }
}