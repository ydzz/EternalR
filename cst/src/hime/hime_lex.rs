use crate::lex::{Lexer};
use crate::types::{SourceToken};
use super::TokenToHimeTerminal;
use hime_redist::lexers::{Lexer as ILexer,TokenKernel,ContextProvider};
use hime_redist::symbols::{Symbol};
use hime_redist::text::{Text};
use hime_redist::tokens::{TokenRepository};
use hime_redist::errors::ParseErrors;

pub struct HimeLex<'a> {
   index:u32,
   tokens:&'a Vec<SourceToken>
}

impl<'a> HimeLex<'a> {
    pub fn new(tokens:&'a Vec<SourceToken>) -> HimeLex<'a> {
        HimeLex {
            index:0,
            tokens
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
        if self.index >= self.tokens.len() as u32 {
            return None;
        }
        
        let cur_token = &self.tokens[self.index as usize];
        println!("next token: {}",cur_token.value);
        let tindex = cur_token.value.to_terminals_index();
        self.index += 1;
        Some(TokenKernel {
            terminal_id: tindex as u32,
            index: self.index - 1
        })
    }
}