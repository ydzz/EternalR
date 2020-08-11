use hime_redist::text::{Text};
use hime_redist::parsers::{Parser,lrk::{self,LRkParser,LRkAutomaton},rnglr::{self,RNGLRParser,RNGLRAutomaton}};
use hime_redist::ast::{AstNode,AstCell};
use hime_redist::result::ParseResult;
use hime_redist::symbols::{Symbol,SemanticBody,SemanticElementTrait};
use crate::hime::consts::{TERMINALS,VARIABLES,VIRTUALS};
use crate::hime::hime_lex::{HimeLex};
use crate::lex::{Lexer};
use crate::types::{SourceToken};
const PARSER_AUTOMATON: &[u8] = include_bytes!("hime/EternaleRParser.bin");
#[test]
fn test_parser() {
    let code_string = String::from("case ma of \n Just a -> 1 \n Nothing -> 2");//std::fs::read_to_string("tests/main.purs").unwrap();
    let text = Text::new(code_string.as_str());
    let mut mylex = Lexer::new(code_string.as_str());
    let raw_tokens = mylex.lex();
    let mut tokens = vec![];
    for rt in raw_tokens {
        match rt {
            Ok(t) => tokens.push(t),
            Err(_) => ()
        }
    }
    println!("all tokens: {:?}",&tokens);
    let mut lexer = HimeLex::new(&tokens);
    let mut my_actions = |_index: usize, _head: Symbol, _body: &dyn SemanticBody| ();
    let mut result = ParseResult::new(TERMINALS, VARIABLES, VIRTUALS, text);
   
    {
        let data = result.get_parsing_data();
        let automaton = RNGLRAutomaton::new(PARSER_AUTOMATON);
        let mut parser = RNGLRParser::new(&mut lexer, automaton, data.2, &mut my_actions);
        parser.parse();

        let ast = result.get_ast();
        print(ast.get_root(),vec![],&tokens);
    }
    
   
   
}


fn print<'a>(node: AstNode<'a>, crossings: Vec<bool>,tokens:&Vec<SourceToken>) {
    let mut i = 0;
    if !crossings.is_empty() {
        while i < crossings.len() - 1 {
            print!("{:}", if crossings[i] { "|   " } else { "    " });
            i += 1;
        }
        print!("+-> ");
    }
    if let Some(token_index) = node.get_token_index() {
        let tok = &tokens[token_index];
        println!("{:}", tok.value);
    } else {
        println!("{:}", node);
    }
    i = 0;
    let children = node.children();
    while i < children.len() {
        let mut child_crossings = crossings.clone();
        child_crossings.push(i < children.len() - 1);
        print(children.at(i), child_crossings,tokens);
        i += 1;
    }
}