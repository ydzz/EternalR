use hime_redist::text::{Text};
use hime_redist::result::ParseResult;
use hime_redist::symbols::{Symbol,SemanticBody};
use crate::hime::consts::{TERMINALS,VARIABLES,VIRTUALS};


fn parse_text(text: Text) -> ParseResult {
    let mut my_actions = |_index: usize, _head: Symbol, _body: &dyn SemanticBody| ();
    let mut result = ParseResult::new(TERMINALS, VARIABLES, VIRTUALS, text);
    {
        //let data = result.get_parsing_data();
        //let mut lexer = new_lexer(data.0, data.1);
        //let automaton = LRkAutomaton::new(PARSER_AUTOMATON);
        //let mut parser = LRkParser::new(&mut lexer, automaton, data.2, &mut my_actions);
        //parser.parse();
    }
    result
}