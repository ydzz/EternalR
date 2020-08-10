pub mod hime_lex;
pub mod consts;

pub trait TokenToHimeTerminal {
    fn to_terminals_index(&self) -> usize;
}