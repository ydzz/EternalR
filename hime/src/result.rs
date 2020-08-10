/*******************************************************************************
 * Copyright (c) 2017 Association Cénotélie (cenotelie.fr)
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

//! Module for the definition of a parse result

use super::ast::Ast;
use super::ast::AstImpl;
use super::errors::ParseErrors;
use super::symbols::Symbol;
use super::text::Text;
use super::tokens::TokenRepository;
use super::tokens::TokenRepositoryImpl;

/// Represents the output of a parser
pub struct ParseResult {
    /// The table of grammar terminals
    terminals: &'static [Symbol],
    /// The table of grammar variables
    variables: &'static [Symbol],
    /// The table of grammar virtuals
    virtuals: &'static [Symbol],
    /// The input text
    text: Text,
    /// The errors found in the input
    errors: ParseErrors,
    /// The table of matched tokens
    tokens: TokenRepositoryImpl,
    /// The produced AST
    ast: AstImpl
}

impl<'a> ParseResult {
    /// Initialize a new parse result
    pub fn new(
        terminals: &'static [Symbol],
        variables: &'static [Symbol],
        virtuals: &'static [Symbol],
        text: Text
    ) -> ParseResult {
        ParseResult {
            terminals,
            variables,
            virtuals,
            text,
            errors: ParseErrors::new(),
            tokens: TokenRepositoryImpl::new(),
            ast: AstImpl::new()
        }
    }

    /// Gets the grammar terminals
    pub fn get_terminals(&self) -> &'static [Symbol] {
        self.terminals
    }

    /// Gets the grammar variables
    pub fn get_variables(&self) -> &'static [Symbol] {
        self.variables
    }

    /// Gets the grammar virtuals
    pub fn get_virtuals(&self) -> &'static [Symbol] {
        self.virtuals
    }

    /// Gets the input text for this result
    //pub fn get_input(&self) -> &Text {
    //    &self.text
    //}

    /// Gets whether this result denotes a successful parsing
    pub fn is_success(&self) -> bool {
        self.ast.has_root()
    }

    /// Gets the collection of errors
    pub fn get_errors(&self) -> &ParseErrors {
        &self.errors
    }

    /// Gets the token repository associated with this result
    pub fn get_tokens(&self) -> TokenRepository {
        TokenRepository::new(&self.terminals, &self.text, &self.tokens)
    }

    /// Gets the resulting AST
    pub fn get_ast(&self) -> Ast {
        Ast::new(
            TokenRepository::new(&self.terminals, &self.text, &self.tokens),
            self.variables,
            self.virtuals,
            &self.ast
        )
    }

    /// Gets the mutable data required for parsing
    pub fn get_parsing_data(&mut self) -> (TokenRepository, &mut ParseErrors, Ast) {
        (
            TokenRepository::new_mut(&self.terminals, &self.text, &mut self.tokens),
            &mut self.errors,
            Ast::new_mut(self.variables, self.virtuals, &mut self.ast)
        )
    }
}
