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

//! Module for the definition of lexical and syntactic errors

use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::ops::Index;

use crate::symbols::Symbol;
use crate::text::TextPosition;
use crate::text::Utf16C;
use crate::utils::iterable::Iterable;

/// Common trait for data about an error
pub trait ParseErrorDataTrait {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition;

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize;

    /// Gets the error's message
    fn get_message(&self) -> String;
}

/// Represents the unexpected of the input text while more characters were expected
#[derive(Copy, Clone)]
pub struct ParseErrorEndOfInput {
    /// The error's position in the input text
    position: TextPosition
}

impl ParseErrorDataTrait for ParseErrorEndOfInput {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition {
        self.position
    }

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize {
        0
    }

    /// Gets the error's message
    fn get_message(&self) -> String {
        String::from("Unexpected end of input")
    }
}

impl ParseErrorEndOfInput {
    /// Creates a new error
    pub fn new(position: TextPosition) -> ParseErrorEndOfInput {
        ParseErrorEndOfInput { position }
    }
}

/// Represents an unexpected character error in the input stream of a lexer
#[derive(Copy, Clone)]
pub struct ParseErrorUnexpectedChar {
    /// The error's position in the input text
    position: TextPosition,
    /// The unexpected character
    unexpected: [Utf16C; 2]
}

impl ParseErrorDataTrait for ParseErrorUnexpectedChar {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition {
        self.position
    }

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize {
        if self.unexpected[1] == 0x00 {
            1
        } else {
            2
        }
    }

    /// Gets the error's message
    fn get_message(&self) -> String {
        let mut result = String::new();
        result.push_str("Unexpected character '");
        if self.unexpected[1] == 0x00 {
            result.push_str(&String::from_utf16(&self.unexpected[0..1]).unwrap());
            result.push_str("' (U+");
            result.push_str(&format!("{:X}", self.unexpected[0]));
        } else {
            let lead = u32::from(self.unexpected[0]);
            let trail = u32::from(self.unexpected[1]);
            let cp = ((trail - 0xDC00) | ((lead - 0xD800) << 10)) + 0x10000;
            result.push_str(&String::from_utf16(&self.unexpected).unwrap());
            result.push_str("' (U+");
            result.push_str(&format!("{:X}", cp));
        }
        result.push_str(")");
        result
    }
}

impl ParseErrorUnexpectedChar {
    /// Creates a new error
    pub fn new(position: TextPosition, unexpected: [Utf16C; 2]) -> ParseErrorUnexpectedChar {
        ParseErrorUnexpectedChar {
            position,
            unexpected
        }
    }
}

/// Represents an incorrect encoding sequence error in the input of a lexer
#[derive(Copy, Clone)]
pub struct ParseErrorIncorrectEncodingSequence {
    /// The error's position in the input text
    position: TextPosition,
    /// The precise error type
    missing_high: bool,
    /// The incorrect sequence
    sequence: Utf16C
}

impl ParseErrorDataTrait for ParseErrorIncorrectEncodingSequence {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition {
        self.position
    }

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize {
        1
    }

    /// Gets the error's message
    fn get_message(&self) -> String {
        let mut result = String::new();
        result.push_str("Incorrect encoding sequence: [");
        if self.missing_high {
            result.push_str("<missing> ");
            result.push_str("0x");
            result.push_str(&format!("{:X}", self.sequence));
        } else {
            result.push_str("0x");
            result.push_str(&format!("{:X}", self.sequence));
            result.push_str(" <missing>");
        }
        result.push_str("]");
        result
    }
}

impl ParseErrorIncorrectEncodingSequence {
    /// Initializes this error
    pub fn new(
        position: TextPosition,
        missing_high: bool,
        sequence: Utf16C
    ) -> ParseErrorIncorrectEncodingSequence {
        ParseErrorIncorrectEncodingSequence {
            position,
            missing_high,
            sequence
        }
    }
}

/// Represents an unexpected token error in a parser
#[derive(Clone)]
pub struct ParseErrorUnexpectedToken {
    /// The error's position in the input text
    position: TextPosition,
    /// The error's length in the input
    length: usize,
    /// The value of the unexpected token
    value: String,
    /// The terminal symbol for the unexpected token
    terminal: Symbol,
    /// The expected terminals
    expected: Vec<Symbol>
}

impl ParseErrorDataTrait for ParseErrorUnexpectedToken {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition {
        self.position
    }

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize {
        self.length
    }

    /// Gets the error's message
    fn get_message(&self) -> String {
        let mut result = String::new();
        result.push_str("Unexpected token \"");
        result.push_str(&self.value);
        result.push_str("\"");
        if !self.expected.is_empty() {
            result.push_str("; expected: ");
            for (i, x) in self.expected.iter().enumerate() {
                if i != 0 {
                    result.push_str(", ");
                }
                result.push_str(x.name);
            }
        }
        result
    }
}

impl ParseErrorUnexpectedToken {
    /// Initializes this error
    pub fn new(
        position: TextPosition,
        length: usize,
        value: String,
        terminal: Symbol,
        expected: Vec<Symbol>
    ) -> ParseErrorUnexpectedToken {
        ParseErrorUnexpectedToken {
            position,
            length,
            value,
            terminal,
            expected
        }
    }
}

/// Represents a lexical or syntactic error
#[derive(Clone)]
pub enum ParseError {
    /// Lexical error occurring when the end of input has been encountered while more characters were expected
    UnexpectedEndOfInput(ParseErrorEndOfInput),
    /// Lexical error occurring when an unexpected character is encountered in the input preventing to match tokens
    UnexpectedChar(ParseErrorUnexpectedChar),
    /// Syntactic error occurring when an unexpected token is encountered by the parser
    UnexpectedToken(ParseErrorUnexpectedToken),
    /// Lexical error occurring when the low surrogate encoding point is missing in a UTF-16 encoding sequence with an expected high and low surrogate pair
    IncorrectUTF16NoLowSurrogate(ParseErrorIncorrectEncodingSequence),
    /// Lexical error occurring when the high surrogate encoding point is missing in a UTF-16 encoding sequence with an expected high and low surrogate pair
    IncorrectUTF16NoHighSurrogate(ParseErrorIncorrectEncodingSequence)
}

impl ParseErrorDataTrait for ParseError {
    /// Gets the error's position in the input
    fn get_position(&self) -> TextPosition {
        match *self {
            ParseError::UnexpectedEndOfInput(ref x) => x.get_position(),
            ParseError::UnexpectedChar(ref x) => x.get_position(),
            ParseError::UnexpectedToken(ref x) => x.get_position(),
            ParseError::IncorrectUTF16NoLowSurrogate(ref x) => x.get_position(),
            ParseError::IncorrectUTF16NoHighSurrogate(ref x) => x.get_position()
        }
    }

    /// Gets the error's length in the input (in number of characters)
    fn get_length(&self) -> usize {
        match *self {
            ParseError::UnexpectedEndOfInput(ref x) => x.get_length(),
            ParseError::UnexpectedChar(ref x) => x.get_length(),
            ParseError::UnexpectedToken(ref x) => x.get_length(),
            ParseError::IncorrectUTF16NoLowSurrogate(ref x) => x.get_length(),
            ParseError::IncorrectUTF16NoHighSurrogate(ref x) => x.get_length()
        }
    }

    /// Gets the error's message
    fn get_message(&self) -> String {
        match *self {
            ParseError::UnexpectedEndOfInput(ref x) => x.get_message(),
            ParseError::UnexpectedChar(ref x) => x.get_message(),
            ParseError::UnexpectedToken(ref x) => x.get_message(),
            ParseError::IncorrectUTF16NoLowSurrogate(ref x) => x.get_message(),
            ParseError::IncorrectUTF16NoHighSurrogate(ref x) => x.get_message()
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "@{} {}", self.get_position(), self.get_message())
    }
}

/// Represents an entity that can handle lexical and syntactic errors
#[derive(Default)]
pub struct ParseErrors {
    /// The overall errors
    errors: Vec<ParseError>
}

impl ParseErrors {
    /// Creates a new instance of the Errors structure
    pub fn new() -> ParseErrors {
        ParseErrors {
            errors: Vec::<ParseError>::new()
        }
    }

    /// Handles the end-of-input error
    pub fn push_error_eoi(&mut self, error: ParseErrorEndOfInput) {
        self.errors.push(ParseError::UnexpectedEndOfInput(error));
    }

    /// Handles the unexpected character error
    pub fn push_error_unexpected_char(&mut self, error: ParseErrorUnexpectedChar) {
        self.errors.push(ParseError::UnexpectedChar(error));
    }

    /// Handles the unexpected token error
    pub fn push_error_unexpected_token(&mut self, error: ParseErrorUnexpectedToken) {
        self.errors.push(ParseError::UnexpectedToken(error));
    }

    /// Handles the incorrect encoding sequence error
    pub fn push_error_no_low_utf16_surrogate(
        &mut self,
        error: ParseErrorIncorrectEncodingSequence
    ) {
        self.errors
            .push(ParseError::IncorrectUTF16NoLowSurrogate(error));
    }

    /// Handles the incorrect encoding sequence error
    pub fn push_error_no_high_utf16_surrogate(
        &mut self,
        error: ParseErrorIncorrectEncodingSequence
    ) {
        self.errors
            .push(ParseError::IncorrectUTF16NoHighSurrogate(error));
    }

    /// Gets the number of errors
    pub fn get_count(&self) -> usize {
        self.errors.len()
    }
}

/// Implementation of the indexer operator for immutable `ParseErrors`
impl<'a> Index<usize> for ParseErrors {
    type Output = ParseError;
    fn index(&self, index: usize) -> &Self::Output {
        &self.errors[index]
    }
}

/// Represents an iterator over parse errors
pub struct ParseErrorsIterator<'a> {
    /// The parent parse errors
    parent: &'a ParseErrors,
    /// The current index
    index: usize
}

/// Implementation of the `Iterator` trait for `ParseErrorsIterator`
impl<'a> Iterator for ParseErrorsIterator<'a> {
    type Item = &'a ParseError;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.parent.errors.len() {
            None
        } else {
            let result = &self.parent[self.index];
            self.index += 1;
            Some(result)
        }
    }
}

/// Implementation of `Iterable` for `ParseErrors`
impl<'a> Iterable<'a> for ParseErrors {
    type Item = &'a ParseError;
    type IteratorType = ParseErrorsIterator<'a>;
    fn iter(&'a self) -> Self::IteratorType {
        ParseErrorsIterator {
            parent: &self,
            index: 0
        }
    }
}
