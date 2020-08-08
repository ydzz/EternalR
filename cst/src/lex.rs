use crate::types::{SourcePos,SourceToken,LineFeed,Comment,Token,SourceStyle,TokenAnn,SourceRange};
use crate::errors::{ParserError,ParserErrorType,ParserErrorInfo};
use super::positions::{advance_leading,advance_token,advance_trailing};
use std::collections::HashSet;
use crate::lex_string::LexString;
use std::iter::FromIterator;
use unicode_general_category::{get_general_category,GeneralCategory};
use crate::layout::{LayoutStack,LayoutDelim,unwind_layout,insert_layout};

pub type LexResult = Result<(LexState,ParserError),SourceToken>;

lazy_static! {
 static ref SYMBOL_SET:HashSet<char> = [':','!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~'].iter().cloned().collect();
 static ref RESERVED_SYMBOL_SET:HashSet<&'static str> = ["::" , "∷" , "<-" , "←" , "->" , "→" , "=>" , "⇒" , "∀" , "|" , "." , "\\" , "="].iter().cloned().collect();
}

#[derive(Debug)]
pub struct LexState {
    pos:SourcePos,
    leading:Vec<Comment>,
    //source:String,
    stack:LayoutStack
}


pub struct Lexer<'a> {
    string:LexString<'a>
}

impl<'a> Lexer<'a> where {
   pub fn new( source:&'a str) -> Self {
      Lexer {
          string:LexString::new(source)
      }
   }
  
   pub fn lex(&mut self) -> Vec<Result<SourceToken,ParserError>> {
     let leading = self.comments();
     let  start_pos = advance_leading(& SourcePos::new(1,1),&leading);
     let mut state = LexState {
        pos:start_pos,
        leading,
        stack:vec![(SourcePos::new(0, 0),LayoutDelim::LytRoot)]
     };
     let mut ret_tokens:Vec<Result<SourceToken,ParserError>> = vec![];
     
     loop {
        let tok_and_cm = self.token_and_comments();
        match tok_and_cm {
           Ok((tok,(trailing,lexleading))) => {
              if tok != Token::TokEof {
                  let end_pos =  advance_token(&state.pos,&tok);
                  let lexpos =  advance_leading (&advance_trailing(&end_pos, &trailing),&lexleading);
                  let ann = TokenAnn {
                     range:SourceRange::new(state.pos.clone(),end_pos),
                     leading_comments:state.leading.clone(),
                     trailing_comments:trailing
                  };
                  let source_token = SourceToken::new(ann, tok);
                  let tokens = insert_layout(&source_token, &lexpos, &mut state.stack);
                  for tok in tokens {
                     ret_tokens.push(Ok(tok));
                  }
                  state.pos = lexpos;
                  state.leading = lexleading
              } else {
                  let tokens = unwind_layout(&state.pos,&state.leading,&mut state.stack);
                  for tok in tokens {
                     ret_tokens.push(Ok(tok));
                  }
                  break;
              }
           },
           Err(err) => {
             
             let mut end_pos = state.pos.clone();
             end_pos.column += 1;
             let ret_err = ParserErrorInfo {
                  range:SourceRange::new(state.pos.clone(),end_pos),
                  toks:vec![],
                  typ:err
             };
             ret_tokens.push(Err(ret_err));
             break;
           }
        }
     }

     ret_tokens
   }

   pub fn next(&mut self) -> Option<char> {
      self.string.next()
   }
   
   pub fn token_and_comments(&mut self) -> Result<(Token,(Vec<Comment>,Vec<Comment>)),ParserErrorType> {
      let tok = self.token()?;
      let comments = self.break_comments();
      return Ok((tok,comments));
   }

   pub fn comments(&mut self) -> Vec<Comment> {
     let (mut alist,blist) = self.break_comments();
     alist.extend(blist);
     return alist;
   }

   pub fn token(&mut self) -> Result<Token,ParserErrorType> {
      if let Some(chr) = self.string.lookahead(1) {
        let tok = match chr {
            '(' =>  { self.next(); self.left_paren() },
            ')' =>  { self.next(); Ok(Token::TokRightParen) },
            '{' =>  { self.next(); Ok(Token::TokLeftBrace)  },
            '}' =>  { self.next(); Ok(Token::TokRightBrace) },
            '[' =>  { self.next(); Ok(Token::TokLeftSquare)},
            ']' =>  { self.next(); Ok(Token::TokRightSquare)},
            '`' =>  { self.next(); Ok(Token::TokTick)},
            ',' =>  { self.next(); Ok(Token::TokComma)},
            '∷' => { self.next(); self.or_operator1(Token::TokDoubleColon(SourceStyle::Unicode) , chr) },
            '←' => { self.next(); self.or_operator1(Token::TokLeftArrow(SourceStyle::Unicode) , chr) },
            '→' => { self.next(); self.or_operator1(Token::TokRightArrow(SourceStyle::Unicode) , chr) },
            '⇒' => { self.next(); self.or_operator1(Token::TokRightFatArrow(SourceStyle::Unicode) , chr) },
            '∀' => { self.next(); self.or_operator1(Token::TokForallU , chr) },
            '|' => { self.next(); self.or_operator1(Token::TokPipe , chr) },
            '.' => { self.next(); self.or_operator1(Token::TokDot , chr) },
            '\\' => { self.next(); self.or_operator1(Token::TokBackslash , chr) },
            '<'  => {  self.next(); self.or_operator2(Token::TokLeftArrow(SourceStyle::ASCII), chr, '-') },
            '-'  => {  self.next(); self.or_operator2(Token::TokRightArrow(SourceStyle::ASCII), chr, '>') },
            '='  => {  self.next(); self.or_operator2_(Token::TokEquals,Token::TokRightFatArrow(SourceStyle::ASCII), chr, '>') },
            ':'  => {  self.next(); self.or_operator2_(Token::TokOperator(String::from(":")),Token::TokDoubleColon(SourceStyle::ASCII), chr, ':') },
            '?' => { self.next(); self.hole() },
            '\'' => { self.next(); self.lchar() },
            '\"' => {self.next();self.lstring() },
            chr => {
               if chr.is_ascii_digit() {
                  self.next();
                  return self.number(chr);
               }
               if chr.is_uppercase() {
                  self.next();
                  return self.upper(vec![],chr);
               }
               if Lexer::is_ident_start(chr) {
                  self.next();
                  return self.lower(vec![],chr);
               }
               if Lexer::is_symbol_char(chr) {
                  self.next();
                  return self.operator(vec![], vec![chr]);
               }
               let mut str =String::default();
               str.push(chr);
               return Err(ParserErrorType::ErrLexeme(Some(str),vec![]));   
            }
         };
         return tok;
      } else {
         return Ok(Token::TokEof);
      }
   }

   fn lower(&mut self, qual:Vec<String>,pre:char) -> Result<Token,ParserErrorType> {
      let mut rest = self.string.take_while(Lexer::is_ident_char).map(|s|s.to_owned()).unwrap_or_default();
      match pre {
         '_' if rest.len() == 0 => {
            if qual.len() == 0 {
               return Ok(Token::TokUnderscore);
            } else {
               let mut str =String::default();
               str.push(pre);
               return Err(ParserErrorType::ErrLexeme(Some(str),vec![]));
            }
         },
         _ => {
            rest.insert(0, pre);
            if rest == "forall" && qual.len() == 0 {
               return Ok(Token::TokForall);
            } else {
               match rest.as_str() {
                  "ado" => Ok(Token::TokAdo),
                  "as" if qual.len() == 0 => Ok(Token::TokAs), 
                  "case" if qual.len() == 0 => Ok(Token::TokCase), 
                  "class" if qual.len() == 0 => Ok(Token::TokClass), 
                  "data" if qual.len() == 0 => Ok(Token::TokData), 
                  "derive" if qual.len() == 0 => Ok(Token::TokDerive), 
                  "do" => Ok(Token::TokDo), 
                  "else" if qual.len() == 0 => Ok(Token::TokElse), 
                  "false" if qual.len() == 0 => Ok(Token::TokFalse), 
                  "foreign" if qual.len() == 0 => Ok(Token::TokForeign), 
                  "hiding" if qual.len() == 0 => Ok(Token::TokHiding),
                  "import" if qual.len() == 0 => Ok(Token::TokImport), 
                  "if" if qual.len() == 0 => Ok(Token::TokIf), 
                  "in" if qual.len() == 0 => Ok(Token::TokIn),  
                  "infix" if qual.len() == 0 => Ok(Token::TokInfix), 
                  "infixl" if qual.len() == 0 => Ok(Token::TokInfixl),
                  "infixr" if qual.len() == 0 => Ok(Token::TokInfixr),
                  "instance" if qual.len() == 0 => Ok(Token::TokInstance),
                  "kind" if qual.len() == 0 => Ok(Token::TokKind),  
                  "let" if qual.len() == 0 => Ok(Token::TokLet),
                  "module" if qual.len() == 0 => Ok(Token::TokModule), 
                  "newtype" if qual.len() == 0 => Ok(Token::TokNewtype),  
                  "nominal" if qual.len() == 0 => Ok(Token::TokNominal), 
                  "phantom" if qual.len() == 0 => Ok(Token::TokPhantom), 
                  "of" if qual.len() == 0 => Ok(Token::TokOf),
                  "representational" if qual.len() == 0 => Ok(Token::TokRepresentational),
                  "role" if qual.len() == 0 => Ok(Token::TokRole),  
                  "then" if qual.len() == 0 => Ok(Token::TokThen),
                  "true" if qual.len() == 0 => Ok(Token::TokTrue),
                  "type" if qual.len() == 0 => Ok(Token::TokType),
                  "where" if qual.len() == 0 => Ok(Token::TokWhere),
                  _ => {
                     if qual.len() == 0 {
                        Ok(Token::TokLowerName(rest))
                     } else {
                        Ok(Token::TokQualLowerName(qual,rest))
                     }
                  } 
               }
            }
         }
      }
   }

   fn upper(&mut self,mut qual:Vec<String>,pre:char) -> Result<Token,ParserErrorType> {
      let mut rest = self.string.take_while(Lexer::is_ident_char).map(|s|s.to_owned()).unwrap_or_default();
      let mb_ch1 = self.string.lookahead(1);
      rest.insert(0, pre);
      match mb_ch1 {
         Some('.') => {
            qual.push(rest.clone());
            self.next();
            match self.string.lookahead(1) {
               Some('(') => {
                  self.next();
                  return self.symbol(qual);
               },
               Some(ch) => {
                  if ch.is_uppercase() {
                     self.next();
                     return  self.upper( qual, ch);
                  } else if Lexer::is_ident_start(ch)  {
                     self.next();
                     return self.lower(qual, ch);
                  } else if Lexer::is_symbol_char(ch) {
                     self.next();
                     return self.operator(qual, vec![ch]);
                  } else {
                     let mut str =String::default();
                     str.push(ch);
                     return Err(ParserErrorType::ErrLexeme(Some(str) ,vec![]))
                  }
               },
               None => return Err(ParserErrorType::ErrEof)
            }
         },
         _ => {
            if qual.len() == 0 {
                Ok(Token::TokUpperName(rest))
            } else {
                Ok(Token::TokQualUpperName(qual,rest))
            }
         } 
      }
   }   

   fn symbol(&mut self,qual:Vec<String> ) -> Result<Token,ParserErrorType> {
      let mb_ch = self.string.lookahead(1);
      match mb_ch {
         Some(ch) if Lexer::is_symbol_char(ch) => {
            let syms = self.string.take_while(Lexer::is_symbol_char).map(|s|s.to_owned()).unwrap_or_default();
            let mb_ch1 = self.string.lookahead(1);
            match mb_ch1 {
               Some(')') => {
                  if Lexer::is_reserved_symbol(syms.as_str()) {
                     return Err(ParserErrorType::ErrReservedSymbol);
                  } else {
                     self.next();
                     if qual.len() == 0 {
                        if syms.as_str() == ".." {
                           return Ok(Token::TokSymbolDoubleDot);
                        } else {
                           return Ok(Token::TokSymbolName(syms));
                        }
                     } else {
                        return Ok(Token::TokQualSymbolName(qual,syms));
                     }
                  }
               },
               Some(ch1) => {
                  let mut str =String::default();
                  str.push(ch1);
                  return Err(ParserErrorType::ErrLexeme(Some(str) ,vec![]))
               } ,
               None => return Err(ParserErrorType::ErrEof)
            }
         },
         Some(ch) => {
            let mut str =String::default();
            str.push(ch);
            return Err(ParserErrorType::ErrLexeme(Some(str) ,vec![]))
         } ,
         None => return Err(ParserErrorType::ErrEof)
      }
   }

   fn number(&mut self,ch1:char) -> Result<Token,ParserErrorType> {
      let ch2 = self.string.lookahead(1);
      match (ch1,ch2) {
        ('0', Some('x')) => {
           self.next();
           let  hex = self.string.take_while(|chr| chr.is_ascii_hexdigit()).unwrap_or_default().to_owned();
           if hex.len() == 0 {
              return Err(ParserErrorType::ErrExpectedHex);
           }
           let mut raw = String::from("0x");
           raw.push_str(hex.as_str());
           let n = Lexer::digits_to_integer_base(16, &hex);
           return Ok(Token::TokInt(raw,n));
        },
         _ => {
            let mb_int1 = self.integer1(ch1)?;
            let mb_fraction = self.fraction()?;
            match (mb_int1,mb_fraction) {
               (Some((raw,sint)),None) => {
                 let int = Lexer::digits_to_integer(sint.as_str());
                 let e = self.exponent()?;
                 match e {
                    Some((mut rawe,exp)) => {
                       let mb_f = sci_to_f64(int, exp);
                       if let Some(f) = mb_f {
                          rawe.insert_str(0, raw.as_str());
                          return Ok(Token::TokNumber(rawe,f));
                       } else {
                          return Err(ParserErrorType::ErrNumberOutOfRange);
                       }
                    },
                    None => return Ok(Token::TokInt(raw,int))
                 }
               },
               (Some((mut raw, sint)),Some((rawf,frac))) => {
                  let mut sint_c = sint.clone();
                  sint_c.push_str(frac.as_str());
                  let val = sint_c.parse::<f64>().unwrap();
                  let mb_e = self.exponent()?;
                  match mb_e {
                     Some((estr,e)) => {
                        let valf = sci_to_f64_(val, e);
                        if let Some(f) = valf {
                           raw.push_str(rawf.as_str());
                           raw.push_str(estr.as_str());
                           return Ok(Token::TokNumber(raw,f));
                        } else {
                           return Err(ParserErrorType::ErrNumberOutOfRange);
                        }
                     },
                     None => {
                        raw.push_str(rawf.as_str());
                        return Ok(Token::TokNumber(raw,val));
                     }
                  }
               },
               (None,Some((mut rawf,frac))) => {
                  //maybe never run this?
                  let val = frac.parse::<f64>().unwrap();
                  let mb_e = self.exponent()?;
                  match mb_e {
                     Some((estr,e)) => {
                        let valf = sci_to_f64_(val, e);
                        if let Some(f) = valf {
                           rawf.push_str(estr.as_str());
                           return Ok(Token::TokNumber(rawf,f));
                        } else {
                           return Err(ParserErrorType::ErrNumberOutOfRange);
                        }
                     },
                     None => return Ok(Token::TokNumber(rawf,val))
                  }
               },
               _ => {
                 let mc = self.string.lookahead(1);
                 let cstr = mc.map(|c| {let mut r = String::default(); r.push(c); r } );
                 return Err(ParserErrorType::ErrLexeme(cstr,vec![]));
               }
            }
         }
      }
   }

   fn exponent(&mut self) -> Result<Option<(String,i32)>,ParserErrorType> {
      let mb_chr = self.string.lookahead(1);
      match mb_chr {
         Some('e') => {
            self.next();
            let mb_chr2 = self.string.lookahead(1);
            let (neg,chr) = match mb_chr2 {
               Some('-') => { self.next(); (true,"-") },
               Some('+') => { self.next(); (false,"+") },
               _ => (false,"")
            };
            let mb_int= self.integer()?;
            match mb_int {
               Some((mut raw,chs)) => {
                  let int = if neg {
                     -Lexer::digits_to_integer(chs.as_str())
                  } else {
                     Lexer::digits_to_integer(chs.as_str())
                  };
                  raw.insert_str(0, chr);
                  raw.insert(0, 'e');
                  return Ok(Some((raw,int)));
               },
               None => return Err(ParserErrorType::ErrExpectedExponent)
            }
         },
         _ => return Ok(None)
      }

     
   }

   fn fraction(&mut self) -> Result<Option<(String,String)>,ParserErrorType> {
      let chr1 = self.string.lookahead(1);      
      match chr1 {
         Some('.') => {
            self.next();
            let nums = self.string.take_while(Lexer::is_number_char);
            match nums {
               Some(str) => {
                  let mut raw = String::from(str);
                  raw.insert(0, '.');
                  let ret:String = raw.chars().filter(|c| *c != '_').collect();
                  return Ok(Some((raw,ret)));
               }
               None => {
                  self.string.put_back('.');
                  return Ok(None)
               }
            }
         },
         _ => return Ok(None)
      }
   }

   fn integer(&mut self) -> Result<Option<(String,String)>,ParserErrorType> {
      let mb_chr = self.string.lookahead(1);
      match mb_chr {
         Some('0') => {
            self.next();
            let mb_chr2 = self.string.lookahead(1);
            match mb_chr2 {
               Some(c) if Lexer::is_number_char(c) => return Err(ParserErrorType::ErrLeadingZero),
               _ => {
                  let zero_str = String::from("0");
                  return Ok(Some((zero_str.clone(),zero_str)));
               } 
            }
         },
         Some(ch) if Lexer::is_digit_char(ch)  => {
             self.digits().map(|d|Some(d))
         },
         _ =>  return Ok(None)
      }
   }

   fn integer1(&mut self,chr:char) -> Result<Option<(String,String)>,ParserErrorType> {
      match chr {
         '0' => {
            let mb_ch = self.string.lookahead(1);
            match mb_ch {
              Some(c) if Lexer::is_number_char(c) => {
                 return  Err(ParserErrorType::ErrLeadingZero); 
              },
               _ => {
                  let zero_str = String::from("0");
                  return Ok(Some((zero_str.clone(),zero_str)));
               } 
            } 
         },
         chr if Lexer::is_digit_char(chr)  => {
            let (mut raw,mut numstr) = self.digits()?;
            raw.insert(0, chr);
            numstr.insert(0, chr);
            return Ok(Some((raw,numstr)));
         }
         _ =>  return Ok(None)
      }
   }

   fn digits(&mut self) -> Result<(String,String),ParserErrorType>{
      let arr = self.string.take_while(Lexer::is_number_char);
      let raw = arr.unwrap_or_default();
      let num_str:String = raw.chars().filter(|c| *c != '_').collect();
      return Ok((raw.to_owned(),num_str));
   }

   fn hole(&mut self) -> Result<Token,ParserErrorType> {
      let mb_name = self.string.take_while(Lexer::is_ident_char);
      if let Some(name) = mb_name {
         Ok(Token::TokHole(name.to_owned()))
      } else {
         Ok(Token::TokOperator(String::from("?")))
      }
   }

   fn digits_to_integer(str:&str) -> i32 {
      Lexer::digits_to_integer_base(10, str)
   }

   fn digits_to_integer_base(base:i32,str:&str) -> i32 {
      let mut n = 0;
      for c in str.chars() {
        n = n * base + (c.to_digit(16).unwrap() as i32)
      }
      n
   }

   fn lstring(&mut self) -> Result<Token,ParserErrorType> {
      let quotes1 = self.string.take_while(|chr| chr == '\"' ).map(|s|s.len()).unwrap_or(0);
      match quotes1 {
         0 => {
            let mut raw:String = String::default();
            let mut acc:String = String::default();
            loop {
               let normals = self.string.take_while(Lexer::is_normal_string_char);
               normals.map(|str|{
                  raw.push_str(str);
                  acc.push_str(str);
               });
               match self.string.lookahead(1) {
                  Some('"') => return {self.next(); Ok(Token::TokString(raw,acc))} ,
                  Some('\\') => {
                     self.next();
                     match self.string.lookahead(1) {
                        Some(chr) if Lexer::is_string_gap_char(chr) => {
                           let gap = self.string.take_while(Lexer::is_string_gap_char).unwrap_or_default().to_owned();
                           match self.string.lookahead(1) {
                              Some('"') => {
                                 self.next();
                                 raw.push_str(gap.as_str());
                                 return Ok(Token::TokString(raw,acc));
                              },
                              Some('\\') => {
                                 self.next();
                                 raw.push_str(gap.as_str());
                                 raw.push('\\');
                                 continue;
                              }
                              Some(ch) => return Err(ParserErrorType::ErrCharInGap(ch)),
                              None => return Err(ParserErrorType::ErrEof),
                           }
                        },
                        _ =>  {
                           let (rawc,chr) = self.escape()?;
                           raw.push_str(rawc.as_str());
                           acc.push(chr);
                           continue;
                        }
                     }
                  },
                  Some(_) => return Err(ParserErrorType::ErrLineFeedInString),
                  None => return Err(ParserErrorType::ErrEof)
               }

            }
         },
         1 => return Ok(Token::TokString(String::default(),String::default())),
         n if n >= 5  => {
            let str = String::from("\"\"\"\"\"");
            return Ok(Token::TokString(str.clone(),str));
         },
         _ => {
            let mut acc = String::default();
            loop {
               let chs = self.string.take_while(|chr| chr != '"').unwrap_or_default().to_owned();
               let quotes2 = self.string.take_while(|chr| chr == '"').unwrap_or_default().to_owned();
               match quotes2.len() {
                  0 => return Err(ParserErrorType::ErrEof),
                  n if n >= 3 => {
                     acc.push_str(chs.as_str());
                     let strskip:String = quotes2.chars().skip(3).collect();
                     acc.push_str(strskip.as_str());
                     return Ok(Token::TokRawString(acc));
                  },
                  _ => {
                     acc.push_str(chs.as_str());
                     acc.push_str(quotes2.as_str());
                     continue;
                  }
               }
            }
         }
      }
   }

   fn lchar(&mut self) -> Result<Token,ParserErrorType> {
      let chr = self.string.lookahead(1);
      let (raw,chr) = match chr {
         Some('\\') => {
           self.next();
           let (mut raw,chr) = self.escape()?;
           raw.insert(0, '\\');
           (raw,chr)
         },
         Some(chr) => {
            let mut str = String::default();
            str.push(chr);
            (str,chr)
         },
         None => return Err(ParserErrorType::ErrEof),
      };
      let mb_chr2 = self.string.lookahead(1);
      match mb_chr2 {
         Some('\'') => {
            if chr.to_digit(10).unwrap_or(0) > 0xFFFF {
              return Err(ParserErrorType::ErrAstralCodePointInChar);
            } else {
               self.next();
               return Ok(Token::TokChar(raw,chr))
            }
         },
         Some(chr2) => {
            let mut str = String::default();
            str.push(chr2);
            return Err(ParserErrorType::ErrLexeme(Some(str),vec![]));
         },
         None => return Err(ParserErrorType::ErrEof)
      }
   }
   
   fn escape(&mut self) -> Result<(String,char),ParserErrorType> {
      let mb_chr = self.string.lookahead(1);
      match mb_chr {
         Some('t') => {self.next(); Ok((String::from("t"),'\t')) },
         Some('r') => {self.next(); Ok((String::from("r"),'\r')) },
         Some('n') => {self.next(); Ok((String::from("n"),'\n')) },
         Some('"') => {self.next(); Ok((String::from("\""),'"')) },
         Some('\'') => {self.next(); Ok((String::from("'"),'\'')) },
         Some('\\') => {self.next(); Ok((String::from("\\"),'\\')) },
         Some('x') => {
            self.next();
            let mut n:u32 = 0;
            let mut real_len:usize = 0;
            let mut raw_chars:Vec<char> = vec!['x'];
            for idx in 0..6 {
              let mb_chr:Option<char> = self.string.lookahead(idx + 1);
              match mb_chr {
                 Some(chr) if chr.is_ascii_hexdigit() => {
                    let d = chr.to_digit(16).unwrap();
                    n = n * 16 + d;
                    real_len += 1;
                    raw_chars.push(chr);
                 },
                 _ => {
                    break;
                 }
              }
            }
            if n <= 0x10FFFF {
               for _ in 0..real_len {
                  self.next();
               }
               let ret_chr = unsafe { std::char::from_u32_unchecked(n) };
               return Ok((String::from_iter(raw_chars.iter()),ret_chr));
            }
            Err(ParserErrorType::ErrCharEscape)
         },
         _ => Err(ParserErrorType::ErrCharEscape)
      }
   }

   fn or_operator1(&mut self,tok:Token,chr:char) -> Result<Token,ParserErrorType> {
      let mb_chr = self.string.lookahead(1);
      if let Some(chr2) = mb_chr  {
         if Lexer::is_symbol_char(chr2) {
           self.next();
           self.operator(vec![], vec![chr,chr2])
         } else {
            Ok(tok)
         }
      } else {
         Ok(tok)
      }
   }
  
   fn or_operator2(&mut self,tok:Token,chr1:char,chr2:char) -> Result<Token,ParserErrorType> {
      let mb_chr = self.string.lookahead(1);
      if mb_chr.is_some() && mb_chr == Some(chr2) {
         let mb_chr3 = self.string.lookahead(2);
         match mb_chr3 {
            Some(chr3) if Lexer::is_symbol_char(chr3)  => {
               self.next();
               self.next();
               
               return self.operator(vec![], vec![chr1,chr2,chr3]);
            }
            _ => {
               self.next();
               return Ok(tok);
            }
         }
      }
      let mut ret = String::default();
      ret.push(chr1);
      return Ok(Token::TokOperator(ret ));
   }

   fn or_operator2_(&mut self,tok1:Token,tok2:Token,chr1:char,chr2:char) -> Result<Token,ParserErrorType> {
      let mb_chr2_ = self.string.lookahead(1);
      match mb_chr2_ {
         Some(chr2_) if chr2_ == chr2 => {
            let mb_chr3 = self.string.lookahead(2);
            match mb_chr3 {
               Some(chr3) if Lexer::is_symbol_char(chr3) => {
                  self.next();
                  self.next();
                  return self.operator(vec![],vec![chr1,chr2,chr3]);
               },
               _ => {
                  self.next();
                  return Ok(tok2);
               }
            }
         },
         _ => return Ok(tok1)
      }
   }

   fn operator(&mut self,qual:Vec<String>,pre:Vec<char>) -> Result<Token,ParserErrorType> {
      let rest = self.string.take_while(Lexer::is_symbol_char).unwrap_or_default();
      let mut op_str = String::from_iter(pre.iter());
      op_str.push_str(rest);
      match op_str.as_str() {
         "<=" if qual.len() == 0 => Ok(Token::TokLeftFatArrow(SourceStyle::ASCII)),
         "⇐" if qual.len() == 0 => Ok(Token::TokLeftFatArrow(SourceStyle::Unicode)),
         ":" if qual.len() == 0  => Ok(Token::TokColon),
         "-" if qual.len() == 0 => Ok(Token::TokOperatorSub),
         "@" if qual.len() == 0 => Ok(Token::TokOperatorAt),
         "#" if qual.len() == 0 => Ok(Token::TokOperatorHash),
         _ => {
            if qual.len() == 0 {
               Ok(Token::TokOperator(op_str))
            } else {
               Ok(Token::TokQualOperator(qual,op_str))
            }
         }
      }
      
   }

   fn  left_paren(&mut self) -> Result<Token,ParserErrorType>  {
      let mb_op = self.string.take_while(Lexer::is_symbol_char).map(|s|s.to_string());
      match mb_op {
         Some(op) => {
            let next_chr = self.string.lookahead(1);
            if next_chr == Some(')') {
               match op.as_str() {
                  "→" => {
                     self.next();
                     Ok(Token::TokSymbolArr(SourceStyle::Unicode))
                  },
                  "->" => {
                     self.next();
                     Ok(Token::TokSymbolArr(SourceStyle::ASCII))
                  }
                  op_str =>  {
                     if Lexer::is_reserved_symbol(op_str) {
                        self.string.put_rollback(op_str);
                        Err(ParserErrorType::ErrReservedSymbol)
                     } else {
                        self.next();
                        if op_str == ".." {
                           Ok(Token::TokSymbolDoubleDot)
                        } else {
                           Ok(Token::TokSymbolName(op_str.to_string()))
                        }
                        
                     }
                  }
               }
            } else {
               Ok(Token::TokLeftParen) 
            }
         },
         None => Ok(Token::TokLeftParen)
      }
   }

   pub fn break_comments(&mut self) -> (Vec<Comment>,Vec<Comment>) {
     let mut commnet_void:Vec<Comment> = vec![];
     let mut commnet_line:Vec<Comment> = vec![];
     loop {
        let spaces = self.string.take_while(|chr| chr == ' ').map(|s| s.len());
        let lines = self.string.take_while( LineFeed::is_line_feed).map(|s|s.to_string());
        if let Some(size) = spaces {
           commnet_void.push(Comment::Space(size));
        }
        if lines.is_none() {
           let comment = self.comment();
           match comment {
              Some(c) => {
                 commnet_void.push(c);
              },
              None => { break }
           }
        } else {
           let comments = self.take_ws(lines.unwrap().as_str());
           commnet_line.extend(comments);
           loop {
              let ws = self.string.take_while(|chr| chr == ' ' || LineFeed::is_line_feed(chr)).map(|s| s.to_string());
              if let Some(str) = ws {
                let comments = self.take_ws(str.as_str());
                commnet_line.extend(comments);
              }
              let comment = self.comment();
              match comment {
               Some(c) => {
                  commnet_line.push(c);
               },
               None => { break }
            }
           }
           break;
        }
     }
     return (commnet_void,commnet_line);
   }

   fn take_ws(&mut self,ws_str:&str) -> Vec<Comment> {
      let mut comments = vec![];
      let chars:Vec<char> = ws_str.chars().collect();
      let mut idx = 0;
      let mut space_num:i32 = -1;
      let check_push_space = |comments:&mut Vec<Comment>,space_num:&mut i32| {
         if *space_num != -1 {
            comments.push(Comment::Space(*space_num as usize));
            *space_num = -1;
         }
      };
      while idx < chars.len() {
         let chr = chars[idx];
         if chr == ' ' {
            if space_num != -1 {
               space_num += 1;
            } else {
               space_num = 1;
            }
            idx += 1
         } else if chr == '\r' {
            check_push_space(&mut comments,&mut space_num);
            if idx + 1 < chars.len() && chars[idx + 1] == '\n' {
               idx += 1;
               comments.push(Comment::Line(LineFeed::CRLF))
            } else {
               comments.push(Comment::Line(LineFeed::CRLF))
            }
            idx += 1;
         } else if chr == '\n' {
            check_push_space(&mut comments,&mut space_num);
            comments.push(Comment::Line(LineFeed::LF));
            idx += 1
         }
      }
      check_push_space(&mut comments,&mut space_num);
      comments
   }

   pub fn comment(&mut self) -> Option<Comment> {
      let chr1 = self.string.lookahead(1);
      let chr2 = self.string.lookahead(2);
      match chr1 {
         Some('-')  => {
            if chr2 == Some('-') {
              return  self.line_comment();
            }
         },
         Some('{') => {
            if chr2 == Some('-') {
               self.next(); // {
               self.next(); // -
               return self.block_comment( String::from("{-"));
            }
         },
         _ => {}
      }
      return  None;
   }
   
   fn line_comment(&mut self) -> Option<Comment> {
      let comm = self.string.take_while(|chr| chr != '\r' && chr != '\n');
      comm.map(|str| Comment::Comment(str.to_string()))
   }

   fn block_comment(&mut self,mut acc: String) -> Option<Comment> {
      let chs = self.string.take_while(|chr| chr != '-').map(|str| str.to_string()).unwrap_or_default();
      let dashes =  self.string.take_while(|chr| chr == '-');
     
      if dashes.is_none() {
         acc.push_str(chs.as_str());
         return  Some(Comment::Comment(chs));
      } else {
         let dashes_str = dashes.unwrap().to_string();
         let next = self.string.lookahead(1);
         match next {
            Some('}') => {
               self.next();
               acc.push_str(chs.as_str());
               acc.push_str(dashes_str.as_str());
               acc.push('}');
               return Some(Comment::Comment(acc));
            },
            _ => {
               acc.push_str(chs.as_str());
               acc.push_str(dashes_str.as_str());
               return self.block_comment(acc);
            }
         }
      }
      
   }

   fn is_symbol_char(chr:char) -> bool {
      //if chr == '∀' {
      //   return false;
      //}
      if SYMBOL_SET.contains(&chr) {
         return true;
      }
      if chr.is_ascii() {
         return false;
      }
      let cate = get_general_category(chr);
      match cate {
         GeneralCategory::MathSymbol => true,
         GeneralCategory::CurrencySymbol => true,
         GeneralCategory::ModifierSymbol => true,
         GeneralCategory::OtherSymbol => true,
         _ => false
      }
   }

   fn is_reserved_symbol(str:&str) -> bool {
      return RESERVED_SYMBOL_SET.contains(str)
   }

   fn is_ident_char(chr:char) -> bool {
      chr.is_ascii_alphanumeric() || chr == '_' || chr == '\''
   }

   fn is_ident_start(chr:char) -> bool {
      chr.is_lowercase() || chr == '_'
   }

   fn is_normal_string_char(chr:char) -> bool {
      return  chr != '"' && chr != '\\' && chr != '\r' && chr != '\n'
   }

   fn is_string_gap_char(chr:char) -> bool {
      return chr == ' ' || chr == '\r' || chr == '\n';
   }
   
   fn is_number_char(chr:char) -> bool {
      return  (chr >= '0' && chr <= '9') || chr == '_'
   }

   fn is_digit_char(chr:char) -> bool {
      return  chr >= '0' && chr <= '9';
   }

}

pub fn sci_to_f64(c:i32,e:i32) -> Option<f64> {
   if c == 0 {
       return Some(0f64);
   }
   if e > 63 || e < -63 {
       return None;
   }
   if e < 0 {
       Some( c as f64 / 10f64.powi(-e))
   } else {
       Some( (c as f64 * 10f64.powi(e) as f64) as f64)
   }
}

pub fn sci_to_f64_(c:f64,e:i32) -> Option<f64> {
   if c == 0f64 {
       return Some(0f64);
   }
   if e > 63 || e < - 63 {
       return None;
   }
   if e < 0 {
       Some( c  / 10f64.powi(-e))
   } else {
       Some( (c * 10f64.powi(e) as f64) as f64)
   }
}

#[test]
fn test_lex() {
   let code_string = std::fs::read_to_string("tests/main.purs").unwrap();
   let mut lex = Lexer::new(code_string.as_str());
   let tokens = lex.lex();
   for t in tokens {
      println!("{:}",t.unwrap().value);
   }
}