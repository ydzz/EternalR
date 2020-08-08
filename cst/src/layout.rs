use crate::types::{Comment, SourcePos, SourceRange, SourceToken, Token, TokenAnn};

pub type LayoutStack = Vec<(SourcePos, LayoutDelim)>;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Copy)]
pub enum LayoutDelim {
  LytRoot,
  LytTopDecl,
  LytTopDeclHead,
  LytDeclGuard,
  LytCase,
  LytCaseBinders,
  LytCaseGuard,
  LytLambdaBinders,
  LytParen,
  LytBrace,
  LytSquare,
  LytIf,
  LytThen,
  LytProperty,
  LytForall,
  LytTick,
  LytLet,
  LytLetStmt,
  LytWhere,
  LytOf,
  LytDo,
  LytAdo,
}

fn is_indented(lyt: &LayoutDelim) -> bool {
  match lyt {
    LayoutDelim::LytLet => true,
    LayoutDelim::LytLetStmt => true,
    LayoutDelim::LytWhere => true,
    LayoutDelim::LytOf => true,
    LayoutDelim::LytDo => true,
    LayoutDelim::LytAdo => true,
    _ => false,
  }
}

fn is_top_decl(pos: &SourcePos, stack: &LayoutStack) -> bool {
  if stack.len() < 2 {
    return false;
  }
  if stack[0].1 == LayoutDelim::LytRoot
    && stack[1].1 == LayoutDelim::LytWhere
    && pos.column == stack[1].0.column
  {
    return true;
  }
  false
}

fn lyt_token(pos: &SourcePos, tok: Token) -> SourceToken {
  let ann = TokenAnn {
    range: SourceRange::new(pos.clone(), pos.clone()),
    leading_comments: vec![],
    trailing_comments: vec![],
  };
  SourceToken::new(ann, tok)
}

pub fn insert_layout(
  src: &SourceToken,
  next_pos: &SourcePos,
  stack: &mut LayoutStack,
) -> Vec<SourceToken> {
  let mut tokens: Vec<SourceToken> = vec![];
  let tok_pos = src.ann.range.start.clone();
  match &src.value {
    Token::TokData => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      if is_top_decl(&tok_pos, stack) {
        stack.push((tok_pos, LayoutDelim::LytTopDecl));
      } else {
        pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
      }
    }
    Token::TokClass => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      if is_top_decl(&tok_pos, stack) {
        stack.push((tok_pos, LayoutDelim::LytTopDeclHead));
      } else {
        pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
      }
    }
    Token::TokWhere => {
      let mb_last = stack.last();
      match mb_last {
        Some(last) if last.1 == LayoutDelim::LytTopDeclHead => {
          tokens.push(src.clone());
          insert_start(LayoutDelim::LytWhere, next_pos, stack, &mut tokens);
        }
        Some(last) if last.1 == LayoutDelim::LytProperty => {
          tokens.push(src.clone());
        }
        _ => {
          collapse(where_p, &tok_pos, stack, &mut tokens);
          tokens.push(src.clone());
          insert_start(LayoutDelim::LytWhere, next_pos, stack, &mut tokens);
        }
      }
    }
    Token::TokIn => {
      let cinfo = collapse(in_p, &tok_pos, stack, &mut tokens);
      let len = stack.len();
      if len >= 2
        && stack[len - 1].1 == LayoutDelim::LytAdo
        && stack[len - 2].1 == LayoutDelim::LytLetStmt
      {
        insert_end(&mut tokens, &tok_pos);
        insert_end(&mut tokens, &tok_pos);
        tokens.push(src.clone());
      } else if len >= 1 && is_indented(&stack[len - 1].1) {
        stack.pop();
        insert_end(&mut tokens, &tok_pos);
      
        tokens.push(src.clone());
       
      } else {
        revert_collapse(cinfo, stack, &mut tokens);
        insert_default(src.clone(), &tok_pos, stack, &mut tokens);
        pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
      }
    }
    Token::TokLet => {
      insert_kw_property(let_next, src, &tok_pos, next_pos, stack, &mut tokens);
    }
    Token::TokDo => {
      insert_kw_property(
        |_, _, next_pos, stack, tokens| {
          insert_start(LayoutDelim::LytDo, next_pos, stack, tokens);
        },
        src,
        &tok_pos,
        next_pos,
        stack,
        &mut tokens,
      );
    }
    Token::TokAdo => {
      insert_kw_property(
        |_, _, next_pos, stack, tokens| {
          insert_start(LayoutDelim::LytAdo, next_pos, stack, tokens);
        },
        src,
        &tok_pos,
        next_pos,
        stack,
        &mut tokens,
      );
    }
    Token::TokCase => {
      insert_kw_property(
        |_, tok_pos, _, stack, _| {
          stack.push((tok_pos.clone(), LayoutDelim::LytCase));
        },
        src,
        &tok_pos,
        next_pos,
        stack,
        &mut tokens,
      );
    }
    Token::TokOf => {
      collapse(indented_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) => {
          if lyt.1 == LayoutDelim::LytCase {
            stack.pop();
            tokens.push(src.clone());
            insert_start(LayoutDelim::LytOf, next_pos, stack, &mut tokens);
            stack.push((next_pos.clone(), LayoutDelim::LytCaseBinders));
          }
        }
        _ => {
          insert_default(src.clone(), &tok_pos, stack, &mut tokens);
          pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
        }
      }
    }
    Token::TokIf => {
      insert_kw_property(
        |_, tok_pos, _, stack, _| {
          stack.push((tok_pos.clone(), LayoutDelim::LytIf));
        },
        src,
        &tok_pos,
        next_pos,
        stack,
        &mut tokens,
      );
    }
    Token::TokThen => {
      let cinfo = collapse(indented_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytIf => {
          stack.pop();
          tokens.push(src.clone());
          stack.push((tok_pos.clone(), LayoutDelim::LytThen));
        },
        _ => {
          revert_collapse(cinfo, stack, &mut tokens);
          insert_default(src.clone(), &tok_pos, stack, &mut tokens);
          pop_stack(|l| LayoutDelim::LytProperty == *l, stack);
        }
      }
    }
    Token::TokElse => {
      let cinfo = collapse(indented_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytThen => {
          stack.pop();
          tokens.push(src.clone());
        }
        _ => {
          revert_collapse(cinfo, stack, &mut tokens);
          collapse(offside_p, &tok_pos, stack, &mut tokens);
          if is_top_decl(&tok_pos, stack) {
            tokens.push(src.clone());
          } else {
            insert_sep(&tok_pos, stack, &mut tokens);
            tokens.push(src.clone());
            pop_stack(|l| LayoutDelim::LytProperty == *l, stack);
          }
        }
      }
    }
    Token::TokForall | Token::TokForallU => {
      insert_kw_property(
        |_, tok_pos, _, stack, _| {
          stack.push((tok_pos.clone(), LayoutDelim::LytForall));
        },
        src,
        &tok_pos,
        next_pos,
        stack,
        &mut tokens,
      );
    }
    Token::TokBackslash => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      stack.push((tok_pos.clone(), LayoutDelim::LytLambdaBinders));
    }
    Token::TokRightArrow(_) => {
      collapse(arrow_p, &tok_pos, stack, &mut tokens);
      pop_stack(guard_p, stack);
      tokens.push(src.clone());
    }
    Token::TokEquals => {
      let cinfo = collapse(equals_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytDeclGuard => {
          stack.pop();
          tokens.push(src.clone());
        }
        _ => {
          revert_collapse(cinfo, stack, &mut tokens);
          insert_default(src.clone(), &tok_pos, stack, &mut tokens);
        }
      }
    }
    Token::TokPipe => {
      let cinfo = collapse(offside_end_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytOf => {
          stack.push((tok_pos.clone(), LayoutDelim::LytCaseGuard));
          tokens.push(src.clone());
        }
        Some(lyt) if lyt.1 == LayoutDelim::LytLet => {
          stack.push((tok_pos.clone(), LayoutDelim::LytDeclGuard));
          tokens.push(src.clone());
        }
        Some(lyt) if lyt.1 == LayoutDelim::LytLetStmt => {
          stack.push((tok_pos.clone(), LayoutDelim::LytDeclGuard));
          tokens.push(src.clone());
        }
        Some(lyt) if lyt.1 == LayoutDelim::LytWhere => {
          stack.push((tok_pos.clone(), LayoutDelim::LytDeclGuard));
          tokens.push(src.clone());
        }
        _ => {
          revert_collapse(cinfo, stack, &mut tokens);
          insert_default(src.clone(), &tok_pos, stack, &mut tokens);
        }
      }
    }
    Token::TokTick => {
      let cinfo = collapse(indented_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytTick => {
          stack.pop();
          tokens.push(src.clone());
        }
        _ => {
          revert_collapse(cinfo, stack, &mut tokens);
          insert_default(src.clone(), &tok_pos, stack, &mut tokens);
          stack.push((tok_pos.clone(), LayoutDelim::LytTick));
        }
      }
    }
    Token::TokComma => {
      collapse(indented_p, &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytBrace => {
          tokens.push(src.clone());
          stack.push((tok_pos.clone(), LayoutDelim::LytProperty));
        }
        _ => {
          tokens.push(src.clone());
        }
      }
    }
    Token::TokDot => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      match stack.last() {
        Some(lyt) if lyt.1 == LayoutDelim::LytForall => {
          stack.pop();
        }
        _ => stack.push((tok_pos.clone(), LayoutDelim::LytProperty)),
      }
    }
    Token::TokLeftParen => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      stack.push((tok_pos.clone(), LayoutDelim::LytParen))
    }
    Token::TokLeftBrace => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      stack.push((tok_pos.clone(), LayoutDelim::LytBrace));
      stack.push((tok_pos.clone(), LayoutDelim::LytProperty));
    }
    Token::TokLeftSquare => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      stack.push((tok_pos.clone(), LayoutDelim::LytSquare));
    }
    Token::TokRightParen => {
      collapse(indented_p, &tok_pos, stack, &mut tokens);
      pop_stack(|lyt| *lyt == LayoutDelim::LytParen, stack);
      tokens.push(src.clone());
    }
    Token::TokRightBrace => {
      collapse(indented_p, &tok_pos, stack, &mut tokens);
      pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
      pop_stack(|lyt| *lyt == LayoutDelim::LytBrace, stack);
      tokens.push(src.clone());
    }
    Token::TokRightSquare => {
      collapse(indented_p, &tok_pos, stack, &mut tokens);
      pop_stack(|lyt| *lyt == LayoutDelim::LytSquare, stack);
      tokens.push(src.clone());
    }
    Token::TokString(_, _) => {
      insert_default(src.clone(), &tok_pos, stack, &mut tokens);
      pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
    }
    Token::TokLowerName(_) => {
        insert_default(src.clone(), &tok_pos, stack, &mut tokens);
        pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
    },
    Token::TokAs | Token::TokDerive | Token::TokFalse | Token::TokTrue | Token::TokForeign | Token::TokHiding
     | Token::TokImport | Token::TokInfix | Token::TokInfixl | Token::TokInfixr | Token::TokInstance | Token::TokKind
     | Token::TokModule | Token::TokNewtype | Token::TokNominal | Token::TokRepresentational | Token::TokPhantom
     | Token::TokRole | Token::TokType
      => {
        insert_default(src.clone(), &tok_pos, stack, &mut tokens);
        pop_stack(|lyt| *lyt == LayoutDelim::LytProperty, stack);
    },
    Token::TokQualOperator(_, _) | Token::TokOperator(_) | Token::TokLeftFatArrow(_) | Token::TokColon
    | Token::TokOperatorSub | Token::TokOperatorHash | Token::TokOperatorAt => {
      collapse(offside_end_p, &tok_pos, stack, &mut tokens);
      insert_sep(&tok_pos, stack, &mut tokens);
      tokens.push(src.clone());
    }
    _ => insert_default(src.clone(), &tok_pos, stack, &mut tokens),
  }
  

  tokens
}

fn let_next(
  _src: &SourceToken,
  tok_pos: &SourcePos,
  next_pos: &SourcePos,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) {
  match stack.last() {
    Some(lyt) if lyt.1 == LayoutDelim::LytDo && lyt.0.column == tok_pos.column => {
      insert_start(LayoutDelim::LytLetStmt, next_pos, stack, tokens);
    }
    Some(lyt) if lyt.1 == LayoutDelim::LytAdo && lyt.0.column == tok_pos.column => {
      insert_start(LayoutDelim::LytLetStmt, next_pos, stack, tokens);
    }
    _ => insert_start(LayoutDelim::LytLet, next_pos, stack, tokens),
  }
}

fn insert_kw_property(
  f: impl Fn(&SourceToken, &SourcePos, &SourcePos, &mut LayoutStack, &mut Vec<SourceToken>),
  src: &SourceToken,
  tok_pos: &SourcePos,
  next_pos: &SourcePos,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) {
  insert_default(src.clone(), tok_pos, stack, tokens);
  match stack.last() {
    Some(last) if last.1 == LayoutDelim::LytProperty => {
      stack.pop();
    }
    _ => {
      f(src, tok_pos, next_pos, stack, tokens);
    }
  }
}

fn insert_end(tokens: &mut Vec<SourceToken>, tok_pos: &SourcePos) {
  let tok = lyt_token(tok_pos, Token::TokLayoutEnd);
  tokens.push(tok);
}

fn guard_p(lyt: &LayoutDelim) -> bool {
  match lyt {
    LayoutDelim::LytCaseBinders => true,
    LayoutDelim::LytCaseGuard => true,
    LayoutDelim::LytLambdaBinders => true,
    _ => false,
  }
}

fn equals_p( _: &SourcePos, _: &SourcePos, lyt: &LayoutDelim) -> bool {
  match lyt {
    LayoutDelim::LytWhere => true,
    LayoutDelim::LytLet => true,
    LayoutDelim::LytLetStmt => true,
    _ => false,
  }
}

fn arrow_p(tok_pos: &SourcePos, lyt_pos: &SourcePos, lyt: &LayoutDelim) -> bool {
  match lyt {
    LayoutDelim::LytDo => true,
    LayoutDelim::LytOf => false,
    _ => offside_end_p(tok_pos, lyt_pos, lyt),
  }
}

fn in_p(_: &SourcePos, _: &SourcePos, lyt: &LayoutDelim) -> bool {
  match lyt {
    LayoutDelim::LytLet => false,
    LayoutDelim::LytAdo => false,
    l => is_indented(l),
  }
}

fn where_p(tok_pos: &SourcePos, lyt_pos: &SourcePos, lyt: &LayoutDelim) -> bool {
  if *lyt == LayoutDelim::LytDo {
    true
  } else {
    offside_end_p(tok_pos, lyt_pos, lyt)
  }
}

fn insert_start(
  lyt: LayoutDelim,
  next_pos: &SourcePos,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) {
  let mut idx = stack.len();
  let mut mb_pos: Option<&SourcePos> = None;
  while idx > 0 {
    if is_indented(&stack[idx - 1].1) {
      mb_pos = Some(&stack[idx - 1].0);
      break;
    }
    idx -= 1
  }
  match mb_pos {
    Some(pos) if next_pos.column <= pos.column => {}
    _ => {
      stack.push((next_pos.clone(), lyt));
      tokens.push(lyt_token(next_pos, Token::TokLayoutStart));
    }
  }
}

fn pop_stack(p: impl Fn(&LayoutDelim) -> bool, stack: &mut LayoutStack) {
  if let Some(last) = stack.last() {
    if p(&last.1) {
      stack.pop();
    }
  }
}

fn insert_default(
  src: SourceToken,
  tok_pos: &SourcePos,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) {
  collapse(offside_p, tok_pos, stack, tokens);
  insert_sep(tok_pos, stack, tokens);
  tokens.push(src);
}

fn insert_sep(tok_pos: &SourcePos, stack: &mut LayoutStack, vec: &mut Vec<SourceToken>) {
  let mb_last = stack.last();
  if let Some(last) = mb_last {
    if (last.1 == LayoutDelim::LytTopDecl || last.1 == LayoutDelim::LytTopDeclHead)
      && sep_p(tok_pos, &last.0)
    {
      stack.pop();
      vec.push(sep_tok(tok_pos));
    } else if indent_sep_p(tok_pos, &last.0, &last.1) {
      if last.1 == LayoutDelim::LytOf {
        vec.push(sep_tok(tok_pos));
        stack.push((tok_pos.clone(), LayoutDelim::LytCaseBinders));
      } else {
        vec.push(sep_tok(tok_pos));
      }
    }
  }
}

fn sep_tok(tok_pos: &SourcePos) -> SourceToken {
  lyt_token(tok_pos, Token::TokLayoutSep)
}

fn collapse(
  p: impl Fn(&SourcePos, &SourcePos, &LayoutDelim) -> bool,
  tok_pos: &SourcePos,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) ->Vec< (Option<(SourcePos, LayoutDelim)>, bool)> {
  let mut opts:Vec<(Option<(SourcePos, LayoutDelim)>, bool)> = vec![];
  loop {
    match stack.last().map(|l| l.clone()) {
      Some(lyt) if p(tok_pos, &lyt.0, &lyt.1) => {
        let pop_lyt = stack.pop();
        let mut tp = (pop_lyt,false);
        if is_indented(&lyt.1) {
          tokens.push(lyt_token(tok_pos, Token::TokLayoutEnd));
          tp.1 = true;
        }
        opts.push(tp);
      }
      _ => break,
    }
  }
  opts
}

fn revert_collapse(
  mut info_arr: Vec<(Option<(SourcePos, LayoutDelim)>, bool)>,
  stack: &mut LayoutStack,
  tokens: &mut Vec<SourceToken>,
) {
  info_arr.reverse();
  for info in info_arr {
    if info.1 {
      tokens.pop();
    }
    if let Some(l) = info.0 {
      stack.push(l);
    }
  }

}

fn indented_p(_: &SourcePos, _: &SourcePos, lyt: &LayoutDelim) -> bool {
  is_indented(lyt)
}

fn offside_p(tok_pos: &SourcePos, lyt_pos: &SourcePos, lyt: &LayoutDelim) -> bool {
  is_indented(lyt) && tok_pos.column < lyt_pos.column
}

fn offside_end_p(tok_pos: &SourcePos, lyt_pos: &SourcePos, lyt: &LayoutDelim) -> bool {
  is_indented(lyt) && tok_pos.column <= lyt_pos.column
}

fn sep_p(tok_pos: &SourcePos, lyt_pos: &SourcePos) -> bool {
  tok_pos.column == lyt_pos.column && tok_pos.line != lyt_pos.line
}

fn indent_sep_p(tok_pos: &SourcePos, lyt_pos: &SourcePos, lyt: &LayoutDelim) -> bool {
  is_indented(lyt) && sep_p(tok_pos, lyt_pos)
}

pub fn unwind_layout(
  pos: &SourcePos,
  comments: &Vec<Comment>,
  stack: &mut LayoutStack,
) -> Vec<SourceToken> {
  let mut ret_tokens = vec![];
  loop {
    if stack.len() == 0 {
      break;
    }
    match stack.last() {
      Some(lyt) if lyt.1 == LayoutDelim::LytRoot => {
        ret_tokens.push(SourceToken::new(
          TokenAnn {
            range: SourceRange::new(pos.clone(), pos.clone()),
            leading_comments: comments.clone(),
            trailing_comments: vec![],
          },
          Token::TokEof,
        ));
        break;
      }
      Some(lyt) if is_indented(&lyt.1) => {
        let tok = lyt_token(pos, Token::TokLayoutEnd);
        stack.pop();
        ret_tokens.push(tok);
      }
      _ => {
        stack.pop();
      }
    }
  }
  ret_tokens
}
