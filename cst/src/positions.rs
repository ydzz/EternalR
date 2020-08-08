use crate::types::{SourceStyle, Token,Comment,LineFeed,SourcePos};


pub fn advance_token(spos:&SourcePos,tok:&Token)  -> SourcePos  {
    let mut pos = spos.clone();
    apply_delta(&mut pos,token_delta(tok));
    pos
}

pub fn advance_leading( spos:&SourcePos,comments:&Vec<Comment>) -> SourcePos {
    let mut pos = spos.clone();
    for comment in comments {
        let cd = comment_delta(line_delta, comment);
        apply_delta(&mut pos,cd);
    }
    pos
}

pub fn advance_trailing( spos:&SourcePos,comments:&Vec<Comment>) -> SourcePos {
    let mut pos = spos.clone();
    for comment in comments {
        let cd = comment_delta(|_| (0,0), comment);
        apply_delta(&mut pos,cd);
    }
    pos
}

pub fn token_delta(tok: &Token) -> (i32, i32) {
    match tok {
        Token::TokLeftParen => (0, 1),
        Token::TokRightParen => (0, 1),
        Token::TokLeftBrace => (0, 1),
        Token::TokRightBrace => (0, 1),
        Token::TokLeftSquare => (0, 1),
        Token::TokRightSquare => (0, 1),
        Token::TokLeftArrow(SourceStyle::ASCII) => (0, 2),
        Token::TokLeftArrow(SourceStyle::Unicode) => (0, 1),
        Token::TokRightArrow(SourceStyle::ASCII) => (0, 2),
        Token::TokRightArrow(SourceStyle::Unicode) => (0, 1),
        Token::TokRightFatArrow(SourceStyle::ASCII) => (0, 2),
        Token::TokRightFatArrow(SourceStyle::Unicode) => (0, 1),
        Token::TokLeftFatArrow(SourceStyle::ASCII) => (0, 2),
        Token::TokLeftFatArrow(SourceStyle::Unicode) => (0, 1),
        Token::TokDoubleColon(SourceStyle::ASCII) => (0, 2),
        Token::TokDoubleColon(SourceStyle::Unicode) => (0, 1),
        Token::TokForall => (0, 6),
        Token::TokForallU => (0, 1),
        Token::TokEquals => (0, 1),
        Token::TokPipe => (0, 1),
        Token::TokTick => (0, 1),
        Token::TokDot => (0, 1),
        Token::TokComma => (0, 1),
        Token::TokUnderscore => (0, 1),
        Token::TokBackslash => (0, 1),
        Token::TokQualLowerName(qual,name) => (0,qual_delta(&qual) + name.len() as i32),
        Token::TokLowerName(name) => (0,name.len() as i32),
        Token::TokQualUpperName(qual,name) => (0,qual_delta(&qual) + name.len() as i32),
        Token::TokUpperName(name) => (0,name.len() as i32),
        Token::TokQualOperator(qual,sym) => (0,qual_delta(&qual) + sym.len() as i32),
        Token::TokOperator(sym) => (0, sym.len() as i32),
        Token::TokColon => (0,1),
        Token::TokOperatorAt => (0,1),
        Token::TokOperatorHash => (0,1),
        Token::TokOperatorSub => (0,1),
        Token::TokAdo => (0,3),
        Token::TokAs => (0,2),
        Token::TokCase => (0,4),
        Token::TokClass => (0,5),
        Token::TokData => (0,4),
        Token::TokDerive => (0,6),
        Token::TokDo => (0,2),
        Token::TokElse => (0,4),
        Token::TokFalse => (0,5),
        Token::TokForeign => (0,7),
        Token::TokHiding => (0,6),
        Token::TokImport => (0,7),
        Token::TokIf => (0,2),
        Token::TokIn => (0,2),
        Token::TokInfix => (0,5),
        Token::TokInfixl => (0,6),
        Token::TokInfixr => (0,6),
        Token::TokInstance => (0,8),
        Token::TokKind => (0,4),
        Token::TokLet => (0,3),
        Token::TokModule => (0,6),
        Token::TokNewtype => (0,7),
        Token::TokNominal => (0,7),
        Token::TokPhantom => (0,7),
        Token::TokOf => (0,2),
        Token::TokRepresentational => (0,16),
        Token::TokRole => (0,4),
        Token::TokThen => (0,4),
        Token::TokTrue => (0,4),
        Token::TokType => (0,4),
        Token::TokWhere => (0,5),
        Token::TokQualSymbolName(qual,sym) => (0,qual_delta(&qual) + sym.len() as i32 + 2),
        Token::TokSymbolName(sym) => (0,sym.len() as i32 + 2),
        Token::TokSymbolDoubleDot => (0,2),
        Token::TokSymbolArr(SourceStyle::Unicode) => (0, 3),
        Token::TokSymbolArr(SourceStyle::ASCII)   => (0, 4),
        Token::TokHole(hole) => (0,hole.len() as i32 + 1),
        Token::TokChar(raw,_) => (0,raw.len() as i32 + 2),
        Token::TokInt(raw,_) => (0,raw.len() as i32),
        Token::TokNumber(raw,_) => (0,raw.len() as i32),
        Token::TokString(raw,_) => multi_line(1, text_delta(&raw)),
        Token::TokRawString(raw) => multi_line(3, text_delta(&raw)), 
        Token::TokLayoutStart           => (0, 0),
        Token::TokLayoutSep             => (0, 0),
        Token::TokLayoutEnd             => (0, 0),
        Token::TokEof                   => (0, 0),
    }
}


fn multi_line(n:i32,lc:(i32,i32)) -> (i32,i32) {
    match lc {
        (0,c) => (0,c + n + n),
        (l,c) => (l,c + n),
    }
}

fn qual_delta(texts:&Vec<String>) -> i32 {
    let mut delta = 0;
    for text in texts {
        delta += text.len();
    }
    delta as i32
}

pub fn comment_delta(f:impl FnOnce(&LineFeed) -> (i32,i32),comment:&Comment) -> (i32,i32) {
    match comment {
        Comment::Space(n) => (0,*n as i32),
        Comment::Comment(c) => text_delta(c.as_str()),
        Comment::Line(l) => f(l)
    }
}

fn line_delta(_l:&LineFeed) -> (i32,i32) {
    (1,1)
}

fn apply_delta(pos:&mut SourcePos,lc:(i32,i32)) {
    match lc {
        (0,n) => {
            pos.column += n as u32;
        },
        (k,d) => {
            pos.column = d as u32;
            pos.line = pos.line + k as u32;
        }
    }
}

pub fn text_delta(tex:&str) -> (i32,i32) {
    let mut l = 0;
    let mut c = 0;
    for chr in tex.chars() {
        if chr == '\n' {
            l += 1;
            c = 1;
        } else {
            c += 1;
        }
    }
    (l,c)
}

#[test]
fn test_position() {
    assert_eq!(text_delta("123\n45"),(1,3));
  
}