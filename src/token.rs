use logos::Logos as _;

use crate::span::Span;

pub fn tokenize(src: &str) -> Tokens<'_> {
    let lexer = Lexer::new(src);
    let mut tokens = Tokens::new(src);

    for token in lexer {
        tokens.append(token);
    }

    tokens
}

pub struct Tokens<'src> {
    src: &'src str,
    kind: Vec<TokenKind>,
    start: Vec<u32>,
}

impl<'src> Tokens<'src> {
    fn new(src: &'src str) -> Self {
        // shrug
        let capacity = src.len() / 7;
        Self {
            src,
            kind: Vec::with_capacity(capacity),
            start: Vec::with_capacity(capacity),
        }
    }

    fn append(&mut self, token: RawToken) {
        self.kind.push(token.kind);
        self.start.push(token.span.start);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.kind.len()
    }

    #[inline]
    pub fn cursor<'tokens>(&'tokens self) -> TokenCursor<'src, 'tokens> {
        TokenCursor {
            tokens: self,
            index: 0,
        }
    }

    #[inline]
    pub fn kind(&self, token: Token) -> TokenKind {
        if token.index() >= self.kind.len() {
            return TokenKind::Eof;
        }

        self.kind[token.index()]
    }

    #[inline]
    fn start(&self, token: Token) -> usize {
        if token.index() >= self.kind.len() {
            return self.src.len();
        }

        self.start[token.index()] as usize
    }

    #[inline]
    pub fn span(&self, token: Token) -> Span {
        let start = self.start(token);
        let kind = self.kind(token);
        let mut span = span(&self.src[start..], kind);
        span.start += start as u32;
        span.end += start as u32;
        span
    }

    #[inline]
    pub fn lexeme(&self, token: Token) -> &'src str {
        let start = self.start(token);
        let kind = self.kind(token);
        lexeme(&self.src[start..], kind)
    }

    #[inline]
    pub fn debug<'tokens>(&'tokens self, token: Token) -> DebugToken<'src, 'tokens> {
        DebugToken {
            tokens: self,
            token,
        }
    }
}

impl<'src> std::fmt::Debug for Tokens<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for token in self.cursor() {
            list.entry(&self.debug(token));
        }
        list.finish()
    }
}

pub struct DebugToken<'src, 'tokens> {
    tokens: &'tokens Tokens<'src>,
    token: Token,
}

impl<'src, 'tokens> std::fmt::Debug for DebugToken<'src, 'tokens> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = self.tokens.kind(self.token);
        let span = self.tokens.span(self.token);
        let lexeme = self.tokens.lexeme(self.token);

        write!(f, "{kind:?}({lexeme:?}, {span})")
    }
}

struct Lexer<'src> {
    inner: logos::SpannedIter<'src, TokenKind>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            inner: TokenKind::lexer(src).spanned(),
        }
    }
}

#[derive(Clone, Copy)]
struct RawToken {
    kind: TokenKind,
    span: Span,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = RawToken;

    fn next(&mut self) -> Option<Self::Item> {
        let (kind, span) = self.inner.next()?;
        let kind = match kind.unwrap_or_else(|_| TokenKind::Error) {
            TokenKind::Tabs => TokenKind::Error,
            kind => kind,
        };
        Some(RawToken {
            kind,
            span: span.into(),
        })
    }
}

fn tokenize_one(src: &str, kind: TokenKind) -> RawToken {
    match TokenKind::lexer(src).spanned().next() {
        Some((Ok(lexed_kind), span)) => {
            debug_assert!(
                kind == lexed_kind,
                "expected {kind:?}, got {lexed_kind:?} at {}",
                Span::from(span)
            );
            RawToken {
                kind: lexed_kind,
                span: span.into(),
            }
        }
        Some((Err(_), span)) => {
            debug_assert!(
                kind == TokenKind::Error,
                "got {kind:?} at {}",
                Span::from(span)
            );
            RawToken {
                kind: TokenKind::Error,
                span: span.into(),
            }
        }
        None => {
            debug_assert!(kind == TokenKind::Eof);
            RawToken {
                kind: TokenKind::Eof,
                span: Span::empty(),
            }
        }
    }
}

fn span(src: &str, kind: TokenKind) -> Span {
    tokenize_one(src, kind).span
}

fn lexeme(src: &str, kind: TokenKind) -> &str {
    match kind {
        TokenKind::Var => "var",
        TokenKind::Fn => "fn",
        TokenKind::Do => "do",
        TokenKind::Loop => "loop",
        TokenKind::End => "end",
        TokenKind::Return => "return",
        TokenKind::Break => "break",
        TokenKind::Continue => "continue",
        TokenKind::If => "if",
        TokenKind::Else => "else",
        TokenKind::ParenL => "(",
        TokenKind::ParenR => ")",
        TokenKind::BraceL => "{",
        TokenKind::BraceR => "}",
        TokenKind::BracketL => "[",
        TokenKind::BracketR => "]",
        TokenKind::Dot => ".",
        TokenKind::Comma => ",",
        TokenKind::Eq => "=",
        TokenKind::PlusEq => "+=",
        TokenKind::MinusEq => "-=",
        TokenKind::StarEq => "*=",
        TokenKind::SlashEq => "/=",
        TokenKind::Or => "or",
        TokenKind::And => "and",
        TokenKind::EqEq => "==",
        TokenKind::NotEq => "!=",
        TokenKind::Gt => ">",
        TokenKind::Ge => ">=",
        TokenKind::Lt => "<",
        TokenKind::Le => "<=",
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Star => "*",
        TokenKind::Slash => "/",
        TokenKind::Not => "not",
        TokenKind::True => "true",
        TokenKind::False => "false",
        TokenKind::Nil => "nil",

        kind @ (TokenKind::Ident | TokenKind::Integer | TokenKind::Float | TokenKind::String) => {
            &src[tokenize_one(src, kind).span]
        }
        TokenKind::Comment
        | TokenKind::Shebang
        | TokenKind::Tabs
        | TokenKind::Whitespace
        | TokenKind::Error
        | TokenKind::Eof => "",
    }
}

#[rustfmt::skip]
macro_rules! t {
    (var) => ($crate::token::TokenKind::Var);
    (fn) => ($crate::token::TokenKind::Fn);
    (do) => ($crate::token::TokenKind::Do);
    (loop) => ($crate::token::TokenKind::Loop);
    (end) => ($crate::token::TokenKind::End);
    (return) => ($crate::token::TokenKind::Return);
    (break) => ($crate::token::TokenKind::Break);
    (continue) => ($crate::token::TokenKind::Continue);
    (if) => ($crate::token::TokenKind::If);
    (else) => ($crate::token::TokenKind::Else);
    ("(") => ($crate::token::TokenKind::ParenL);
    (")") => ($crate::token::TokenKind::ParenR);
    ("{") => ($crate::token::TokenKind::BraceL);
    ("}") => ($crate::token::TokenKind::BraceR);
    ("[") => ($crate::token::TokenKind::BracketL);
    ("]") => ($crate::token::TokenKind::BracketR);
    (.) => ($crate::token::TokenKind::Dot);
    (,) => ($crate::token::TokenKind::Comma);
    (=) => ($crate::token::TokenKind::Eq);
    (+=) => ($crate::token::TokenKind::PlusEq);
    (-=) => ($crate::token::TokenKind::MinusEq);
    (*=) => ($crate::token::TokenKind::StarEq);
    (/=) => ($crate::token::TokenKind::SlashEq);
    (or) => ($crate::token::TokenKind::Or);
    (and) => ($crate::token::TokenKind::And);
    (==) => ($crate::token::TokenKind::EqEq);
    (!=) => ($crate::token::TokenKind::NotEq);
    (>) => ($crate::token::TokenKind::Gt);
    (>=) => ($crate::token::TokenKind::Ge);
    (<) => ($crate::token::TokenKind::Lt);
    (<=) => ($crate::token::TokenKind::Le);
    (+) => ($crate::token::TokenKind::Plus);
    (-) => ($crate::token::TokenKind::Minus);
    (*) => ($crate::token::TokenKind::Star);
    (/) => ($crate::token::TokenKind::Slash);
    (not) => ($crate::token::TokenKind::Not);

    (ident) => ($crate::token::TokenKind::Ident);
    (int) => ($crate::token::TokenKind::Integer);
    (float) => ($crate::token::TokenKind::Float);
    (str) => ($crate::token::TokenKind::String);
    (true) => ($crate::token::TokenKind::True);
    (false) => ($crate::token::TokenKind::False);
    (nil) => ($crate::token::TokenKind::Nil);

    (EOF) => ($crate::token::TokenKind::Eof);
}

pub struct TokenCursor<'src, 'tokens> {
    tokens: &'tokens Tokens<'src>,
    index: usize,
}

impl<'src, 'tokens> TokenCursor<'src, 'tokens> {
    #[inline]
    pub fn kind(&self, token: Token) -> TokenKind {
        self.tokens.kind(token)
    }

    #[inline]
    pub fn lexeme(&self, token: Token) -> &'src str {
        self.tokens.lexeme(token)
    }

    #[inline]
    pub fn span(&self, token: Token) -> Span {
        self.tokens.span(token)
    }

    #[inline]
    pub fn advance(&mut self) {
        let _ = self.next();
    }

    #[inline]
    pub fn current(&self) -> Token {
        Token(self.index as u32)
    }

    #[inline]
    pub fn peek(&self) -> Token {
        Token((self.index + 1) as u32)
    }
}

impl<'src, 'tokens> Iterator for TokenCursor<'src, 'tokens> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.tokens.len() {
            return None;
        }

        let token = self.current();
        self.index += 1;
        Some(token)
    }
}

#[derive(Clone, Copy)]
pub struct Token(u32);

impl Token {
    #[inline]
    fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, logos::Logos)]
pub enum TokenKind {
    #[token("var")]
    Var,
    #[token("fn")]
    Fn,
    #[token("do")]
    Do,
    #[token("loop")]
    Loop,
    #[token("end")]
    End,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
    #[token("{")]
    BraceL,
    #[token("}")]
    BraceR,
    #[token("[")]
    BracketL,
    #[token("]")]
    BracketR,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,

    #[token("=")]
    Eq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("not")]
    Not,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,
    #[regex(r"[0-9]([0-9_]*[0-9])?", priority = 100)]
    Integer,
    #[regex(r"[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?", priority = 10)]
    Float,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("nil")]
    Nil,

    #[regex(r"\t+")]
    Tabs,
    #[regex(r"[ \r\n\f]+", logos::skip)]
    Whitespace,

    #[regex(r"#[^\\n]*", logos::skip)]
    Comment,
    #[regex(r"#![^\\n]*", logos::skip)]
    Shebang,

    Error,
    Eof,
}
