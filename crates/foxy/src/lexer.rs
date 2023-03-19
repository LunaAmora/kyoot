use std::{fmt, io::Read};

use anyhow::Result;
use chumsky::{error::Rich, extra::Err, prelude::*};

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type ErrorType<'a, T> = Rich<'a, T>;
pub type OutputErrors<V, E> = (Option<V>, Vec<E>);

type Tokens<'src> = Vec<Spanned<Token<'src>>>;
type LexError<'src> = ErrorType<'src, char>;
type LexOutput<'src> = OutputErrors<Tokens<'src>, LexError<'src>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    Num(f64),
    Ctrl(char),
    Op(&'src str),
    Str(&'src str),
    Ident(&'src str),
    Keyword(Keyword),
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{}", x),
            Self::Num(n) => write!(f, "{}", n),
            Self::Str(s) => write!(f, "{}", s),
            Self::Op(s) => write!(f, "{}", s),
            Self::Ctrl(c) => write!(f, "{}", c),
            Self::Ident(s) => write!(f, "{}", s),
            Self::Keyword(k) => write!(f, "{}", k),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Struct,
    If,
    Else,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Struct => write!(f, "struct"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
        }
    }
}

pub struct Lexer {
    pub src: String,
}

impl Lexer {
    pub fn new(mut buff: impl Read) -> Result<Self> {
        let mut src = String::new();
        buff.read_to_string(&mut src)?;
        Ok(Self { src })
    }

    pub fn lex(&self) -> LexOutput {
        lex().parse(&self.src).into_output_errors()
    }

    pub fn span(&self) -> SimpleSpan {
        (self.src.len()..self.src.len()).into()
    }
}

fn lex<'src>() -> impl Parser<'src, &'src str, Tokens<'src>, Err<LexError<'src>>> {
    // A parser for numbers
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .from_str()
        .unwrapped()
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(Token::Str);

    // A parser for operators
    let op = one_of("+*-/!=:>")
        .repeated()
        .at_least(1)
        .map_slice(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{},|.").map(Token::Ctrl);

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: &str| match ident {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "if" => Token::Keyword(Keyword::If),
        "else" => Token::Keyword(Keyword::Else),
        "struct" => Token::Keyword(Keyword::Struct),
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num.or(str_).or(op).or(ctrl).or(ident);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
