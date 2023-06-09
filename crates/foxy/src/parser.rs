use std::collections::HashMap;

use chumsky::{extra::Err, input::SpannedInput, prelude::*, recovery::ViaParser, Parser};

use crate::lexer::{
    ErrorType,
    Keyword::{Else, If},
    OutputErrors, Span, Spanned, Token,
};

#[derive(Debug)]
pub struct Module<'src> {
    pub name: Option<&'src str>,
    pub funcs: HashMap<&'src str, Func<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'src> {
    Unit,
    Bool(bool),
    Num(f64),
    Str(&'src str),
    Func(&'src str),
    // List(Vec<Self>),
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum Expr<'src> {
    Error,
    Local(&'src str),
    Value(Value<'src>),
    BinaryOp(BinaryOp),
    List(Vec<Spanned<Self>>),
    Call(Box<Spanned<Self>>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(&'src str, Option<Type<'src>>, Box<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Closure(Box<Func<'src>>),
}

#[derive(Debug)]
pub struct Type<'src>(&'src str);

#[derive(Debug)]
pub struct Func<'src> {
    pub args: Vec<(&'src str, Option<Type<'src>>)>,
    pub span: Span,
    pub body: Spanned<Expr<'src>>,
}

type ParserError<'tokens, 'src> = ErrorType<'tokens, Token<'src>>;
type ParserOutput<'tokens, 'src> =
    OutputErrors<Spanned<Module<'src>>, ParserError<'tokens, 'src>>;

type ParserInput<'tokens, 'src> =
    SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

trait IParser<'tokens, 'src: 'tokens, I> =
    Parser<'tokens, ParserInput<'tokens, 'src>, I, Err<ParserError<'tokens, 'src>>>;

pub fn parse<'tokens, 'src: 'tokens>(
    tokens: &'src Vec<(Token, SimpleSpan)>, eoi: Span,
) -> ParserOutput<'tokens, 'src> {
    parse_module()
        .map_with_span(|ast, span| (ast, span))
        .parse(tokens.as_slice().spanned(eoi))
        .into_output_errors()
}

fn block_recovery<'tokens, 'src: 'tokens>(
) -> ViaParser<impl IParser<'tokens, 'src, Spanned<Expr<'src>>> + Clone> {
    via_parser(nested_delimiters(
        Token::Ctrl('{'),
        Token::Ctrl('}'),
        [
            (Token::Ctrl('('), Token::Ctrl(')')),
            (Token::Ctrl('['), Token::Ctrl(']')),
        ],
        |span| (Expr::Error, span),
    ))
}

fn parse_expr<'tokens, 'src: 'tokens>(
) -> impl IParser<'tokens, 'src, Spanned<Expr<'src>>> + Clone {
    recursive(|expr| {
        // Blocks are expressions but delimited with braces
        let block = just(Token::Ctrl('{'))
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Ctrl('}')))
            .map_with_span(|body, span| {
                body.unwrap_or_else(|| (Expr::Value(Value::Unit), span))
            })
            .recover_with(block_recovery());

        let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

        let pattern = (ident.then(just(Token::Op(":")).ignore_then(ident).or_not()))
            .map(|(name, typ)| (name, typ.map(Type)))
            .labelled("pattern");

        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Bool(x) => Expr::Value(Value::Bool(x)),
                Token::Num(n) => Expr::Value(Value::Num(n)),
                Token::Str(s) => Expr::Value(Value::Str(s)),
            };

            // A list of expressions
            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect::<Vec<_>>();

            let list = items
                .clone()
                .map(Expr::List)
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

            let cls_args = pattern
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect()
                .delimited_by(just(Token::Ctrl('|')), just(Token::Ctrl('|')))
                .map_with_span(|args, span| (args, span));

            let closure = cls_args.then(block.clone().or(inline_expr.clone())).map(
                |((args, span), body)| Expr::Closure(Box::new(Func { args, span, body })),
            );

            // Function calls have very high precedence so we prioritise them
            let call = choice((
                ident.map_with_span(|name, span| (Expr::Value(Value::Func(name)), span)),
                closure.clone().map_with_span(|cls, span| (cls, span)),
                block.clone(),
            ))
            .then_ignore(just(Token::Ctrl('(')))
            .then_ignore(just(Token::Ctrl(')')))
            .map(|call| Expr::Call(Box::new(call)));

            choice((call, closure, val, ident.map(Expr::Local), list))
                .map_with_span(|expr, span| (expr, span))
                .labelled("inlined expression")
        });

        let op = select! {
            Token::Op("+") => Expr::BinaryOp(BinaryOp::Add),
            Token::Op("-") => Expr::BinaryOp(BinaryOp::Sub),
            Token::Op("*") => Expr::BinaryOp(BinaryOp::Mul),
            Token::Op("/") => Expr::BinaryOp(BinaryOp::Div),
            Token::Op("=") => Expr::BinaryOp(BinaryOp::Eq),
            Token::Op("!=") => Expr::BinaryOp(BinaryOp::NotEq),
        }
        .map_with_span(|op, span| (op, span))
        .labelled("op");

        let code_block = inline_expr.clone().or(block.clone()).labelled("block");

        // A let expression
        let let_ = (pattern
            .clone()
            .filter(|(_, typ)| typ.is_some())
            .then_ignore(just(Token::Op("=")).or(just(Token::Op(":")))))
        .or(pattern
            .filter(|(_, typ)| typ.is_none())
            .then_ignore(just(Token::Op(":=")).or(just(Token::Op("::")))))
        .then(code_block.clone())
        .map_with_span(|((name, val), inline), span| {
            (Expr::Let(name, val, Box::new(inline)), span)
        })
        .labelled("let expression");

        let if_ = recursive(|if_| {
            just(Token::Keyword(If))
                .ignore_then(code_block.clone())
                .then(
                    just(Token::Keyword(Else))
                        .ignore_then(code_block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|(a, b), span: Span| {
                    (
                        Expr::If(
                            Box::new(a),
                            // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                            Box::new(
                                b.unwrap_or_else(|| (Expr::Value(Value::Unit), span)),
                            ),
                        ),
                        span,
                    )
                })
        })
        .labelled("if expression");

        choice((let_, op, code_block, if_)).foldl(expr.repeated(), |a, b| {
            let span = a.1.start..b.1.end;
            (Expr::Then(Box::new(a), Box::new(b)), span.into())
        })
    })
}

fn parse_funcs<'tokens, 'src: 'tokens>(
) -> impl IParser<'tokens, 'src, HashMap<&'src str, Func<'src>>> {
    let ident = select! { Token::Ident(ident) => ident };
    let pattern = (ident.then(just(Token::Op(":")).ignore_then(ident)))
        .or(ident.map(|typ| ("", typ)))
        .map(|(name, typ)| (name, Some(Type(typ))))
        .labelled("pattern");

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = pattern
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let body = just(Token::Ctrl('{'))
        .then(just(Token::Ctrl('}')))
        .map_with_span(|_, span| (Expr::Value(Value::Unit), span))
        .or(parse_expr().delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
        .recover_with(block_recovery());

    let func = ident
        .map_with_span(|name, span| (name, span))
        .labelled("function name")
        .then_ignore(just(Token::Op("::")))
        .then(args)
        .map_with_span(|start, span| (start, span))
        .then(body)
        .map(|(((name, args), span), body)| (name, Func { args, span, body }))
        .labelled("function");

    func.repeated()
        .collect::<Vec<_>>()
        .validate(|fs, _, emitter| {
            let mut funcs = HashMap::new();
            for ((name, name_span), f) in fs {
                if funcs.insert(name, f).is_some() {
                    emitter.emit(Rich::custom(
                        name_span,
                        format!("Function '{}' already exists", name),
                    ));
                }
            }
            funcs
        })
}

fn parse_module<'tokens, 'src: 'tokens>() -> impl IParser<'tokens, 'src, Module<'src>> {
    parse_funcs().map(|funcs| Module { name: None, funcs })
}
