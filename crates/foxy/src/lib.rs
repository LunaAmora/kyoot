use std::io::Read;

use anyhow::Result;
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use tracing::info;

use crate::{lexer::Lexer, parser::parse};
mod lexer;
mod parser;

pub fn compile(src: impl Read, source_name: &str) -> Result<()> {
    let lexer = Lexer::new(src)?;
    let (tokens, mut errs) = lexer.lex();

    let parse_errs = tokens.as_ref().map_or_else(Vec::new, |tokens| {
        let (ast, parse_errs) = parse(tokens, lexer.span());

        if let Some((module, file_span)) =
            ast.filter(|_| errs.len() + parse_errs.len() == 0)
        {
            if let Some(main) = module.funcs.get("main") {
                if !main.args.is_empty() {
                    errs.push(Rich::custom(
                        main.span,
                        "The main function cannot have arguments".to_string(),
                    ));
                } else {
                    info!("Result from the Foxy compiler:\n{:#?}", module.funcs);
                }
            } else {
                errs.push(Rich::custom(
                    file_span,
                    "Programs need a main function but none was found".to_string(),
                ));
            }
        }

        parse_errs
    });

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, source_name.to_string(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((source_name.to_string(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                // .with_labels(e.contexts().map(|(label, span)| {
                //     Label::new((source_name.clone(), span.into_range()))
                //         .with_message(format!("while parsing this {}", label))
                //         .with_color(Color::Yellow)
                // }))
                .finish()
                .print(sources([(source_name.to_string(), lexer.src.clone())]))
                .unwrap();
        });

    Ok(())
}
