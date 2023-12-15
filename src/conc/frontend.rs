use anyhow::{Result, anyhow};
use pest::Parser as PestParser;
use pest_derive::Parser as PestParser;

use crate::conc::{Action, Intrinsic};

#[derive(PestParser)]
#[grammar = "conc/grammar.pest"]
struct Parser;

pub fn parse_conc(code: &str) -> Result<Vec<Action>> {
    let mut conc_rules = Parser::parse(Rule::program, code)?;

    use pest::iterators::{Pair, Pairs};

    fn parse_statement_list(pair: Pairs<Rule>) -> Result<Vec<Action>> {
        let mut result = vec![];

        let mut rules = pair;

        while let Some(rule) = rules.next() {
            result.push(parse_value(rule)?);
        }

        Ok(result)
    }

    fn parse_string(pair: Pairs<Rule>) -> Result<Action> {
        let mut result = String::new();

        let mut rules = pair;

        while let Some(pair) = rules.next() {
            let rule = pair.as_rule();

            match rule {
                Rule::raw_string => result.push_str(pair.as_str()),
                Rule::escape_target => {
                    let s = pair.as_str();
                    let char = s.chars().next()
                        .ok_or(anyhow!("Expected to have a character to escape"))?;

                    println!("Escaping in `{s}` char: `{char}`");

                    let escaped = match char {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\"' => '\"',
                        '\'' => '\'',
                        _ => unimplemented!()
                    };

                    result.push(escaped);
                },
                _ => unreachable!()
            }
        }

        Ok(Action::PushString(result))
    }

    fn parse_value(pair: Pair<Rule>) -> Result<Action> {
        match pair.as_rule() {
            Rule::r#true => Ok(Action::PushBool(true)),
            Rule::r#false => Ok(Action::PushBool(false)),
            Rule::add => Ok(Action::Intrinsic(Intrinsic::Add)),
            Rule::sub => Ok(Action::Intrinsic(Intrinsic::Sub)),
            Rule::mul => Ok(Action::Intrinsic(Intrinsic::Mul)),
            Rule::r#mod => Ok(Action::Intrinsic(Intrinsic::Mod)),
            Rule::equals => Ok(Action::Intrinsic(Intrinsic::Equals)),
            Rule::not_equals => Ok(Action::Intrinsic(Intrinsic::NotEquals)),
            Rule::less_than => Ok(Action::Intrinsic(Intrinsic::LessThan)),
            Rule::or => Ok(Action::Intrinsic(Intrinsic::Or)),
            Rule::drop => Ok(Action::Intrinsic(Intrinsic::Drop)),
            Rule::debug => Ok(Action::Intrinsic(Intrinsic::Debug)),
            Rule::dup => Ok(Action::Intrinsic(Intrinsic::Dup)),
            Rule::rot => Ok(Action::Intrinsic(Intrinsic::Rot)),
            Rule::over => Ok(Action::Intrinsic(Intrinsic::Over)),
            Rule::puts => Ok(Action::Intrinsic(Intrinsic::Print)),
            Rule::putc => Ok(Action::Intrinsic(Intrinsic::PrintChar)),
            Rule::r#break => Ok(Action::Intrinsic(Intrinsic::Break)),
            Rule::integer => Ok(Action::PushInt(pair.as_str().parse()?)),
            // FIXME: Do correct parsing of the string
            Rule::string => parse_string(pair.into_inner()),
            Rule::character => {
                let rule = pair.clone().into_inner().next()
                .ok_or(anyhow!("Expected a character"))?
                .as_rule();

                match &rule {
                    Rule::raw_character => {
                        let s = pair.as_str();
                        let char = s.bytes().next()
                            .ok_or(anyhow!("Expected to have a character"))?;

                        Ok(Action::PushChar(char))
                    },
                    Rule::escape_target => {
                        let s = pair.into_inner().next().ok_or(anyhow!("Expected character internals"))?.as_str();
                        let char = s.chars().next()
                            .ok_or(anyhow!("Expected to have a character"))?;
    
                        let escaped = match char {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\"' => '\"',
                            '\'' => '\'',
                            _ => unimplemented!("No escape code for `{char}`")
                        };
                        let mut encoded = [0; 1];
                        escaped.encode_utf8(&mut encoded);

                        println!("`{char}`: {encoded:?}");
                        Ok(Action::PushChar(encoded[0]))
                    },
                    _ => unreachable!()
                }
            }
            Rule::loop_st => {
                let value = pair.into_inner().next()
                    .ok_or(anyhow!("Expected a statement list inside the loop"))?
                    .into_inner();

                let statements = parse_statement_list(value)?;

                Ok(Action::Loop(statements))
            }
            Rule::if_st => {
                let mut pairs = pair.into_inner();
                let value = pairs.next()
                    .ok_or(anyhow!("Expected a statement list inside the loop"))?
                    .into_inner();

                let statements = parse_statement_list(value)?;

                let else_values = match pairs.next() {
                    Some(val) => Some(parse_statement_list(val.into_inner())?),
                    None => None
                };

                Ok(Action::If(statements, else_values))
            }
            Rule::while_st => {
                let mut pairs = pair.into_inner();
                let value = pairs.next()
                    .ok_or(anyhow!("Expected a condition for while statement"))?
                    .into_inner();

                let condition = parse_statement_list(value)?;

                let statements = pairs.next()
                    .ok_or(anyhow!("Expected a statement list inside while statement"))?
                    .into_inner();

                let body = parse_statement_list(statements)?;

                Ok(Action::While(condition, body))
            }
              Rule::program
            | Rule::statement
            | Rule::intrinsic
            | Rule::else_st
            | Rule::EOI
            | Rule::BLOCK_TERMINATOR
            | Rule::WHITESPACE 
            | Rule::COMMENT
            | Rule::raw_string
            | Rule::escaped_character
            | Rule::escape_target
            | Rule::string_internals
            | Rule::character_internals
            | Rule::raw_character
            | Rule::statement_list => {
                println!("{pair:?}");
                unreachable!()
            }
        }
    }

    let result = parse_statement_list(
        conc_rules.next().ok_or(anyhow!("Expected a statement list"))?.into_inner()
    )?;

    Ok(result)
}