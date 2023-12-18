use anyhow::{Result, anyhow};
use pest::Parser as PestParser;
use pest_derive::Parser as PestParser;

use crate::conc::{ParseTree, Intrinsic, StackPoint};

#[derive(PestParser)]
#[grammar = "conc/grammar.pest"]
struct Parser;

pub fn parse_conc(code: &str) -> Result<Vec<ParseTree>> {
    let mut conc_rules = Parser::parse(Rule::program, code)?;

    use pest::iterators::{Pair, Pairs};

    fn parse_statement_list(pair: Pairs<Rule>) -> Result<Vec<ParseTree>> {
        let mut result = vec![];

        let mut rules = pair;

        while let Some(rule) = rules.next() {
            result.push(parse_value(rule)?);
        }

        Ok(result)
    }

    fn parse_string(pair: Pairs<Rule>) -> Result<ParseTree> {
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

        Ok(ParseTree::PushString(result))
    }

    fn parse_stack_description(pairs: &mut Pairs<Rule>) -> Result<Vec<StackPoint>> {
        let mut result = vec![];
        while let Some(tp) = pairs.next() {
            let rule = tp.as_rule();

            let point = match rule {
                Rule::builtin_type => {
                    match tp.as_str() {
                        "u64" => StackPoint::U64,
                        "u8" => StackPoint::U8,
                        "bool" => StackPoint::Bool,
                        "ptr" => StackPoint::Ptr,
                        _ => return Err(anyhow!("Unknown type {}", tp.as_str()))
                    }
                }
                _ => unreachable!()
            };

            result.push(point);
        }

        Ok(result)
    }

    fn parse_function(pairs: Pairs<Rule>) -> Result<ParseTree> {
        let mut pairs = pairs;
        let name = pairs.next().ok_or(anyhow!("Expected an identifier"))?.as_str();

        let mut stack_description = pairs.next()
            .ok_or(anyhow!("Expected a stack description"))?
            .into_inner();

        let input_stack = parse_stack_description(&mut stack_description)?;

        let mut next_piece = pairs.next()
            .ok_or(anyhow!("Expected an output stack description or the function body"))?;
        
        let output_stack = if let Rule::stack_descripcion = next_piece.as_rule() {
            let mut stack_description = next_piece.into_inner();

            next_piece = pairs.next().ok_or(anyhow!("Expected a function body"))?;
            
            parse_stack_description(&mut stack_description)?
        } else {
            vec![]
        };

        let body_pair = next_piece;

        let body = parse_statement_list(body_pair.into_inner())?;

        Ok(ParseTree::Function(name.to_string(), input_stack, output_stack, body))
    }

    fn parse_value(pair: Pair<Rule>) -> Result<ParseTree> {
        match pair.as_rule() {
            Rule::r#true => Ok(ParseTree::PushBool(true)),
            Rule::r#false => Ok(ParseTree::PushBool(false)),
            Rule::add => Ok(ParseTree::Intrinsic(Intrinsic::Add)),
            Rule::sub => Ok(ParseTree::Intrinsic(Intrinsic::Sub)),
            Rule::mul => Ok(ParseTree::Intrinsic(Intrinsic::Mul)),
            Rule::r#mod => Ok(ParseTree::Intrinsic(Intrinsic::Mod)),
            Rule::equals => Ok(ParseTree::Intrinsic(Intrinsic::Equals)),
            Rule::not_equals => Ok(ParseTree::Intrinsic(Intrinsic::NotEquals)),
            Rule::less_than => Ok(ParseTree::Intrinsic(Intrinsic::LessThan)),
            Rule::greater_than => Ok(ParseTree::Intrinsic(Intrinsic::GreaterThan)),
            Rule::or => Ok(ParseTree::Intrinsic(Intrinsic::Or)),
            Rule::not => Ok(ParseTree::Intrinsic(Intrinsic::Not)),
            Rule::drop => Ok(ParseTree::Intrinsic(Intrinsic::Drop)),
            Rule::debug => Ok(ParseTree::Intrinsic(Intrinsic::Debug)),
            Rule::dup => Ok(ParseTree::Intrinsic(Intrinsic::Dup)),
            Rule::rot => Ok(ParseTree::Intrinsic(Intrinsic::Rot)),
            Rule::over => Ok(ParseTree::Intrinsic(Intrinsic::Over)),
            Rule::puts => Ok(ParseTree::Intrinsic(Intrinsic::Print)),
            Rule::putc => Ok(ParseTree::Intrinsic(Intrinsic::PrintChar)),
            Rule::r#break => Ok(ParseTree::Intrinsic(Intrinsic::Break)),
            Rule::integer => Ok(ParseTree::PushInt(pair.as_str().parse()?)),
            // FIXME: Do correct parsing of the string
            Rule::string => parse_string(pair.into_inner()),
            Rule::character => {
                let rule = pair.clone().into_inner().next()
                .ok_or(anyhow!("Expected a character"))?
                .as_rule();

                match &rule {
                    Rule::raw_character => {
                        let s = pair.into_inner().next()
                            .ok_or(anyhow!("Expected to have a character"))?
                            .as_str();
                        
                        let char = s.bytes().next()
                            .ok_or(anyhow!("Expected to have a character"))?;

                        Ok(ParseTree::PushChar(char))
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
                        Ok(ParseTree::PushChar(encoded[0]))
                    },
                    _ => unreachable!()
                }
            }
            Rule::loop_st => {
                let value = pair.into_inner().next()
                    .ok_or(anyhow!("Expected a statement list inside the loop"))?
                    .into_inner();

                let statements = parse_statement_list(value)?;

                Ok(ParseTree::Loop(statements))
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

                Ok(ParseTree::If(statements, else_values))
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

                Ok(ParseTree::While(condition, body))
            }
            Rule::function => {
                let pairs = pair.clone().into_inner();
                parse_function(pairs)
            }
            Rule::func_call => {
                let pairs = pair.clone().into_inner();

                let name = pairs.as_str();

                Ok(ParseTree::FunctionCall(name.to_string()))
            }
              Rule::program
            | Rule::statement
            | Rule::identifier
            | Rule::builtin_type
            | Rule::data_type
            | Rule::stack_descripcion
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