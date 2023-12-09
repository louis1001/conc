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

    fn parse_value(pair: Pair<Rule>) -> Result<Action> {
        match pair.as_rule() {
            Rule::add => Ok(Action::Intrinsic(Intrinsic::Add)),
            Rule::sub => Ok(Action::Intrinsic(Intrinsic::Sub)),
            Rule::less_than => Ok(Action::Intrinsic(Intrinsic::LessThan)),
            Rule::drop => Ok(Action::Intrinsic(Intrinsic::Drop)),
            Rule::pnt => Ok(Action::Intrinsic(Intrinsic::Print)),
            Rule::integer => Ok(Action::PushInt(pair.as_str().parse()?)),
            // FIXME: Do correct parsing of the string
            Rule::string => Ok(Action::PushString(pair.as_str().to_string().trim_matches('"').to_string())),
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
              Rule::program
            | Rule::statement
            | Rule::intrinsic
            | Rule::else_st
            | Rule::EOI
            | Rule::BLOCK_TERMINATOR
            | Rule::WHITESPACE 
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