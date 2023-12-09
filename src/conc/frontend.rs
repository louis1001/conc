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
            Rule::integer => Ok(Action::PushInt(pair.as_str().parse()?)),
            Rule::string => todo!(),
            Rule::loop_st => {
                let value = pair.into_inner().next()
                    .ok_or(anyhow!("Expected a statement list inside the loop"))?
                    .into_inner();

                let statements = parse_statement_list(value)?;

                Ok(Action::Loop(statements))
            }
              Rule::program
            | Rule::statement
            | Rule::intrinsic
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