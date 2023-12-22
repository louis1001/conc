use crate::conc::frontend::StackType;
use crate::conc::frontend::{Intrinsic, ParseTree};
use crate::conc::symbol_resolver::{SymbolResolver, ResolvedSemanticContext};

use std::collections::HashMap;

use anyhow::{Result, anyhow};

impl ParseTree {
    fn internals_to_semantic(&self) -> Result<SemanticTree> {
        let node = match self {
            ParseTree::PushInt(v) => SemanticTree::PushInt(*v),
            ParseTree::PushString(v) => SemanticTree::PushString(v.clone()),
            ParseTree::PushBool(v) => SemanticTree::PushBool(*v),
            ParseTree::PushChar(v) => SemanticTree::PushChar(*v),
            ParseTree::Intrinsic(v) => SemanticTree::Intrinsic(v.clone()),
            ParseTree::If(body, else_body) => {
                let body = body.into_iter().map(|x| x.internals_to_semantic()).collect::<Result<_>>()?;
                let else_body = match else_body {
                    Some(else_body) => {
                        let statements = else_body.into_iter().map(|x| x.internals_to_semantic());

                        let statements = statements.collect::<Result<_>>()?;

                        Some(statements)
                    },
                    None => None
                };
                
                SemanticTree::If(body, else_body)
            }
            ParseTree::While(cond, body) => {
                let cond = cond.iter().map(|x| x.internals_to_semantic()).into_iter().collect::<Result<_>>()?;
                let body = body.iter().map(|x| x.internals_to_semantic()).into_iter().collect::<Result<_>>()?;
                
                SemanticTree::While(cond, body)
            },
            ParseTree::Loop(body) => {
                SemanticTree::Loop(body.iter().map(|x| x.internals_to_semantic()).into_iter().collect::<Result<_>>()?)
            }
            ParseTree::Struct(_, _) => return Err(anyhow!("Invalid struct declaration in non global environment")),
            ParseTree::ConstructStruct(name) => SemanticTree::ConstructStruct(name.to_string()),
            ParseTree::MemberAccess(name) => SemanticTree::MemberAccess(name.to_string()),
            ParseTree::Function(_, _, _, _) => return Err(anyhow!("Invalid function declaration in non global environment")),
            ParseTree::FunctionCall(name) => SemanticTree::FunctionCall(name.to_string())
        };

        Ok(node)
    }
}

#[derive(Debug)]
pub enum SemanticTree {
    PushInt(u64),
    PushString(String),
    PushBool(bool),
    PushChar(u8),
    Intrinsic(Intrinsic),

    If(Vec<SemanticTree>, Option<Vec<SemanticTree>>),

    While(Vec<SemanticTree>, Vec<SemanticTree>),
    Loop(Vec<SemanticTree>),

    ConstructStruct(String),
    MemberAccess(String),

    FunctionCall(String),
    Function(String, Vec<SemanticTree>)
}

#[derive(Clone, Debug)]
pub struct UserDefinedStruct {
    name: String,
    members: Vec<(String, StackType)>
}

impl UserDefinedStruct {
    pub fn name<'a>(&'a self) -> &'a String {
        &self.name
    }

    pub fn members<'a>(&'a self) -> &'a Vec<(String, StackType)> {
        &self.members
    }
}

#[derive(Clone)]
pub struct UserDefinedFunction {
    name: String,
    input: Vec<StackType>,
    output: Vec<StackType>
}

impl UserDefinedFunction {
    pub fn name<'a>(&'a self) -> &'a String {
        &self.name
    }

    pub fn input<'a>(&'a self) -> &'a Vec<StackType> {
        &self.input
    }

    pub fn output<'a>(&'a self) -> &'a Vec<StackType> {
        &self.output
    }
}

#[derive(Clone)]
pub struct SemanticContext {
    structs: HashMap<String, UserDefinedStruct>,
    functions: HashMap<String, UserDefinedFunction>
}

impl SemanticContext {
    pub fn new() -> SemanticContext {
        SemanticContext { structs: HashMap::new(), functions: HashMap::new() }
    }

    pub fn structs<'a>(&'a self) -> &'a HashMap<String, UserDefinedStruct> {
        &self.structs
    }

    pub fn functions<'a>(&'a self) -> &'a HashMap<String, UserDefinedFunction> {
        &self.functions
    }
}

pub struct SemanticAnalyzer {
    context: SemanticContext
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer{
            context: SemanticContext::new()
        }
    }

    pub fn resolved_context(&self) -> Result<ResolvedSemanticContext> {
        let mut resolver = SymbolResolver::new(&self.context);

        let _ = resolver.resolve_context()?;
        let context = resolver.get_context();

        Ok(context)
    }

    pub fn analyze_program(&mut self, actions: &[ParseTree]) -> Result<Vec<SemanticTree>> {
        let mut result = vec![];

        for action in actions {
            let node = match action {
                ParseTree::Function(name, input_stack, output_stack, body) => {
                    if let Some(_) = self.context.functions.get(name) {
                        return Err(anyhow!("Invalid redeclaration of function {name}"));
                    }

                    let function = UserDefinedFunction {
                        name: name.clone(),
                        input: input_stack.clone(),
                        output: output_stack.clone()
                    };

                    self.context.functions.insert(name.clone(), function);
                    
                    let body = body.clone()
                        .into_iter().map(|x| x.internals_to_semantic())
                        .collect::<Result<_>>()?;

                    SemanticTree::Function(name.clone(), body)
                }
                ParseTree::Struct(name, members) => {
                    if let Some(_) = self.context.structs.get(name) {
                        return Err(anyhow!("Invalid redeclaration of struct {name}"));
                    }

                    let defined_struct = UserDefinedStruct {
                        name: name.clone(),
                        members: members.clone()
                    };
                    
                    self.context.structs.insert(name.clone(), defined_struct);

                    continue; // Don't emit anything. Structs are just conc metadata
                }
                _ => return Err(anyhow!("Invalid statement {action:?} in global scope"))
            };

            result.push(node);
        }

        return Ok(result);
    }
}
