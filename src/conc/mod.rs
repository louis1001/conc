pub mod frontend;

use anyhow::{Result, anyhow, Context};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};
use crate::conc::frontend::StackType;

use std::collections::{HashMap, HashSet};
use std::fmt::{Write, Debug};

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Add,
    Sub,
    Mul,
    Mod,

    Print,
    PrintChar,
    
    LessThan,
    GreaterThan,
    Equals,
    NotEquals,
    Not,
    Or,

    Drop,
    Dup,
    Rot,
    Swap,
    Over,
    Debug,

    Break
}

#[derive(Debug, Clone)]
pub enum ParseTree {
    PushInt(u64),
    PushString(String),
    PushBool(bool),
    PushChar(u8),
    Intrinsic(Intrinsic),

    If(Vec<ParseTree>, Option<Vec<ParseTree>>),

    While(Vec<ParseTree>, Vec<ParseTree>),
    Loop(Vec<ParseTree>),

    Struct(String, Vec<(String, StackType)>),
    ConstructStruct(String),
    MemberAccess(String),

    FunctionCall(String),
    Function(String, Vec<StackType>, Vec<StackType>, Vec<ParseTree>)
}

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
struct UserDefinedStruct {
    name: String,
    members: Vec<(String, StackType)>
}

#[derive(Clone)]
struct UserDefinedFunction {
    name: String,
    input: Vec<StackType>,
    output: Vec<StackType>
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
}

pub struct SymbolResolver<'a> {
    unresolved_context: &'a SemanticContext,
    resolved_context: ResolvedSemanticContext,

    currently_resolving: HashSet<String>,
}

impl<'a> SymbolResolver<'a> {
    fn new(unresolved_context: &'a SemanticContext) -> SymbolResolver {
        SymbolResolver {
            unresolved_context,
            resolved_context: ResolvedSemanticContext::new(),
            currently_resolving: HashSet::new()
        }
    }

    fn resolve_type(&mut self, stack_type: StackType) -> Result<StackPoint> {
        let value = match stack_type {
            StackType::Bool => StackPoint::Bool,
            StackType::U8 => StackPoint::U8,
            StackType::U64 => StackPoint::U64,
            StackType::Ptr => StackPoint::Ptr,
            StackType::Custom(name) => {
                if self.currently_resolving.contains(&name) {
                    return Err(anyhow!("Found a recursive dependency on struct {}", name));
                }

                let resolved = self.resolve_struct(&name)
                    .context(format!("Resolving struct {name}"))?
                    .ok_or(anyhow!("Type {name} is unknown"))?;

                StackPoint::Struct(resolved.name.clone())
            }
        };

        Ok(value)
    }

    fn resolve_struct(&mut self, name: &str) -> Result<Option<ResolvedStruct>> {
        if let Some(rs) = self.resolved_context.structs.get(name) {
            return Ok(Some(rs.clone()));
        }

        self.currently_resolving.insert(name.to_string());

        let s = match self.unresolved_context.structs.get(name) {
            Some(s) => s.clone(),
            None => return Ok(None)
        };

        let members: Result<_> = s.members.clone()
            .into_iter()
            .map(|(n, x)| {
                match self.resolve_type(x) {
                    Ok(t) => Ok((n, t)),
                    Err(e) => Err(e)
                }
            })
            .collect();

        let resolved_struct = ResolvedStruct {
            name: s.name.clone(),
            members: members?
        };

        self.resolved_context.structs.insert(name.to_string(), resolved_struct.clone());

        self.currently_resolving.remove(name);

        Ok(Some(resolved_struct.clone()))
    }

    fn resolve_context(&mut self) -> Result<&ResolvedSemanticContext> {
        for (name, f) in &self.unresolved_context.functions {
            let input: Result<_> = f.input.clone().into_iter()
                .map(|x| self.resolve_type(x)).collect();
            let output: Result<_> = f.output.clone().into_iter()
                .map(|x| self.resolve_type(x)).collect();
            let resolved_function = ResolvedFunction {
                name: f.name.clone(),
                input: input?,
                output: output?,
            };

            self.resolved_context.functions.insert(name.clone(), resolved_function);
        }

        let mut structs = HashMap::new();

        for name in self.unresolved_context.structs.keys() {
            let resolved_struct = self.resolve_struct(name)?
                .expect("Already iterating over valid keys");

            structs.insert(name.clone(), resolved_struct.clone());
        }

        Ok(&self.resolved_context)
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

        Ok(resolver.resolved_context)
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

#[derive(Clone)]
pub struct ResolvedFunction {
    name: String,
    input: Vec<StackPoint>,
    output: Vec<StackPoint>
}

impl ResolvedFunction {
    fn label_name(&self) -> Result<String> {
        let mut result = String::new();

        result.push_str("fn_");
        result.push_str(&self.name);
        result.push_str("_");
        for input in self.input.iter() {
            write!(result, "{input:#?}")?;
        }

        result.push_str("$");
        for output in self.output.iter() {
            write!(result, "{output:#?}")?;
        }

        return Ok(result);
    }
}

#[derive(Clone)]
pub struct ResolvedStruct {
    name: String,
    members: Vec<(String, StackPoint)>
}

#[derive(Clone)]
pub struct ResolvedSemanticContext {
    functions: HashMap<String, ResolvedFunction>,
    structs: HashMap<String, ResolvedStruct>
}

impl ResolvedSemanticContext {
    fn new() -> Self {
        Self { functions: HashMap::new(), structs: HashMap::new() }
    }
}

#[derive(Clone, PartialEq)]
pub enum StackPoint {
    Bool,
    U8,
    U64,
    Ptr,
    Struct(String)
}

impl std::fmt::Debug for StackPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::U8 => write!(f, "u8"),
            Self::U64 => write!(f, "u64"),
            Self::Ptr => write!(f, "ptr"),
            Self::Struct(name) => write!(f, "@{name}"),
        }
    }
}

impl StackPoint {
    fn size(&self, context: &ResolvedSemanticContext) -> Result<usize> {
        let v = match self {
            StackPoint::U8 => 1,
            StackPoint::U64 => 8,
            StackPoint::Bool => 1,
            StackPoint::Ptr => 8,
            StackPoint::Struct(name) => {
                let st = context.structs.get(name)
                    .ok_or(anyhow!("Failed to resolve size of struct {name}. Doesn't exist in context"))?;

                let mut sz = 0usize;

                for (_, member_size) in &st.members {
                    sz += member_size.size(context)?;
                }

                sz
            }
        };

        Ok(v)
    }
}

impl std::fmt::Display for StackPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackPoint::Bool => f.write_str("bool"),
            StackPoint::U8 => f.write_str("u8"),
            StackPoint::U64 => f.write_str("u64"),
            StackPoint::Ptr => f.write_str("ptr"),
            StackPoint::Struct(name) => f.write_str(name)
        }
    }
}

#[derive(Debug)]
pub enum TypecheckedTree {
    PushInt(u64),
    PushString(String),
    PushBool(bool),
    PushChar(u8),
    Intrinsic(Intrinsic),
    SizedDrop(usize),
    SizedDup(usize),

    If(Vec<TypecheckedTree>, Option<Vec<TypecheckedTree>>),

    While(Vec<TypecheckedTree>, Vec<TypecheckedTree>),
    Loop(Vec<TypecheckedTree>),

    MemberAccess{struct_size: usize, member_offset: usize, member_type: StackPoint},

    FunctionCall(String),
    Function(String, Vec<TypecheckedTree>),

    NoOp
}

pub struct Typechecker {
    stack_tracker: Vec<StackPoint>,
    semantic_context: ResolvedSemanticContext
}

impl Typechecker {
    pub fn new(semantic_context: ResolvedSemanticContext) -> Self {
        Typechecker {
            stack_tracker: vec![],
            semantic_context
        }
    }

    fn typecheck_action(&mut self, action: &SemanticTree) -> Result<TypecheckedTree> {
        let node = match action {
            SemanticTree::PushInt(n) => {
                self.stack_tracker.push(StackPoint::U64);
                TypecheckedTree::PushInt(*n)
            },
            SemanticTree::PushString(val) => {
                self.stack_tracker.push(StackPoint::Ptr);
                self.stack_tracker.push(StackPoint::U64);

                TypecheckedTree::PushString(val.clone())
            },
            SemanticTree::PushBool(val) => {
                self.stack_tracker.push(StackPoint::Bool);

                TypecheckedTree::PushBool(*val)
            },
            SemanticTree::PushChar(c) => {
                self.stack_tracker.push(StackPoint::U8);

                TypecheckedTree::PushChar(*c)
            }
            SemanticTree::Intrinsic(intr) => {
                match intr {
                    Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul | Intrinsic::Mod => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::U64);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic {intr:?} needs 2 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Equals | Intrinsic::NotEquals => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::Bool);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic {intr:?} needs 2 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Not => {
                        let a = self.stack_tracker.pop();

                        match a {
                            Some(StackPoint::Bool) => {
                                self.stack_tracker.push(StackPoint::Bool);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic Not needs 2 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Or => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::Bool), Some(StackPoint::Bool)) => {
                                self.stack_tracker.push(StackPoint::Bool);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic {intr:?} needs 2 bool values on the stack"));
                            }
                        }
                    },
                    Intrinsic::LessThan | Intrinsic::GreaterThan => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::Bool);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic {intr:?} needs 2 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Drop => {
                        match self.stack_tracker.pop() {
                            Some(point) => {
                                let size = point.size(&self.semantic_context)?;
                                
                                if size == std::mem::size_of::<u64>() {
                                    return Ok(TypecheckedTree::Intrinsic(Intrinsic::Drop));
                                } else {
                                    return Ok(TypecheckedTree::SizedDrop(size));
                                }
                            },
                            _ => {
                                return Err(anyhow!("Intrinsic Drop needs a value on the stack"));
                            }
                        }
                    },
                    Intrinsic::Dup => {
                        match self.stack_tracker.pop() {
                            Some(point) => {
                                self.stack_tracker.push(point.clone());
                                self.stack_tracker.push(point.clone());

                                let size = point.size(&self.semantic_context)?;
                                
                                if size == std::mem::size_of::<u64>() {
                                    return Ok(TypecheckedTree::Intrinsic(Intrinsic::Drop));
                                } else {
                                    return Ok(TypecheckedTree::SizedDup(size));
                                }
                            },
                            _ => {
                                return Err(anyhow!("Intrinsic Dup needs a value on the stack"));
                            }
                        }
                    }
                    Intrinsic::Rot => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();
                        let c = self.stack_tracker.pop();

                        match (a, b, c) {
                            (Some(StackPoint::U64), Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::U64);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic Rot needs 3 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Swap => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::U64);
                            }
                            (Some(StackPoint::Ptr), Some(StackPoint::Ptr)) => {
                                self.stack_tracker.push(StackPoint::Ptr);
                                self.stack_tracker.push(StackPoint::Ptr);
                            }
                            (Some(StackPoint::Ptr), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::Ptr);
                                self.stack_tracker.push(StackPoint::U64);
                            }
                            (Some(StackPoint::U64), Some(StackPoint::Ptr)) => {
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::Ptr);
                            }
                            _ => {
                                println!("Current stack: {:?}", self.stack_tracker);
                                return Err(anyhow!("Intrinsic {intr:?} needs 2 u64/ptr values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Over => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::U64);
                                self.stack_tracker.push(StackPoint::U64);
                            }
                            _ => {
                                return Err(anyhow!("Intrinsic Over needs 2 u64 values on the stack"));
                            }
                        }
                    },
                    Intrinsic::Debug => {
                        let a = self.stack_tracker.pop();

                        match a {
                            Some(StackPoint::U64) => {}
                            _ => {
                                return Err(anyhow!("Intrinsic Debug needs a u64 value on the stack"));
                            }
                        }
                    }
                    Intrinsic::Print => {
                        let a = self.stack_tracker.pop();
                        let b = self.stack_tracker.pop();

                        match (a, b) {
                            (Some(StackPoint::U64), Some(StackPoint::Ptr)) => {}
                            _ => {
                                return Err(anyhow!("Intrinsic Print needs a string length (u64) and a pointer (&str) value on the stack"));
                            }
                        }
                    }
                    Intrinsic::PrintChar => {
                        let a = self.stack_tracker.pop();

                        match a {
                            Some(StackPoint::U8) => {}
                            _ => {
                                return Err(anyhow!("Intrinsic PrintChar needs a char value on the stack"));
                            }
                        }
                    }

                    Intrinsic::Break => {}
                }

                TypecheckedTree::Intrinsic(intr.clone())
            }
            SemanticTree::Loop(contents) => {
                let (result, _) = self.typecheck_subblock(&contents, Some(&[]))?;

                TypecheckedTree::Loop(result)
            }
            SemanticTree::While(condition, body) => {
                let mut stack_before = self.stack_tracker.clone();
                let mut cond_nodes = vec![];
                for action in condition {
                    cond_nodes.push(self.typecheck_action(action)?);
                }

                stack_before.push(StackPoint::Bool);
                if stack_before != self.stack_tracker {
                    return Err(anyhow!("Condition for while statement must leave one Bool in the stack."));
                }

                let _ = self.stack_tracker.pop();

                let expected_output = self.stack_tracker.clone();

                let (nodes, _) = self.typecheck_subblock(&body, Some(&expected_output))?;

                TypecheckedTree::While(cond_nodes, nodes)
            }
            SemanticTree::If(contents, else_contents) => {
                let condition = self.stack_tracker.pop();
                match condition {
                    Some(StackPoint::Bool) => {},
                    _ => {
                        return Err(anyhow!("If statement needs a bool condition on the stack"));
                    }
                }
                let (branch_nodes, branch_output) = self.typecheck_subblock(&contents, None)?;
                let else_branch_nodes;
                if let Some(else_contents) = else_contents {
                    let (nodes, _) = self.typecheck_subblock(&else_contents, Some(&branch_output))?;
                    else_branch_nodes = Some(nodes);
                } else {
                    else_branch_nodes = None;
                }

                TypecheckedTree::If(branch_nodes, else_branch_nodes)
            }
            SemanticTree::ConstructStruct(name) => {
                let defined_struct = self.semantic_context.structs.get(name)
                    .ok_or(anyhow!("Constructing unknown struct {name}."))?;

                let mut members = defined_struct.members.clone();
                members.reverse();

                for expected_member in &members {
                    let stack = self.stack_tracker.pop()
                        .ok_or(anyhow!("Function call to {name} needed {expected_member:?} on the stack but found nothing"))?;

                    if stack != expected_member.1 {
                        return Err(anyhow!("Function call to {name} needed {expected_member:?} on the stack but found {stack:?}"));
                    }
                }

                self.stack_tracker.push(StackPoint::Struct(name.clone()));

                TypecheckedTree::NoOp
            }
            SemanticTree::MemberAccess(name) => {
                let tp = self.stack_tracker.last()
                    .ok_or(anyhow!("Expected to have a value in the stack to access `{name}`"))?.clone();

                match tp {
                    StackPoint::Struct(struct_name) => {
                        let members = &self.semantic_context.structs.get(&struct_name)
                            .ok_or(anyhow!("[UNREACHABLE] Accessing unknown struct {struct_name:?}"))?
                            .members;

                        let mut found = false;
                        let mut member_offset = 0usize;
                        let mut struct_size = 0usize;
                        let mut member_type = StackPoint::Bool;
                        for (member_name, point) in members {
                            let size = point.size(&self.semantic_context)?;
                            if !found && name.as_str() == member_name {
                                found = true;
                                member_offset = struct_size;
                                member_type = point.clone();
                            }
                            
                            struct_size += size;
                        }

                        if !found {
                            return Err(anyhow!("Invalid MemberAccess in struct {name}: Member {name} not found"));
                        }

                        self.stack_tracker.push(member_type.clone());

                        TypecheckedTree::MemberAccess { struct_size, member_offset, member_type }
                    }
                    _ => {
                        return Err(anyhow!("Intrinsic MemberAccess needs a struct on the stack, but found {tp:?}"));
                    }
                }
            }
            SemanticTree::FunctionCall(name) => {
                let function = self.semantic_context.functions.get(name)
                    .ok_or(anyhow!("Call to unknown function {name}."))?;

                let mut input = function.input.clone();
                input.reverse();

                for expected_input in &input {
                    let stack = self.stack_tracker.pop()
                        .ok_or(anyhow!("Function call to {name} needed {expected_input:?} on the stack but found nothing"))?;

                    if stack != *expected_input {
                        return Err(anyhow!("Function call to {name} needed {expected_input:?} on the stack but found {stack:?}"));
                    }
                }
                
                for output in &function.output {
                    self.stack_tracker.push(output.clone());
                }

                TypecheckedTree::FunctionCall(name.clone())
            }
            SemanticTree::Function(name, body) => {
                let function = self.semantic_context.functions.get(name)
                    .expect(format!("Function {name} should have been semantically analyzed").as_str())
                    .clone();
                
                let (body, _) = self.typecheck_function(&body, &function.input, &function.output).context(format!("Typechecking function {name} failed"))?;

                TypecheckedTree::Function(name.clone(), body)
            }
        };

        Ok(node)
    }

    pub fn typecheck_scope(&mut self, actions: &[SemanticTree], output: Option<&[StackPoint]>) -> Result<(Vec<TypecheckedTree>, Vec<StackPoint>)> {
        let mut nodes = vec![];
        for action in actions.into_iter() {
            let node = self.typecheck_action(action)?;

            nodes.push(node);
        }

        if let Some(expected_output) = output {
            if self.stack_tracker != expected_output {
                return Err(anyhow!("Invalid state at the end of a scope.\nExpected: {:?}\nbut found: {:?}", expected_output, self.stack_tracker));
            }
        }

        Ok((nodes, self.stack_tracker.clone()))
    }

    fn typecheck_subblock(&mut self, actions: &[SemanticTree], output: Option<&[StackPoint]>) -> Result<(Vec<TypecheckedTree>, Vec<StackPoint>)> {
        let prev_stack = self.stack_tracker.clone();
        let mut subchecker = self.subchecker(prev_stack)?;

        let result = subchecker.typecheck_scope(actions, output).context("Subblock typecheck failed")?;
        if output.is_some() {
            self.stack_tracker = result.1.clone();
        }

        Ok(result)
    }

    fn typecheck_function(&mut self, body: &[SemanticTree], input: &[StackPoint], output: &[StackPoint]) -> Result<(Vec<TypecheckedTree>, Vec<StackPoint>)> {
        let mut subchecker = self.subchecker(input.to_owned())?;

        let result = subchecker.typecheck_scope(body, Some(output))?;

        Ok((result.0, vec![]))
    }

    fn subchecker(&self, input: Vec<StackPoint>) -> Result<Typechecker> {
        let mut new_checker = Typechecker::new(self.semantic_context.clone());
        new_checker.stack_tracker = input;

        Ok(new_checker)
    }
}

pub struct Codegen {
    label_count: usize,
    pb: BytecodeBuilder,
    semantic_context: ResolvedSemanticContext
}

impl Codegen {
    pub fn new(semantic_context: ResolvedSemanticContext) -> Self {
        Codegen { label_count: 0, pb: BytecodeBuilder::new(), semantic_context }
    }

    pub fn generate_program(&mut self, actions: &[TypecheckedTree]) -> Result<()> {
        if let Some(main_func) = self.semantic_context.functions.get("main") {
            self.pb.emit(bytecode::Instruction::PushLabel(main_func.label_name()?));
            self.pb.emit_instruction(bytecode::Opcode::Cll);
            self.pb.emit_instruction(bytecode::Opcode::Ext);
        }

        self.generate_scope(actions)
    }

    pub fn generate_scope(&mut self, actions: &[TypecheckedTree]) -> Result<()> {
        for action in actions.into_iter() {
            match action {
                TypecheckedTree::NoOp => { /* Emit NoOp?? */ },
                TypecheckedTree::PushInt(val) => self.pb.emit(bytecode::Instruction::Push(*val)),
                TypecheckedTree::PushString(val) => {
                    self.pb.emit(bytecode::Instruction::String(val.clone()));
                }
                TypecheckedTree::PushBool(val) => self.pb.emit(bytecode::Instruction::PushByte(*val as u8)),
                TypecheckedTree::PushChar(c) => {
                    self.pb.emit(bytecode::Instruction::PushByte(*c));
                }
                TypecheckedTree::SizedDrop(size) => {
                    self.pb.emit(bytecode::Instruction::DropBytes(*size as u64));
                }
                TypecheckedTree::SizedDup(size) => {
                    self.pb.emit(bytecode::Instruction::DupBytes { offset: *size as u64, n: *size as u64 });
                }
                TypecheckedTree::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => self.pb.emit_instruction(bytecode::Opcode::Add),
                        Intrinsic::Sub => self.pb.emit_instruction(bytecode::Opcode::Sub),
                        Intrinsic::Mul => self.pb.emit_instruction(bytecode::Opcode::Mul),
                        Intrinsic::Mod => self.pb.emit_instruction(bytecode::Opcode::Mod),
                        Intrinsic::LessThan => self.pb.emit_instruction(bytecode::Opcode::Lt),
                        Intrinsic::GreaterThan => self.pb.emit_instruction(bytecode::Opcode::Gt),
                        Intrinsic::Equals => self.pb.emit_instruction(bytecode::Opcode::Equ),
                        Intrinsic::NotEquals => {
                            self.pb.emit_instruction(bytecode::Opcode::Equ);
                            self.pb.emit_instruction(bytecode::Opcode::Not);
                        },
                        Intrinsic::Not => self.pb.emit_instruction(bytecode::Opcode::Not),
                        Intrinsic::Or => self.pb.emit_instruction(bytecode::Opcode::Or),
                        Intrinsic::Drop => self.pb.emit_instruction(bytecode::Opcode::Drp),
                        Intrinsic::Dup => self.pb.emit_instruction(bytecode::Opcode::Dup),
                        Intrinsic::Rot => self.pb.emit_instruction(bytecode::Opcode::Rot),
                        Intrinsic::Swap => self.pb.emit_instruction(bytecode::Opcode::Swp),
                        Intrinsic::Over => self.pb.emit_instruction(bytecode::Opcode::Ovr),
                        Intrinsic::Debug => self.pb.emit_instruction(bytecode::Opcode::Dbg),
                        Intrinsic::Print => self.pb.emit_instruction(bytecode::Opcode::Pts),
                        Intrinsic::PrintChar => self.pb.emit_instruction(bytecode::Opcode::Ptc),
                        Intrinsic::Break => self.pb.emit_instruction(bytecode::Opcode::Bkp),
                    }
                }
                TypecheckedTree::Loop(contents) => {
                    let loop_label = self.pb.create_label(&format!("loop_label_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.link_label(&loop_label)?;
                    
                    self.generate_scope(&contents)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(loop_label));
                    self.pb.emit_instruction(bytecode::Opcode::Jmp);
                }
                TypecheckedTree::If(contents, else_contents) => {
                    let else_label = self.pb.create_label(&format!("if_else_label_{}", self.label_count));
                    self.label_count += 1;

                    let end_label = self.pb.create_label(&format!("if_end_label_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.emit(bytecode::Instruction::PushLabel(else_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jpf);
                    self.generate_scope(&contents)?;

                    if let Some(else_contents) = else_contents {
                        self.pb.emit(bytecode::Instruction::PushLabel(end_label.clone()));
                        self.pb.emit_instruction(bytecode::Opcode::Jmp);

                        self.pb.link_label(&else_label)?;
                        self.generate_scope(&else_contents)?;
                    } else {
                        self.pb.link_label(&else_label)?;
                    }

                    self.pb.link_label(&end_label)?;
                },
                TypecheckedTree::While(condition, body) => {
                    let go_back_label = self.pb.create_label(&format!("go_back_while_{}", self.label_count));
                    self.label_count += 1;

                    let break_label = self.pb.create_label(&format!("break_while_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.link_label(&go_back_label)?;
                    self.generate_scope(&condition)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(break_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jpf);

                    self.generate_scope(&body)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(go_back_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jmp);

                    self.pb.link_label(&break_label)?;
                }
                TypecheckedTree::MemberAccess { struct_size, member_offset, member_type } => {

                    let struct_size = *struct_size;
                    let member_offset = *member_offset;
                    let member_size = member_type.size(&self.semantic_context)?;

                    self.pb.emit(bytecode::Instruction::DupBytes { offset: (struct_size - member_offset) as u64, n: member_size as u64 });
                },
                TypecheckedTree::FunctionCall(name) => {
                    let function = self.semantic_context.functions.get(name)
                        .expect(format!("Function {name} should have been semantically analyzed").as_str());

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.emit(bytecode::Instruction::PushLabel(fn_label));
                    self.pb.emit_instruction(bytecode::Opcode::Cll);
                }
                TypecheckedTree::Function(name, body) => {
                    let function = self.semantic_context.functions.get(name).expect(format!("Function {name} should have been semantically analyzed").as_str()).clone();

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.link_label(&fn_label)?;
                    
                    let args_size = function.input.iter().map(|x| x.size(&self.semantic_context))
                        .reduce(|acc, e| {
                            match (acc, e) {
                                (Ok(a), Ok(e)) => {
                                    Ok(a + e)
                                }
                                (Err(e), _) | (_, Err(e)) => {
                                    Err(e)
                                }
                            }
                        })
                        .unwrap_or(Ok(0))?;

                    if args_size > 0 {
                        self.pb.emit(bytecode::Instruction::Push(args_size as u64));
                        self.pb.emit_instruction(bytecode::Opcode::Tks);
                    }

                    self.generate_scope(&body)?;

                    self.pb.emit_instruction(bytecode::Opcode::Ret);
                }
            }
        }

        Ok(())
    }

    pub fn get_program(&self) -> Result<bytecode::Program> {
        self.pb.to_program()
    }
}
