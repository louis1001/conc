pub mod frontend;

use anyhow::{Result, anyhow, Context};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};

use std::collections::HashMap;
use std::fmt::Write;

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

    FunctionCall(String),

    Function(String, Vec<StackPoint>, Vec<StackPoint>, Vec<ParseTree>)
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

    FunctionCall(String),
    Function(String, Vec<StackPoint>, Vec<StackPoint>, Vec<SemanticTree>)
}

#[derive(Clone)]
struct UserDefinedFunction {
    name: String,
    input: Vec<StackPoint>,
    output: Vec<StackPoint>
}

impl UserDefinedFunction {
    fn label_name(&self) -> Result<String> {
        let mut result = String::new();

        result.push_str("fn_");
        result.push_str(&self.name);
        result.push_str("_");
        for input in self.input.iter() {
            write!(result, "{input}")?;
        }

        result.push_str("$");
        for output in self.output.iter() {
            write!(result, "{output}")?;
        }

        return Ok(result);
    }
}

#[derive(Clone)]
pub struct SemanticContext {
    functions: HashMap<String, UserDefinedFunction>
}

impl SemanticContext {
    pub fn new() -> SemanticContext {
        SemanticContext { functions: HashMap::new() }
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

    pub fn get_context(&self) -> &SemanticContext {
        &self.context
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

                    SemanticTree::Function(name.clone(), input_stack.clone(), output_stack.clone(), body)
                }
                _ => return Err(anyhow!("Invalid statement {action:?} in global scope"))
            };

            result.push(node);
        }

        return Ok(result);
    }
}

pub struct Codegen {
    label_count: usize,
    pb: BytecodeBuilder,
    semantic_context: SemanticContext
}

impl Codegen {
    pub fn new(semantic_context: SemanticContext) -> Self {
        Codegen { label_count: 0, pb: BytecodeBuilder::new(), semantic_context }
    }

    pub fn generate_program(&mut self, actions: &[SemanticTree]) -> Result<()> {
        if let Some(main_func) = self.semantic_context.functions.get("main") {
            self.pb.emit(bytecode::Instruction::PushLabel(main_func.label_name()?));
            self.pb.emit_instruction(bytecode::Opcode::Cll);
            self.pb.emit_instruction(bytecode::Opcode::Ext);
        }

        self.generate_scope(actions)
    }

    pub fn generate_scope(&mut self, actions: &[SemanticTree]) -> Result<()> {
        for action in actions.into_iter() {
            match action {
                SemanticTree::PushInt(val) => self.pb.emit(bytecode::Instruction::Push(*val)),
                SemanticTree::PushString(val) => {
                    self.pb.emit(bytecode::Instruction::String(val.clone()));
                }
                SemanticTree::PushBool(val) => self.pb.emit(bytecode::Instruction::PushByte(*val as u8)),
                SemanticTree::PushChar(c) => {
                    self.pb.emit(bytecode::Instruction::PushByte(*c));
                }
                SemanticTree::Intrinsic(intr) => {
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
                        Intrinsic::Over => self.pb.emit_instruction(bytecode::Opcode::Ovr),
                        Intrinsic::Debug => self.pb.emit_instruction(bytecode::Opcode::Dbg),
                        Intrinsic::Print => self.pb.emit_instruction(bytecode::Opcode::Pts),
                        Intrinsic::PrintChar => self.pb.emit_instruction(bytecode::Opcode::Ptc),
                        Intrinsic::Break => self.pb.emit_instruction(bytecode::Opcode::Bkp),
                    }
                }
                SemanticTree::Loop(contents) => {
                    let loop_label = self.pb.create_label(&format!("loop_label_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.link_label(&loop_label)?;
                    
                    self.generate_scope(&contents)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(loop_label));
                    self.pb.emit_instruction(bytecode::Opcode::Jmp);
                }
                SemanticTree::If(contents, else_contents) => {
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
                SemanticTree::While(condition, body) => {
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
                SemanticTree::FunctionCall(name) => {
                    let function = self.semantic_context.functions.get(name)
                        .expect(format!("Function {name} should have been semantically analyzed").as_str());

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.emit(bytecode::Instruction::PushLabel(fn_label));
                    self.pb.emit_instruction(bytecode::Opcode::Cll);
                }
                SemanticTree::Function(name, input_stack, _, body) => {
                    let function = self.semantic_context.functions.get(name).expect(format!("Function {name} should have been semantically analyzed").as_str()).clone();

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.link_label(&fn_label)?;
                    
                    let args_size = input_stack.iter().map(|x| x.size()).reduce(|acc, e| acc + e).unwrap_or(0);

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

#[derive(Debug, Clone, PartialEq)]
pub enum StackPoint {
    Bool,
    U8,
    U64,
    Ptr // This means: bottom [Ptr*, U64] top
}

impl StackPoint {
    fn size(&self) -> usize {
        match self {
            StackPoint::U8 => 1,
            StackPoint::U64 => 8,
            StackPoint::Bool => 1,
            StackPoint::Ptr => 8
        }
    }
}

impl std::fmt::Display for StackPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackPoint::Bool => f.write_str("bool"),
            StackPoint::U8 => f.write_str("u8"),
            StackPoint::U64 => f.write_str("u64"),
            StackPoint::Ptr => f.write_str("ptr"),
        }
    }
}

pub struct Typechecker {
    stack_tracker: Vec<StackPoint>,
    semantic_context: SemanticContext
}

impl Typechecker {
    pub fn new(semantic_context: SemanticContext) -> Self {
        Typechecker {
            stack_tracker: vec![],
            semantic_context
        }
    }

    fn typecheck_action(&mut self, action: &SemanticTree) -> Result<()> {
        match action {
            SemanticTree::PushInt(_) => {
                self.stack_tracker.push(StackPoint::U64);
            },
            SemanticTree::PushString(_) => {
                self.stack_tracker.push(StackPoint::Ptr);
                self.stack_tracker.push(StackPoint::U64);
            },
            SemanticTree::PushBool(_) => {
                self.stack_tracker.push(StackPoint::Bool);
            },
            SemanticTree::PushChar(_) => self.stack_tracker.push(StackPoint::U8),
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
                            Some(_) => {},
                            _ => {
                                return Err(anyhow!("Intrinsic Drop needs a value on the stack"));
                            }
                        }
                    },
                    Intrinsic::Dup => {
                        match self.stack_tracker.pop() {
                            Some(val) => {
                                self.stack_tracker.push(val.clone());
                                self.stack_tracker.push(val);
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
            }
            SemanticTree::Loop(contents) => {
                self.typecheck_subblock(&contents, Some(&[]))?;
            }
            SemanticTree::While(condition, body) => {
                let mut stack_before = self.stack_tracker.clone();
                for action in condition {
                    self.typecheck_action(action)?;
                }

                stack_before.push(StackPoint::Bool);
                if stack_before != self.stack_tracker {
                    return Err(anyhow!("Condition for while statement must leave one Bool in the stack."));
                }

                let _ = self.stack_tracker.pop();

                let expected_output = self.stack_tracker.clone();

                self.typecheck_subblock(&body, Some(&expected_output))?;
            }
            SemanticTree::If(contents, else_contents) => {
                let condition = self.stack_tracker.pop();
                match condition {
                    Some(StackPoint::Bool) => {},
                    _ => {
                        return Err(anyhow!("If statement needs a bool condition on the stack"));
                    }
                }
                let branch_output = self.typecheck_subblock(&contents, None)?;
                if let Some(else_contents) = else_contents {
                    self.typecheck_subblock(&else_contents, Some(&branch_output))?;
                }
            }
            SemanticTree::FunctionCall(name) => {
                let function = self.semantic_context.functions.get(name)
                    .expect(format!("Call to unknown function {name}.").as_str());

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
            }
            SemanticTree::Function(name, input_stack, output_stack, body) => {
                self.typecheck_function(&body, &input_stack, &output_stack).context(format!("Typechecking function {name} failed"))?;
            }
        }

        Ok(())
    }

    pub fn typecheck_scope(&mut self, actions: &[SemanticTree], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
        for action in actions.into_iter() {
            self.typecheck_action(action)?;
        }

        if let Some(expected_output) = output {
            if self.stack_tracker != expected_output {
                return Err(anyhow!("Invalid state at the end of a scope.\nExpected: {:?}\nbut found: {:?}", expected_output, self.stack_tracker));
            }
        }

        Ok(self.stack_tracker.clone())
    }

    fn typecheck_subblock(&mut self, actions: &[SemanticTree], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
        let prev_stack = self.stack_tracker.clone();
        let mut subchecker = self.subchecker(prev_stack)?;

        let result = subchecker.typecheck_scope(actions, output).context("Subblock typecheck failed")?;
        if output.is_some() {
            self.stack_tracker = result.clone();
        }

        Ok(result)
    }

    fn typecheck_function(&mut self, body: &[SemanticTree], input: &[StackPoint], output: &[StackPoint]) -> Result<Vec<StackPoint>> {
        let mut subchecker = self.subchecker(input.to_owned())?;

        let _ = subchecker.typecheck_scope(body, Some(output))?;

        Ok(vec![])
    }

    fn subchecker(&self, input: Vec<StackPoint>) -> Result<Typechecker> {
        let mut new_checker = Typechecker::new(self.semantic_context.clone());
        new_checker.stack_tracker = input;

        Ok(new_checker)
    }
}