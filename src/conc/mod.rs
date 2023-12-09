pub mod frontend;

use anyhow::{Result, anyhow};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};

#[derive(Debug)]
pub enum Intrinsic {
    Add,
    Sub,
    Print,
    LessThan,
    Drop
}

#[derive(Debug)]
pub enum Action {
    PushInt(u64),
    PushString(String),
    Intrinsic(Intrinsic),
    If(Vec<Action>, Option<Vec<Action>>),

    Loop(Vec<Action>)
}

pub struct Codegen {
    label_count: usize,
    pb: BytecodeBuilder
}

impl Codegen {
    pub fn new() -> Self {
        Codegen { label_count: 0, pb: BytecodeBuilder::new() }
    }

    pub fn generate_program(&mut self, actions: &[Action]) -> Result<()> {
        for action in actions.into_iter() {
            match action {
                Action::PushInt(val) => self.pb.emit(bytecode::Instruction::Push(*val)),
                Action::PushString(val) => {
                    self.pb.emit(bytecode::Instruction::String(val.clone()));
                }
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => self.pb.emit_instruction(bytecode::Opcode::Add),
                        Intrinsic::Sub => self.pb.emit_instruction(bytecode::Opcode::Sub),
                        Intrinsic::LessThan => self.pb.emit_instruction(bytecode::Opcode::Lt),
                        Intrinsic::Drop => self.pb.emit_instruction(bytecode::Opcode::Drp),
                        Intrinsic::Print => self.pb.emit_instruction(bytecode::Opcode::Pnt),
                    }
                }
                Action::Loop(contents) => {
                    let loop_label = self.pb.create_label(&format!("loop_label_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.link_label(&loop_label)?;
                    
                    self.generate_program(&contents)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(loop_label));
                    self.pb.emit_instruction(bytecode::Opcode::Jmp);
                }
                Action::If(contents, else_contents) => {
                    let else_label = self.pb.create_label(&format!("if_else_label_{}", self.label_count));
                    self.label_count += 1;

                    let end_label = self.pb.create_label(&format!("if_end_label_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.emit(bytecode::Instruction::PushLabel(else_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jpf);
                    self.generate_program(&contents)?;

                    if let Some(else_contents) = else_contents {
                        self.pb.emit(bytecode::Instruction::PushLabel(end_label.clone()));
                        self.pb.emit_instruction(bytecode::Opcode::Jmp);

                        self.pb.link_label(&else_label)?;
                        self.generate_program(&else_contents)?;
                    } else {
                        self.pb.link_label(&else_label)?;
                    }

                    self.pb.link_label(&end_label)?;
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

pub struct Typechecker {
    stack_tracker: Vec<StackPoint>
}

impl Typechecker {
    pub fn new() -> Self {
        Typechecker {
            stack_tracker: vec![]
        }
    }

    pub fn typecheck_scope(&mut self, actions: &[Action], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
        for action in actions.into_iter() {
            match action {
                Action::PushInt(_) => {
                    self.stack_tracker.push(StackPoint::U64);
                },
                Action::PushString(_) => {
                    self.stack_tracker.push(StackPoint::Ptr);
                    self.stack_tracker.push(StackPoint::U64);
                },
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add | Intrinsic::Sub => {
                            let a = self.stack_tracker.pop();
                            let b = self.stack_tracker.pop();

                            match (a, b) {
                                (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                    self.stack_tracker.push(StackPoint::U64);
                                }
                                _ => {
                                    return Err(anyhow!("Intrinsic Add needs 2 u64 values on the stack"));
                                }
                            }
                        },
                        Intrinsic::LessThan => {
                            let a = self.stack_tracker.pop();
                            let b = self.stack_tracker.pop();

                            match (a, b) {
                                (Some(StackPoint::U64), Some(StackPoint::U64)) => {
                                    self.stack_tracker.push(StackPoint::Bool);
                                }
                                _ => {
                                    return Err(anyhow!("Intrinsic LessThan needs 2 u64 values on the stack"));
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
                    }
                }
                Action::Loop(contents) => {
                    self.typecheck_subblock(&contents, &[], Some(&[]))?;
                }
                Action::If(contents, else_contents) => {
                    let condition = self.stack_tracker.pop();
                    match condition {
                        Some(StackPoint::Bool) => {},
                        _ => {
                            return Err(anyhow!("If statement needs a bool condition on the stack"));
                        }
                    }
                    let branch_output = self.typecheck_subblock(&contents, &[], None)?;
                    if let Some(else_contents) = else_contents {
                        self.typecheck_subblock(&else_contents, &[], Some(&branch_output))?;
                    }
                }
            }
        }

        if let Some(expected_output) = output {
            if self.stack_tracker != expected_output {
                return Err(anyhow!("Invalid state at the end of a scope.\nExpected: {:?}\nbut found: {:?}", expected_output, self.stack_tracker));
            }
        }

        Ok(self.stack_tracker.clone())
    }

    fn typecheck_subblock(&mut self, actions: &[Action], input: &[StackPoint], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
        let mut subchecker = self.subchecker(input)?;

        let result = subchecker.typecheck_scope(actions, output)?;
        if let Some(output) = output {
            self.stack_tracker.extend_from_slice(output);
        }

        Ok(result)
    }

    fn subchecker(&self, input: &[StackPoint]) -> Result<Typechecker> {
        let mut new_checker = Typechecker::new();
        new_checker.stack_tracker.extend_from_slice(input);

        Ok(new_checker)
    }
}