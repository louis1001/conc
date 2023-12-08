use anyhow::{Result, anyhow};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};

pub enum Intrinsic {
    Add,
    LessThan
}

pub enum Action {
    PushInt(u64),
    Intrinsic(Intrinsic)
}

pub struct Codegen;

impl Codegen {
    pub fn new() -> Self {
        Codegen {}
    }

    pub fn generate_program(&self, actions: &[Action]) -> Result<bytecode::Program> {
        let mut pb = BytecodeBuilder::new();

        for action in actions.into_iter() {
            match action {
                Action::PushInt(val) => pb.emit(bytecode::Instruction::Push(*val)),
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => pb.emit_instruction(bytecode::Opcode::Add),
                        Intrinsic::LessThan => pb.emit_instruction(bytecode::Opcode::Lt)
                    }
                }
            }
        }

        pb.to_program()
    }
}

enum StackPoint {
    u8,
    u64
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

    pub fn typecheck_program(&mut self, actions: &[Action]) -> Result<()> {
        for action in actions.into_iter() {
            match action {
                Action::PushInt(_) => {
                    self.stack_tracker.push(StackPoint::u64);
                },
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => {
                            let a = self.stack_tracker.pop();
                            let b = self.stack_tracker.pop();

                            match (a, b) {
                                (Some(StackPoint::u64), Some(StackPoint::u64)) => {
                                    self.stack_tracker.push(StackPoint::u64);
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
                                (Some(StackPoint::u64), Some(StackPoint::u64)) => {
                                    self.stack_tracker.push(StackPoint::u8);
                                }
                                _ => {
                                    return Err(anyhow!("Intrinsic LessThan needs 2 u64 values on the stack"));
                                }
                            }
                        },
                    }
                }
            }
        }

        Ok(())
    }
}