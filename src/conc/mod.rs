pub mod frontend;

use anyhow::{Result, anyhow};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};

pub enum Intrinsic {
    Add,
    Sub,
    LessThan,
    Drop
}

pub enum Action {
    PushInt(u64),
    Intrinsic(Intrinsic),

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

    pub fn generate_program(&mut self, actions: &[Action]) -> Result<bytecode::Program> {
        for action in actions.into_iter() {
            match action {
                Action::PushInt(val) => self.pb.emit(bytecode::Instruction::Push(*val)),
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => self.pb.emit_instruction(bytecode::Opcode::Add),
                        Intrinsic::Sub => self.pb.emit_instruction(bytecode::Opcode::Sub),
                        Intrinsic::LessThan => self.pb.emit_instruction(bytecode::Opcode::Lt),
                        Intrinsic::Drop => self.pb.emit_instruction(bytecode::Opcode::Drp)
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
            }
        }

        self.pb.to_program()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum StackPoint {
    U8,
    U64
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
                                    self.stack_tracker.push(StackPoint::U8);
                                }
                                _ => {
                                    return Err(anyhow!("Intrinsic LessThan needs 2 u64 values on the stack"));
                                }
                            }
                        },
                        Intrinsic::Drop => {
                            match self.stack_tracker.pop() {
                                Some(StackPoint::U64) => {},
                                _ => {
                                    return Err(anyhow!("Intrinsic Drop needs a u64 value on the stack"));
                                }
                            }
                        }
                    }
                }
                Action::Loop(contents) => {
                    let initial_stack = self.stack_tracker.clone();

                    self.typecheck_program(&contents)?;

                    if initial_stack != self.stack_tracker {
                        return Err(
                            anyhow!(format!("Invalid stack state at the end of the loop. The stack should be the same as the beginning.
Stack before:\n{:?}
Stack After:\n{:?}",
                                initial_stack,
                                self.stack_tracker
                            ))
                        );
                    }
                }
            }
        }

        Ok(())
    }
}