pub mod frontend;

use anyhow::{Result, anyhow};
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};

#[derive(Debug)]
pub enum Intrinsic {
    Add,
    Sub,
    Mul,
    Mod,

    Print,
    PrintChar,
    
    LessThan,
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

#[derive(Debug)]
pub enum Action {
    PushInt(u64),
    PushString(String),
    PushBool(bool),
    PushChar(u8),
    Intrinsic(Intrinsic),

    If(Vec<Action>, Option<Vec<Action>>),

    While(Vec<Action>, Vec<Action>),
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
                Action::PushBool(val) => self.pb.emit(bytecode::Instruction::PushByte(*val as u8)),
                Action::PushChar(c) => {
                    self.pb.emit(bytecode::Instruction::PushByte(*c));
                }
                Action::Intrinsic(intr) => {
                    match intr {
                        Intrinsic::Add => self.pb.emit_instruction(bytecode::Opcode::Add),
                        Intrinsic::Sub => self.pb.emit_instruction(bytecode::Opcode::Sub),
                        Intrinsic::Mul => self.pb.emit_instruction(bytecode::Opcode::Mul),
                        Intrinsic::Mod => self.pb.emit_instruction(bytecode::Opcode::Mod),
                        Intrinsic::LessThan => self.pb.emit_instruction(bytecode::Opcode::Lt),
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
                },
                Action::While(condition, body) => {
                    let go_back_label = self.pb.create_label(&format!("go_back_while_{}", self.label_count));
                    self.label_count += 1;

                    let break_label = self.pb.create_label(&format!("break_while_{}", self.label_count));
                    self.label_count += 1;

                    self.pb.link_label(&go_back_label)?;
                    self.generate_program(&condition)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(break_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jpf);

                    self.generate_program(&body)?;

                    self.pb.emit(bytecode::Instruction::PushLabel(go_back_label.clone()));
                    self.pb.emit_instruction(bytecode::Opcode::Jmp);

                    self.pb.link_label(&break_label)?;
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

    fn typecheck_action(&mut self, action: &Action) -> Result<()> {
        match action {
            Action::PushInt(_) => {
                self.stack_tracker.push(StackPoint::U64);
            },
            Action::PushString(_) => {
                self.stack_tracker.push(StackPoint::Ptr);
                self.stack_tracker.push(StackPoint::U64);
            },
            Action::PushBool(_) => {
                self.stack_tracker.push(StackPoint::Bool);
            },
            Action::PushChar(_) => self.stack_tracker.push(StackPoint::U8),
            Action::Intrinsic(intr) => {
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
            Action::Loop(contents) => {
                self.typecheck_subblock(&contents, Some(&[]))?;
            }
            Action::While(condition, body) => {
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
            Action::If(contents, else_contents) => {
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
        }

        Ok(())
    }

    pub fn typecheck_scope(&mut self, actions: &[Action], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
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

    fn typecheck_subblock(&mut self, actions: &[Action], output: Option<&[StackPoint]>) -> Result<Vec<StackPoint>> {
        let prev_stack = self.stack_tracker.clone();
        let mut subchecker = self.subchecker(prev_stack)?;

        let result = subchecker.typecheck_scope(actions, output)?;
        if output.is_some() {
            self.stack_tracker = result.clone();
        }

        Ok(result)
    }

    fn subchecker(&self, input: Vec<StackPoint>) -> Result<Typechecker> {
        let mut new_checker = Typechecker::new();
        new_checker.stack_tracker = input;

        Ok(new_checker)
    }
}