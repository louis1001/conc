use crate::conc::frontend::Intrinsic;
use crate::conc::symbol_resolver::ResolvedSemanticContext;
use crate::bytecode::{self, ProgramBuilder as BytecodeBuilder};
use crate::conc::typechecker::TypecheckedTree;

use anyhow::Result;

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
        if let Some(main_func) = self.semantic_context.functions().get("main") {
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
                    let function = self.semantic_context.functions().get(name)
                        .expect(format!("Function {name} should have been semantically analyzed").as_str());

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.emit(bytecode::Instruction::PushLabel(fn_label));
                    self.pb.emit_instruction(bytecode::Opcode::Cll);
                }
                TypecheckedTree::Function(name, body) => {
                    let function = self.semantic_context.functions().get(name)
                        .expect(format!("Function {name} should have been semantically analyzed").as_str()).clone();

                    let label_name = function.label_name()?;
                    let fn_label = self.pb.create_label(&label_name);

                    self.pb.link_label(&fn_label)?;
                    
                    let args_size = function.input().iter().map(|x| x.size(&self.semantic_context))
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
