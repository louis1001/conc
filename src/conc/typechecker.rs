use crate::conc::frontend::Intrinsic;
use crate::conc::symbol_resolver::ResolvedSemanticContext;
use crate::conc::semantic_analyzer::SemanticTree;

use anyhow::{Result, anyhow, Context};

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
                let defined_struct = self.semantic_context.structs().get(name)
                    .ok_or(anyhow!("Constructing unknown struct {name}."))?;

                let mut members = defined_struct.members().clone();
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
                        let members = self.semantic_context.structs().get(&struct_name)
                            .ok_or(anyhow!("[UNREACHABLE] Accessing unknown struct {struct_name:?}"))?
                            .members();

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
                let function = self.semantic_context.functions().get(name)
                    .ok_or(anyhow!("Call to unknown function {name}."))?;

                let mut input = function.input().clone();
                input.reverse();

                for expected_input in &input {
                    let stack = self.stack_tracker.pop()
                        .ok_or(anyhow!("Function call to {name} needed {expected_input:?} on the stack but found nothing"))?;

                    if stack != *expected_input {
                        return Err(anyhow!("Function call to {name} needed {expected_input:?} on the stack but found {stack:?}"));
                    }
                }
                
                for output in function.output() {
                    self.stack_tracker.push(output.clone());
                }

                TypecheckedTree::FunctionCall(name.clone())
            }
            SemanticTree::Function(name, body) => {
                let function = self.semantic_context.functions().get(name)
                    .expect(format!("Function {name} should have been semantically analyzed").as_str())
                    .clone();
                
                let (body, _) = self.typecheck_function(&body, function.input(), function.output())
                    .context(format!("Typechecking function {name} failed"))?;

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
    pub fn size(&self, context: &ResolvedSemanticContext) -> Result<usize> {
        let v = match self {
            StackPoint::U8 => 1,
            StackPoint::U64 => 8,
            StackPoint::Bool => 1,
            StackPoint::Ptr => 8,
            StackPoint::Struct(name) => {
                let st = context.structs().get(name)
                    .ok_or(anyhow!("Failed to resolve size of struct {name}. Doesn't exist in context"))?;

                let mut sz = 0usize;

                for (_, member_size) in st.members() {
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