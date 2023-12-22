
use num::Zero;
use std::{collections::HashMap, mem::size_of};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use anyhow::{Result, anyhow, Context};

trait IterToPrimitive {
    fn cast_to<T>(&mut self) -> T
    where
        T: From<u8> + Zero + std::ops::Shl<u32, Output = T> + std::ops::BitOr<T, Output = T>;
}

impl IterToPrimitive for dyn Iterator<Item = u8> {
    fn cast_to<T>(&mut self) -> T 
    where
        T: From<u8> + Zero + std::ops::Shl<u32, Output = T> + std::ops::BitOr<T, Output = T> {
        let mut result = Zero::zero();
        let size = size_of::<T>();

        for _ in 0..size {
            let byte = match self.next() {
                Some(n) => n,
                None => return result
            };

            result = (result << 1) | byte.into();
        }

        result
    }
}

impl IterToPrimitive for core::slice::Iter<'_, u8> {
    fn cast_to<T>(&mut self) -> T 
    where
        T: From<u8> + Zero + std::ops::Shl<u32, Output = T> + std::ops::BitOr<T, Output = T> {
        let mut result = Zero::zero();
        let size = size_of::<T>();

        for i in 0..size {
            let byte = match self.next() {
                Some(n) => n,
                None => return result
            };

            let byte_as_t: T = (*byte).into();

            result = result | (byte_as_t << (i as u32));
        }

        result
    }
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Opcode {
    Nop = 0x00,
//  Ldr = 0x02,
    Add = 0x03,
    Sub = 0x04,
    Mod = 0x05,
    Div = 0x06,
    Mul = 0x07,
    Equ = 0x08,
    Lt  = 0x09,
    Gt  = 0x1B,
    Not = 0x21,
    Or  = 0x22,
    Jmp = 0x0A,
    Jpt = 0x0B,
    Jpf = 0x0C,
    Cll = 0x0d,
    Str = 0x0E,
    Pts = 0x0F,
    Dbg = 0x10,
    Ptc = 0x20,
    Inc = 0x11,
    Dec = 0x12,
    Psh = 0x13,
    Dup = 0x14,
    Swp = 0x15,
    Drp = 0x16,
    Rot = 0x17,
    Ovr = 0x18,
    Ret = 0x23,
    Tks = 0x24,
    Ref = 0x30,
    Rf8 = 0x31,
    Ps8 = 0x40,
    Drn = 0x41,
    Dpn = 0x42,
    Bkp = 0xFE,
    Ext = 0xFF
}

pub struct Program {
    data: Vec<u8>,
}

impl Program {
    pub fn dissassemble(&self) -> Result<()> {
        let mut iter = self.data.iter();

        let mut counter: usize = 0;

        while let Some(bt) = iter.next() {
            let op: Opcode = (*bt).try_into()
                .context(format!("Byte {bt:#X} is not a valid opcode. Maybe there's an offset problem?"))?;
            print!("0x{:04X}\t", counter);
            print!("    {op:?} ");

            counter += 1;

            match op {
                Opcode::Psh => {
                    // println!("\n-----\n{:?}\n-----\n", iter);
                    // let slice: &[u8; 8] = iter.as_slice().try_into()?;
                    // let num: u64 = u64::from_be_bytes(*slice);
                    counter += std::mem::size_of::<u64>();
                    let num = iter.cast_to::<u64>();
                    
                    print!("0x{:04X}", num);

                    // println!("\n-----\n{:?}\n-----\n", iter);
                },
                Opcode::Ps8 => {
                    // println!("\n-----\n{:?}\n-----\n", iter);
                    // let slice: &[u8; 8] = iter.as_slice().try_into()?;
                    // let num: u64 = u64::from_be_bytes(*slice);
                    counter += 1;
                    let num = iter.cast_to::<u8>();
                    
                    print!("0x{:02X}", num);

                    // println!("\n-----\n{:?}\n-----\n", iter);
                },
                Opcode::Drn => {
                    counter += std::mem::size_of::<u64>();
                    let num = iter.cast_to::<u64>();
                    
                    print!("{:#04X}", num);
                }
                Opcode::Dpn => {
                    counter += std::mem::size_of::<u64>();
                    let num = iter.cast_to::<u64>();
                    
                    print!("offset: {:#04X} ", num);

                    counter += std::mem::size_of::<u64>();
                    let num = iter.cast_to::<u64>();
                    
                    print!("size: {:#04X}", num);
                }
                Opcode::Str => {
                    counter += std::mem::size_of::<u64>();
                    let num = iter.cast_to::<u64>();
                    print!("({num:#X}) ");
                    print!("\"");

                    for _ in 0..num {
                        let c = match iter.next() {
                            Some(n) => n,
                            None => break
                        };

                        counter += 1;

                        // TODO: Escape characters

                        match *c as char {
                            '\n' => print!("\\n"),
                            '\t' => print!("\\t"),
                            '\r' => print!("\\r"),
                            '\"' => print!("\\\""),
                            c => print!("{}", c)
                        }
                    }
                    print!("\"");
                },
                _ => {}
            }
            
            println!("");
        }

        Ok(())
    }

    pub fn bytes(&self) -> &[u8] {
        &self.data
    }
}

#[derive(Debug)]
pub enum Instruction {
    Push(u64),
    PushByte(u8),
    PushLabel(String),
    DropBytes(u64),
    DupBytes{offset: u64, n: u64},
    String(String),
    Single(Opcode),
}

impl Instruction {
    fn size(&self) -> usize {
        1 +
        match self {
            Instruction::Push(_) | Instruction::PushLabel(_) => size_of::<u64>(),
            Instruction::DropBytes(_) => size_of::<u64>(),
            Instruction::DupBytes { offset: _, n: _ } => size_of::<u64>() * 2,
            Instruction::PushByte(_) => size_of::<u8>(),
            Instruction::String(val) => {
                size_of::<u64>() +      // len 
                val.bytes().len()       // chars
             }
            Instruction::Single(_) => 0,
        }
    }
}

pub struct ProgramBuilder {
    instructions: Vec<Instruction>,
    labels: HashMap<String, Option<u64>>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn emit_instruction(&mut self, opcode: Opcode) {
        self.instructions.push(Instruction::Single(opcode))
    }

    pub fn create_label(&mut self, name: &str) -> String {
        if self.labels.contains_key(name) { return name.to_string(); }
        
        self.labels.insert(name.to_string(), None);

        name.to_string()
    }

    pub fn link_label(&mut self, name: &str) -> Result<()> {
        if let Some(label) = self.labels.get_mut(name) {
            let instruction_index = self.instructions.len();
            *label = Some(instruction_index as u64);

            Ok(())
        } else {
            Err(anyhow::anyhow!("Linking unknown label: {}", name))
        }
    }

    pub fn to_program(&self) -> Result<Program> {
        let mut instruction_addrs = vec![];
        instruction_addrs.reserve(self.instructions.len());

        let mut pointer = 0usize;
        
        for instruction in &self.instructions {
            instruction_addrs.push(pointer);

            pointer += instruction.size();
        }

        let mut data = Vec::new();
        data.reserve(pointer);

        let mut label_addrs = HashMap::new();
        for (key, val) in self.labels.iter() {
            let index = match *val {
                Some(val) => val,
                None => return Err(anyhow!("Trying to push unlinked label {key}")),
            };

            let instruction_index = instruction_addrs[index as usize];
            label_addrs.insert(key, instruction_index);
        }

        for instruction in self.instructions.iter() {
            match instruction {
                Instruction::Push(val) => {
                    data.push(Opcode::Psh.into());
                    data.extend_from_slice(&val.to_le_bytes());
                },
                Instruction::PushLabel(lbl) => {
                    data.push(Opcode::Psh.into());
                    let label_value = match label_addrs.get(&lbl) {
                        Some(index) => index,
                        None => return Err(anyhow!("Trying to push unknown lable {lbl}")),
                    };

                    data.extend_from_slice(&label_value.to_le_bytes());
                },
                Instruction::PushByte(b) => {
                    data.push(Opcode::Ps8.into());
                    data.push(*b);
                }
                Instruction::DropBytes(val) => {
                    data.push(Opcode::Drn.into());
                    data.extend_from_slice(&val.to_le_bytes());
                },
                Instruction::DupBytes{offset, n} => {
                    data.push(Opcode::Dpn.into());
                    data.extend_from_slice(&offset.to_le_bytes());
                    data.extend_from_slice(&n.to_le_bytes());
                },
                Instruction::String(str) => {
                    data.push(Opcode::Str.into());

                    let length = str.len();

                    data.extend_from_slice(&length.to_le_bytes());

                    data.extend_from_slice(str.as_bytes());
                },
                Instruction::Single(opcode) => {
                    data.push((*opcode).clone().into());
                },
            }
        }

        Ok(Program {
            data,
        })
    }
}