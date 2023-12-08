
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

        for _ in 0..size {
            let byte = match self.next() {
                Some(n) => n,
                None => return result
            };

            result = (result << 1) | (*byte).into();
        }

        result
    }
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[derive(Debug)]
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
    Jmp = 0x0A,
    Jpt = 0x0B,
    Jpf = 0x0C,
    Cll = 0x0d,
    Str = 0x0E,
    Pnt = 0x0F,
    Dbg = 0x10,
    Pch = 0x20,
    Inc = 0x11,
    Dec = 0x12,
    Psh = 0x13,
    Dup = 0x14,
    Swp = 0x15,
    Drp = 0x16,
    Rot = 0x17,
    Ovr = 0x18,
    Ref = 0x30,
    Rf8 = 0x31,
    Bkp = 0xFE,
    Ext = 0xFF
}

pub struct Program {
    data: Vec<u8>,
}

impl Program {
    pub fn dissassemble(&self) -> Result<()> {
        let mut iter = self.data.iter();

        while let Some(bt) = iter.next() {
            let op: Opcode = (*bt).try_into()
                .context(format!("Byte {bt:#X} is not a valid opcode. Maybe there's an offset problem?"))?;
            print!("    {op:?} ");

            match op {
                Opcode::Psh => {
                    // println!("\n-----\n{:?}\n-----\n", iter);
                    // let slice: &[u8; 8] = iter.as_slice().try_into()?;
                    // let num: u64 = u64::from_be_bytes(*slice);
                    let num = iter.cast_to::<u64>();
                    
                    print!("{}", num);

                    // println!("\n-----\n{:?}\n-----\n", iter);
                },
                Opcode::Str => {
                    print!("\"");
                    while let Some(c) = iter.next() {
                        if *c == b'\0' { break; } // C str

                        // TODO: Escape characters

                        print!("{}", *c as char);
                    }
                    print!("\"");
                },
                _ => {}
            }
            
            println!("");
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Instruction {
    Push(u64),
    PushLabel(String),
    String(String),
    Single(Opcode),
}

impl Instruction {
    fn size(&self) -> usize {
        match self {
            Instruction::Push(_) => 1 + size_of::<u64>(),
            Instruction::PushLabel(_) => 1 + size_of::<u64>(),
            Instruction::String(val) => 1 + val.bytes().len() + 1, // \0 terminated
            Instruction::Single(_) => 1,
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

    pub fn to_program(self) -> Result<Program> {
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
                Some(val) => val - 1,
                None => return Err(anyhow!("Trying to push unlinked label {key}")),
            };

            let instruction_index = instruction_addrs[index as usize];
            label_addrs.insert(key, instruction_index);
        }

        for instruction in self.instructions {
            match instruction {
                Instruction::Push(val) => {
                    data.push(Opcode::Psh.into());
                    data.extend_from_slice(&val.to_be_bytes());
                },
                Instruction::PushLabel(lbl) => {
                    data.push(Opcode::Psh.into());
                    let label_value = match label_addrs.get(&lbl) {
                        Some(index) => index,
                        None => return Err(anyhow!("Trying to push unknown lable {lbl}")),
                    };

                    data.extend_from_slice(&label_value.to_be_bytes());
                },
                Instruction::String(str) => {
                    data.push(Opcode::Str.into());

                    data.extend_from_slice(str.as_bytes());

                    data.push('\0' as u8);
                },
                Instruction::Single(opcode) => {
                    data.push(opcode.into());
                },
            }
        }

        Ok(Program {
            data,
        })
    }
}