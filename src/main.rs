use anyhow::{Result, anyhow};
mod conc;
mod bytecode;

fn main() -> Result<()> {
    let mut args = std::env::args();
    let _program = args.next().unwrap();

    let input_file = args.next().ok_or(anyhow!("Expected an input file"))?;

    // let code = "1 2 + 10 < loop 10 drop end";
    let code = std::fs::read_to_string(input_file)?;

    let mut codegen = conc::Codegen::new();
    let mut typecheck = conc::Typechecker::new();
    
    let actions = conc::frontend::parse_conc(&code)?;
    typecheck.typecheck_scope(&actions, Some(&[]))?;

    codegen.generate_program(&actions)?;
    let program = codegen.get_program()?;
    program.dissassemble()?;

    Ok(())
}
