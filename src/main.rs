use anyhow::Result;
mod conc;
mod bytecode;

fn main() -> Result<()> {
    let mut codegen = conc::Codegen::new();
    let mut typecheck = conc::Typechecker::new();
    
    let actions = conc::frontend::parse_conc("1 2 + 10 < loop 10 drop end")?;
    typecheck.typecheck_program(&actions)?;

    let program = codegen.generate_program(&actions)?;
    program.dissassemble()?;

    Ok(())
}
