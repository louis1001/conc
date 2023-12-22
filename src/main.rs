use anyhow::{Result, anyhow, Context};
mod conc;
mod bytecode;

fn main() -> Result<()> {
    let mut args = std::env::args();
    let _program = args.next().unwrap();

    let input_file = args.next().ok_or(anyhow!("Expected an input file"))?;

    // let code = "1 2 + 10 < loop 10 drop end";
    let code = std::fs::read_to_string(input_file)?;

    let mut sem_an = conc::SemanticAnalyzer::new();
    let actions = conc::frontend::parse_conc(&code)?;

    let semantic_tree = sem_an.analyze_program(&actions)?;

    let context = sem_an.resolved_context()?;

    let mut typecheck = conc::Typechecker::new(context.clone());
    
    let (tree, _) = typecheck.typecheck_scope(&semantic_tree, Some(&[])).context("Global typecheck failed")?;

    let mut codegen = conc::Codegen::new(context.clone());
    codegen.generate_program(&tree)?;
    let program = codegen.get_program()?;
    program.dissassemble()?;

    if let Some(output_file) = args.next() {
        std::fs::write(output_file.clone(), program.bytes())?;

        println!("Exported program as {output_file}");
    }

    Ok(())
}
