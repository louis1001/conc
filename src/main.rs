use anyhow::Result;
mod conc;
mod bytecode;

fn main() -> Result<()> {
    let codegen = conc::Codegen::new();
    let mut typecheck = conc::Typechecker::new();

    let code = vec![
        conc::Action::PushInt(1),
        conc::Action::PushInt(2),
        conc::Action::Intrinsic(conc::Intrinsic::Add),
        conc::Action::PushInt(10),
        conc::Action::Intrinsic(conc::Intrinsic::LessThan)
    ];

    typecheck.typecheck_program(&code)?;
    let p = codegen.generate_program(&code)?;
    p.dissassemble()?;

    Ok(())
}
