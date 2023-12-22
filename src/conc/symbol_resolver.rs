use std::collections::{HashSet, HashMap};
use crate::conc::frontend::StackType;
use crate::conc::semantic_analyzer::SemanticContext;
use crate::conc::typechecker::StackPoint;

use anyhow::{Result, anyhow, Context};

use std::fmt::Write;

pub struct SymbolResolver<'a> {
    unresolved_context: &'a SemanticContext,
    resolved_context: ResolvedSemanticContext,

    currently_resolving: HashSet<String>,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(unresolved_context: &'a SemanticContext) -> SymbolResolver {
        SymbolResolver {
            unresolved_context,
            resolved_context: ResolvedSemanticContext::new(),
            currently_resolving: HashSet::new()
        }
    }

    fn resolve_type(&mut self, stack_type: StackType) -> Result<StackPoint> {
        let value = match stack_type {
            StackType::Bool => StackPoint::Bool,
            StackType::U8 => StackPoint::U8,
            StackType::U64 => StackPoint::U64,
            StackType::Ptr => StackPoint::Ptr,
            StackType::Custom(name) => {
                if self.currently_resolving.contains(&name) {
                    return Err(anyhow!("Found a recursive dependency on struct {}", name));
                }

                let resolved = self.resolve_struct(&name)
                    .context(format!("Resolving struct {name}"))?
                    .ok_or(anyhow!("Type {name} is unknown"))?;

                StackPoint::Struct(resolved.name.clone())
            }
        };

        Ok(value)
    }

    fn resolve_struct(&mut self, name: &str) -> Result<Option<ResolvedStruct>> {
        if let Some(rs) = self.resolved_context.structs.get(name) {
            return Ok(Some(rs.clone()));
        }

        self.currently_resolving.insert(name.to_string());

        let s = match self.unresolved_context.structs().get(name) {
            Some(s) => s.clone(),
            None => return Ok(None)
        };

        let members: Result<_> = s.members().clone()
            .into_iter()
            .map(|(n, x)| {
                match self.resolve_type(x) {
                    Ok(t) => Ok((n, t)),
                    Err(e) => Err(e)
                }
            })
            .collect();

        let resolved_struct = ResolvedStruct {
            name: s.name().clone(),
            members: members?
        };

        self.resolved_context.structs.insert(name.to_string(), resolved_struct.clone());

        self.currently_resolving.remove(name);

        Ok(Some(resolved_struct.clone()))
    }

    pub fn resolve_context(&mut self) -> Result<&ResolvedSemanticContext> {
        for (name, f) in self.unresolved_context.functions() {
            let input: Result<_> = f.input().clone().into_iter()
                .map(|x| self.resolve_type(x)).collect();
            let output: Result<_> = f.output().clone().into_iter()
                .map(|x| self.resolve_type(x)).collect();
            let resolved_function = ResolvedFunction {
                name: f.name().clone(),
                input: input?,
                output: output?,
            };

            self.resolved_context.functions.insert(name.clone(), resolved_function);
        }

        let mut structs = HashMap::new();

        for name in self.unresolved_context.structs().keys() {
            let resolved_struct = self.resolve_struct(name)?
                .expect("Already iterating over valid keys");

            structs.insert(name.clone(), resolved_struct.clone());
        }

        Ok(&self.resolved_context)
    }

    pub fn get_context(self) -> ResolvedSemanticContext {
        self.resolved_context
    }
}

#[derive(Clone)]
pub struct ResolvedFunction {
    name: String,
    input: Vec<StackPoint>,
    output: Vec<StackPoint>
}

impl ResolvedFunction {
    pub fn input<'a>(&'a self) -> &'a Vec<StackPoint> {
        &self.input
    }

    pub fn output<'a>(&'a self) -> &'a Vec<StackPoint> {
        &self.output
    }

    pub fn label_name(&self) -> Result<String> {
        let mut result = String::new();

        result.push_str("fn_");
        result.push_str(&self.name);
        result.push_str("_");
        for input in self.input.iter() {
            write!(result, "{input:#?}")?;
        }

        result.push_str("$");
        for output in self.output.iter() {
            write!(result, "{output:#?}")?;
        }

        return Ok(result);
    }
}

#[derive(Clone)]
pub struct ResolvedStruct {
    name: String,
    members: Vec<(String, StackPoint)>
}

impl ResolvedStruct {
    #[allow(dead_code)]
    pub fn name<'a>(&'a self) -> &'a String {
        &self.name
    }

    pub fn members<'a>(&'a self) -> &'a Vec<(String, StackPoint)> {
        &self.members
    }
}

#[derive(Clone)]
pub struct ResolvedSemanticContext {
    functions: HashMap<String, ResolvedFunction>,
    structs: HashMap<String, ResolvedStruct>
}

impl ResolvedSemanticContext {
    fn new() -> Self {
        Self { functions: HashMap::new(), structs: HashMap::new() }
    }

    pub fn functions<'a>(&'a self) -> &'a HashMap<String, ResolvedFunction> {
        &self.functions
    }
    
    pub fn structs<'a>(&'a self) -> &'a HashMap<String, ResolvedStruct> {
        &self.structs
    }
}