use crate::evaluate::Val;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    pub env: HashMap<String, Val>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            env: HashMap::new(),
        }
    }

    pub fn extend(&mut self, var: String, val: Val) {
        self.env.insert(var, val);
    }

    pub fn lookup(&mut self, var: String) -> Option<Val> {
        self.env.get(&var).cloned()
    }
}
