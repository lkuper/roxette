use crate::evaluate::Val;
use std::collections::HashMap;

pub struct Environment {
    pub env: HashMap<String, Val>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            env: HashMap::new(),
        }
    }

    fn extend(&mut self, var: String, val: Val) {
        self.env.insert(var, val);
    }

    fn lookup(&mut self, var: String) -> Option<Val> {
        self.env.get(&var).cloned()
    }
}
