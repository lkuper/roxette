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

    pub fn lookup(&self, var: String) -> Option<Val> {
        self.env.get(&var).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluate::Val;

    #[test]
    fn env_works() {
        let mut env = Environment::new();
        env.extend("x".to_string(), Val::LNum(5.0));
        let result = env.lookup("x".to_string()).unwrap();
        assert_eq!(result, Val::LNum(5.0));
    }
}
