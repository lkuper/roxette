use crate::parser::BinOp;
use crate::parser::Expr;
use crate::parser::Lit;
use crate::parser::Program;
use crate::parser::Stmt;
use crate::parser::UnaryOp;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    LNum(f64),
    LStr(String),
    LBool(bool),
    LNil,
}

// This is so you can print Vals.
impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Val::LNum(n) => write!(f, "{}", n),
            Val::LStr(s) => write!(f, "{}", s),
            Val::LBool(b) => write!(f, "{}", b),
            Val::LNil => write!(f, "{}", "nil"),
        }
    }
}

fn eval_eq(v1: Val, v2: Val) -> bool {
    match (v1, v2) {
        // Just use Rust's equality on numbers, strings, and bools.
        (Val::LNum(v1), Val::LNum(v2)) => v1 == v2,
        (Val::LStr(v1), Val::LStr(v2)) => v1 == v2,
        (Val::LBool(v1), Val::LBool(v2)) => v1 == v2,
        // Nil is always equal to itself.
        (Val::LNil, Val::LNil) => true,
        // Things of different types are never equal.
        (_, _) => false,
    }
}

fn eval_lt(v1: Val, v2: Val) -> bool {
    match (v1, v2) {
        // Just use Rust's ordering on numbers, strings, and bools.
        (Val::LNum(v1), Val::LNum(v2)) => v1 < v2,
        (Val::LStr(v1), Val::LStr(v2)) => v1 < v2,
        (Val::LBool(v1), Val::LBool(v2)) => v1 < v2,
        // Nil is never less than itself.
        (Val::LNil, Val::LNil) => false,
        // Things of different types never have an order.
        (_, _) => false,
    }
}

pub fn evaluate_expr(e: Expr) -> Val {
    match e {
        Expr::Literal(l) => match l {
            Lit::Number(n) => Val::LNum(n),
            Lit::String(s) => Val::LStr(s),
            Lit::True => Val::LBool(true),
            Lit::False => Val::LBool(false),
            Lit::Nil => Val::LNil,
        },
        Expr::Grouping(e) => evaluate_expr(*e),
        Expr::Unary(op, e) => {
            let lv = evaluate_expr(*e);
            match (op, lv) {
                // I'm just going to make a design decision that you can't
                // use "-" on something that isn't a number,
                // or use "!" on something that isn't a Boolean.
                (UnaryOp::Neg, Val::LNum(n)) => Val::LNum(-n),
                (UnaryOp::Neg, _) => panic!("Can't use `-` on a non-numeric value"),
                (UnaryOp::Not, Val::LBool(b)) => Val::LBool(!b),
                (UnaryOp::Not, _) => panic!("Can't use `!` on a non-boolean value"),
            }
        }
        Expr::Binary(e1, op, e2) => {
            let lv1 = evaluate_expr(*e1);
            let lv2 = evaluate_expr(*e2);
            match (lv1.clone(), op, lv2.clone()) {
                // What things can you compare with `==` or `!=`?
                // All the things, but if you compare things of different types,
                // `==` will always be false and `!=` will always be true.
                (_, BinOp::Eq, _) => Val::LBool(eval_eq(lv1, lv2)),
                (_, BinOp::Ne, _) => Val::LBool(!eval_eq(lv1, lv2)),
                (_, BinOp::Lt, _) => Val::LBool(eval_lt(lv1, lv2)),
                (_, BinOp::Gt, _) => Val::LBool(!eval_lt(lv1, lv2)),

                // What things can you compare with `<=` or `>=`?
                // All the things, but if you compare things of different types,
                // `<=` and `>=` will always be false.
                (_, BinOp::Le, _) => {
                    match eval_eq(lv1.clone(), lv2.clone()) {
                        true => Val::LBool(true),
                        // If they're not =, check if they're <.
                        false => Val::LBool(eval_lt(lv1, lv2)),
                    }
                }
                (_, BinOp::Ge, _) => {
                    match eval_eq(lv1.clone(), lv2.clone()) {
                        true => Val::LBool(true),
                        // If they're not =, check if they're >.
                        false => Val::LBool(!eval_lt(lv1, lv2)),
                    }
                }
                // Arithmetic can only be done on numbers.
                (Val::LNum(v1), BinOp::Plus, Val::LNum(v2)) => Val::LNum(v1 + v2),
                (Val::LNum(v1), BinOp::Minus, Val::LNum(v2)) => Val::LNum(v1 - v2),
                (Val::LNum(v1), BinOp::Times, Val::LNum(v2)) => Val::LNum(v1 * v2),
                (Val::LNum(v1), BinOp::Div, Val::LNum(v2)) => {
                    if v2 == 0.0 {
                        panic!("Have you tried not dividing by zero?")
                    } else {
                        Val::LNum(v1 / v2)
                    }
                }
                (_, _, _) => panic!("Not implemented yet"),
            }
        }
    }
}

fn evaluate_stmt(stmt: Stmt) {
    match stmt {
        // For an expression statement,
        // just evaluate it for its side effect.
        Stmt::ExprStmt(e) => {
            evaluate_expr(e);
        }
        // For a print statement,
        // evaluate it, print the value, and return it.
        Stmt::PrintStmt(e) => {
            let val = evaluate_expr(e);
            println!("{}", val);
        }
    }
}

pub fn evaluate(program: Program) {
    println!("Evaluating...");

    for stmt in program.statements {
        evaluate_stmt(stmt);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::parser::parse_expr_wrapper;
    use crate::reader::Source;
    use crate::tokenize::tokenize;

    #[test]
    fn evaluator_runs() {
        let ast = Expr::Literal(Lit::True);
        let _val = evaluate_expr(ast);
        assert!(true);
    }

    #[test]
    fn eval_unary_1() {
        let source = Source::new("!false".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LBool(true));
    }

    #[test]
    fn eval_unary_2() {
        let source = Source::new("!!false".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LBool(false));
    }

    #[test]
    fn eval_unary_3() {
        let source = Source::new("---3".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LNum(-3.0));
    }

    #[test]
    fn eval_arith() {
        let source = Source::new("((30-10)/5)+((5*6)/10)".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LNum(7.0));
    }

    #[test]
    fn eval_emoji_string() {
        let source = Source::new("\"❤️ ❤️ ❤️\"".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LStr("❤️ ❤️ ❤️".to_string()));
    }

    #[test]
    fn eval_emoji_string_cmp() {
        let source = Source::new("\"❤️ ❤️ ❤️\" == \"❤️ ❤️\"".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LBool(false));
    }

    #[test]
    fn eval_expr_1() {
        let source = Source::new("(((30-10)/5)+((5*6)/10)) != \"❤️\"".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LBool(true));
    }

    #[test]
    fn eval_expr_2() {
        let source = Source::new("!(3 <= 4)".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);
        let val = evaluate_expr(ast);
        assert_eq!(val, Val::LBool(false));
    }

    #[test]
    fn eval_prog() {
        // TODO: Not really sure how to test this.
        // Redirect printing to a file or something,
        // then check the contents of the file?
        let program = "// Here's a Lox program!\n\
                       print \"Hello, world!\";\n\
                       print \"I'm a Lox program!\";";
        let source = Source::new(program.to_string());
        let tokens = tokenize(source);
        let ast = parse(tokens);
        let _val = evaluate(ast);
        assert!(true);
    }
}
