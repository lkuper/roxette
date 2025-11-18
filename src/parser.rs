use crate::tokenize::Token;
use crate::tokenize::TokenType;
use crate::tokenize::Tokens;

// Lox program grammar
// (from https://craftinginterpreters.com/statements-and-state.html#variable-syntax)

// program        → declaration* EOF ;

// declaration    → varDecl
//                | statement ;

// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

// statement      → exprStmt
//                | printStmt ;

// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;

// Lox expression grammar
// (from https://craftinginterpreters.com/representing-code.html#a-grammar-for-lox-expressions),
// with the addition of `IDENTIFIER` to the "literal" rule.

// This is different from what the book does, since in chapter 6 the book introduces something
// called "primary" to the grammar which takes the place of the old "literal" rule.
// It's a bit weird to cram in identifiers under "literal", but let's roll with it for a while.

// expression     → literal
//                | unary
//                | binary
//                | grouping ;

// primary        → NUMBER | STRING | "true" | "false" | "nil" | IDENTIFIER ;
// grouping       → "(" expression ")" ;
// unary          → ( "-" | "!" ) expression ;
// binary         → expression operator expression ;
// operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
//                | "+"  | "-"  | "*" | "/" ;

// Our `Expr` type looks like the above.
// However, the actual expression grammar we'll use looks like this:

// expression   := term | binary ;
// binary       := term operator term ;
// term         := literal | unary | grouping ;
// unary        := ("-" | "!") term ;
// literal      := NUMBER | STRING | "true" | "false" | "nil" | IDENTIFIER ;
// grouping     := "(" expression ")" ;
// operator     :=  "==" | "!=" | "<" | "<=" | ">" | ">="
//                 | "+" | "-"  | "*" | "/" ;

// This'll make it so we don't have to deal with expressions like

// "2 + 3 + 4"

// Instead, it'd have to be *grouped* into "(2 + 3) + 4" or "2 + (3 + 4)".

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

impl Program {
    pub fn new(decls: Vec<Decl>) -> Program {
        Program { decls }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    ExprStmt(Expr),
    PrintStmt(Expr),
    VarDecl(Token, Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Lit),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Var(Token),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Plus,
    Minus,
    Times,
    Div,
}

// Pretty-printing expressions.
// TODO: this isn't used anywhere except in tests.
// But it would be nice to have a pretty-printer!
fn format_expr(e: Expr) -> String {
    match e {
        Expr::Literal(l) => match l {
            Lit::Number(n) => n.to_string(),
            Lit::String(s) => s,
            Lit::True => "true".to_string(),
            Lit::False => "false".to_string(),
            Lit::Nil => "nil".to_string(),
        },
        Expr::Grouping(e) => {
            let s = "(".to_owned() + format_expr(*e).as_str() + ")";
            s.to_string()
        }
        Expr::Unary(op, e) => {
            let o = match op {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "!",
            };
            let s = o.to_owned() + format_expr(*e).as_str();
            s.to_string()
        }
        Expr::Binary(e1, op, e2) => {
            let o = match op {
                BinOp::Eq => "==",
                BinOp::Ne => "!=",
                BinOp::Lt => "<",
                BinOp::Le => "<=",
                BinOp::Gt => ">",
                BinOp::Ge => ">=",
                BinOp::Plus => "+",
                BinOp::Minus => "-",
                BinOp::Times => "*",
                BinOp::Div => "/",
            };
            let s = format_expr(*e1).as_str().to_owned() + o + format_expr(*e2).as_str();
            s.to_string()
        }
        Expr::Var(_) => {
            todo!()
        }
    }
}

// Maybe this function belongs in the tokenizer?
fn is_unary_op(t: TokenType) -> bool {
    match t {
        // This is a little weird, because obviously a `-` token occurring
        // in the middle of a binary expression is a binary operator.
        // But the way we're using the function, hopefully it's fine.
        TokenType::Minus => true,
        TokenType::Bang => true,
        _ => false,
    }
}

// Maybe this function belongs in the tokenizer?
fn is_literal(t: TokenType) -> bool {
    match t {
        TokenType::String => true,
        TokenType::Number => true,
        TokenType::True => true,
        TokenType::False => true,
        TokenType::Nil => true,
        _ => false,
    }
}

// Maybe this function belongs in the tokenizer?
fn is_binop(t: TokenType) -> bool {
    match t {
        TokenType::EqEq => true,
        TokenType::BangEq => true,
        TokenType::Less => true,
        TokenType::Le => true,
        TokenType::Greater => true,
        TokenType::Ge => true,
        TokenType::Plus => true,
        TokenType::Minus => true,
        TokenType::Slash => true,
        TokenType::Star => true,
        _ => false,
    }
}

fn parse_ident(token: Token, n: usize) -> (Expr, usize) {
    let ident = match token.token_type {
        TokenType::Identifier => Expr::Var(token),
        _ => panic!("Passed a non-ident token to parse_ident"),
    };
    (ident, n + 1)
}

fn parse_lit(token: Token, n: usize) -> (Expr, usize) {
    let lit = match token.token_type {
        TokenType::String => Expr::Literal(Lit::String(token.lexeme.to_string())),
        // We have to do this here because we're not storing actual numbers in tokens,
        // unlike in the Nystrom book.  We're just storing lexemes as strings.
        TokenType::Number => Expr::Literal(Lit::Number(token.lexeme.parse().unwrap())),
        TokenType::True => Expr::Literal(Lit::True),
        TokenType::False => Expr::Literal(Lit::False),
        TokenType::Nil => Expr::Literal(Lit::Nil),
        _ => panic!("Passed a non-literal token to parse_lit"),
    };
    (lit, n + 1)
}

fn parse_unary_op(token: Token, n: usize) -> (UnaryOp, usize) {
    let unary_op = match token.token_type {
        TokenType::Minus => UnaryOp::Neg,
        TokenType::Bang => UnaryOp::Not,
        _ => panic!("Passed a non-unary-op token to parse_unary_op"),
    };
    (unary_op, n + 1)
}

fn parse_binop(token: Token, n: usize) -> (BinOp, usize) {
    let binary_op = match token.token_type {
        TokenType::Minus => BinOp::Minus,
        TokenType::Plus => BinOp::Plus,
        TokenType::Slash => BinOp::Div,
        TokenType::Star => BinOp::Times,
        TokenType::BangEq => BinOp::Ne,
        TokenType::EqEq => BinOp::Eq,
        TokenType::Greater => BinOp::Gt,
        TokenType::Ge => BinOp::Ge,
        TokenType::Less => BinOp::Lt,
        TokenType::Le => BinOp::Le,
        _ => panic!("Passed a non-binop token to parse_binop"),
    };
    (binary_op, n + 1)
}

fn parse_term(tokens: Tokens, n: usize) -> (Expr, usize) {
    let (term, n_after_term) = match tokens.tokens[n].token_type {
        TokenType::Identifier => parse_ident(tokens.tokens[n].clone(), n),
        tt if is_literal(tt) => parse_lit(tokens.tokens[n].clone(), n),
        tt if is_unary_op(tt) => {
            let (op, n_after_unary_op) = parse_unary_op(tokens.tokens[n].clone(), n);
            let (expr, n_after_term) = parse_term(tokens.clone(), n_after_unary_op);
            (Expr::Unary(op, Box::new(expr)), n_after_term)
        }
        // We have a grouping
        TokenType::LeftParen => {
            // Increment one to consume the left paren
            let (expr, n_after_grouping) = parse_expr(tokens.clone(), n + 1);
            // Increment one to consume the right paren
            (Expr::Grouping(Box::new(expr)), n_after_grouping + 1)
        }
        _ => panic!("Not implemented yet"),
    };
    (term, n_after_term)
}

fn parse_expr(tokens: Tokens, n: usize) -> (Expr, usize) {
    let (left, n_after_left) = parse_term(tokens.clone(), n);
    match tokens.tokens[n_after_left].token_type {
        // Binary expression case
        tt if is_binop(tt) => {
            // We have a binary expression
            let (op, n_after_op) = parse_binop(tokens.tokens[n_after_left].clone(), n_after_left);
            let (right, n_after_right) = parse_term(tokens, n_after_op);
            let expr = Expr::Binary(Box::new(left), op, Box::new(right));
            (expr, n_after_right)
        }
        // Term case
        _ => (left, n_after_left),
    }
}

// This is for testing of parsing expressions in isolation.
// TODO: I'm not really sure where this code should live,
// since it's used for testing only.
// It should probably go in the test module.
pub fn parse_expr_wrapper(tokens: Tokens) -> Expr {
    let (e, _) = parse_expr(tokens, 0);
    e
}

fn parse_print(tokens: Tokens, n: usize) -> (Decl, usize) {
    // Increment one to consume the "print" token
    let (expr, n_after_print) = parse_expr(tokens.clone(), n + 1);
    // What's next had better be a semicolon
    let next_token = tokens.tokens[n_after_print].clone();
    if !(next_token.token_type == TokenType::Semicolon) {
        panic!(
            "There seems to be a syntax error on or around line {:?}",
            next_token.line
        );
    } else {
        // Increment one to consume the semicolon token
        (Decl::PrintStmt(expr), n_after_print + 1)
    }
}

fn parse_stmt(tokens: Tokens, n: usize) -> (Decl, usize) {
    let (stmt, n_after_stmt) = match tokens.tokens[n].token_type {
        // Handle print statements, which contain expressions.
        TokenType::Print => parse_print(tokens.clone(), n),
        // We'll handle other statements later.
        // Meanwhile, if we're here, we must have the only other thing we can have,
        // which is an "expression statement".
        _ => {
            let (expr, n_after_expr) = parse_expr(tokens.clone(), n);
            // What's next had better be a semicolon
            let next_token = tokens.tokens[n_after_expr].clone();
            if !(next_token.token_type == TokenType::Semicolon) {
                panic!(
                    "There seems to be a syntax error on or around line {:?}",
                    next_token.line
                );
            } else {
                // Increment one to consume the semicolon token
                (Decl::ExprStmt(expr), n_after_expr + 1)
            }
        }
    };
    (stmt, n_after_stmt)
}

fn parse_ident_decl(token: Token, n: usize) -> (Token, usize) {
    let name = match token.token_type {
        TokenType::Identifier => token,
        _ => panic!("Passed a non-ident token to parse_ident_decl"),
    };
    (name, n + 1)
}

fn parse_var_decl(tokens: Tokens, n: usize) -> (Decl, usize) {
    // Increment one to consume the "var" token
    let (name, n_after_var) = parse_ident_decl(tokens.tokens[n].clone(), n + 1);
    let (initializer, n_after_expr) = parse_expr(tokens.clone(), n_after_var);
    // Increment one to consume the semicolon token
    (Decl::VarDecl(name, initializer), n_after_expr + 1)
}

// Oddly, in the book's grammar, a "statement" is a kind of "declaration",
// but the way the book's code is written, a "declaration" seems to be a kind of "statement".
// We're going to go with the former.

// Hmm, although, maybe it's split up this way in the book because of the book's weird grammar
// that's designed to make precedence explicit.
// I'm actually kind of confused about what the right thing is to do here.
fn parse_decl(tokens: Tokens, n: usize) -> (Decl, usize) {
    let (decl, n_after_decl) = match tokens.tokens[n].token_type {
        // If we encounter a `var` token, this is a variable declaration.
        // Increment one to consume the `var` keyword.
        TokenType::Var => parse_var_decl(tokens.clone(), n + 1),
        // Otherwise, this is a statement.
        _ => parse_stmt(tokens.clone(), n),
    };
    (decl, n_after_decl)
}

pub fn parse(tokens: Tokens) -> Program {
    println!("Parsing...");

    // At the top level, a Lox program is just a list of decls.
    let mut program = Program::new(Vec::new());
    // A counter for tokens.
    let mut n = 0;

    while tokens.tokens[n].token_type != TokenType::Eof {
        let (s, n_after_decl) = parse_decl(tokens.clone(), n);
        n = n_after_decl;
        program.decls.push(s);
    }

    // Check if we actually parsed all the input.
    // It's len()-1 because there should be one `Eof` token remaining.
    // TODO: Ensure that this last token is in fact the `Eof` token.
    if n != (tokens.tokens.len() - 1) {
        panic!("There's a syntax error someplace!")
    } else {
        program
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::Source;
    use crate::tokenize::tokenize;

    #[test]
    fn parser_runs() {
        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Nil, "nil".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let tokens = Tokens { tokens: vec };
        let _ast = parse_expr_wrapper(tokens);
        assert!(true);
    }

    #[test]
    fn format_number() {
        let e = Expr::Literal(Lit::Number(42 as f64));
        let p = format_expr(e);
        assert_eq!(p, "42");
    }

    #[test]
    fn format_string() {
        let e = Expr::Literal(Lit::String("hello".to_string()));
        let p = format_expr(e);
        assert_eq!(p, "hello");
    }

    #[test]
    fn format_true() {
        let e = Expr::Literal(Lit::True);
        let p = format_expr(e);
        assert_eq!(p, "true");
    }

    #[test]
    fn format_false() {
        let e = Expr::Literal(Lit::False);
        let p = format_expr(e);
        assert_eq!(p, "false");
    }

    #[test]
    fn format_nil() {
        let e = Expr::Literal(Lit::Nil);
        let p = format_expr(e);
        assert_eq!(p, "nil");
    }

    #[test]
    fn format_neg() {
        let e = Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "-42");
    }

    #[test]
    fn format_not() {
        let e = Expr::Unary(UnaryOp::Not, Box::new(Expr::Literal(Lit::False)));
        let p = format_expr(e);
        assert_eq!(p, "!false");
    }

    #[test]
    fn format_binop_eq() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::False)),
            BinOp::Eq,
            Box::new(Expr::Literal(Lit::False)),
        );
        let p = format_expr(e);
        assert_eq!(p, "false==false");
    }

    #[test]
    fn format_binop_not_eq() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::False)),
            BinOp::Ne,
            Box::new(Expr::Literal(Lit::True)),
        );
        let p = format_expr(e);
        assert_eq!(p, "false!=true");
    }

    #[test]
    fn format_binop_less_than() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
            BinOp::Lt,
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "42<43");
    }

    #[test]
    fn format_binop_less_eq() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
            BinOp::Le,
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "42<=43");
    }

    #[test]
    fn format_binop_greater_than() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Gt,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43>42");
    }

    #[test]
    fn format_binop_greater_eq() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Ge,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43>=42");
    }

    #[test]
    fn format_binop_plus() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Plus,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43+42");
    }

    #[test]
    fn format_binop_minus() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Minus,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43-42");
    }

    #[test]
    fn format_binop_times() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Times,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43*42");
    }

    #[test]
    fn format_binop_div() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let p = format_expr(e);
        assert_eq!(p, "43/42");
    }

    #[test]
    fn format_grouping() {
        let e = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let e_group = Expr::Grouping(Box::new(e));
        let p = format_expr(e_group);
        assert_eq!(p, "(43/42)");
    }

    #[test]
    fn parse_nil() {
        let source = Source::new("nil".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        assert_eq!(ast, Expr::Literal(Lit::Nil));
    }

    #[test]
    fn parse_unary_expr_1() {
        let source = Source::new("-42".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_unary_expr_2() {
        let source = Source::new("!false".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Unary(UnaryOp::Not, Box::new(Expr::Literal(Lit::False)));

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_grouping_1() {
        let source = Source::new("(-42)".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let binary_expr = Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let expected = Expr::Grouping(Box::new(binary_expr));
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_grouping_2() {
        let source = Source::new("(43/42)".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let binary_expr = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );
        let expected = Expr::Grouping(Box::new(binary_expr));
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_binop_1() {
        let source = Source::new("43+42".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Plus,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_binop_2() {
        let source = Source::new("43/42".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(43 as f64))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(42 as f64))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_binop_3() {
        let source = Source::new("(1+2)/3".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Literal(Lit::Number(1 as f64))),
                BinOp::Plus,
                Box::new(Expr::Literal(Lit::Number(2 as f64))),
            )))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(3 as f64))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_binop_4() {
        let source = Source::new("((1+2)/3)+((4-5)*6)".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let subexpr1 = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Literal(Lit::Number(1 as f64))),
                BinOp::Plus,
                Box::new(Expr::Literal(Lit::Number(2 as f64))),
            )))),
            BinOp::Div,
            Box::new(Expr::Literal(Lit::Number(3 as f64))),
        );
        let subexpr2 = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::Literal(Lit::Number(4 as f64))),
                BinOp::Minus,
                Box::new(Expr::Literal(Lit::Number(5 as f64))),
            )))),
            BinOp::Times,
            Box::new(Expr::Literal(Lit::Number(6 as f64))),
        );
        let expected = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(subexpr1))),
            BinOp::Plus,
            Box::new(Expr::Grouping(Box::new(subexpr2))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_binop_5() {
        let source = Source::new("7 != \"❤️\"".to_string());
        let tokens = tokenize(source);
        let ast = parse_expr_wrapper(tokens);

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(7 as f64))),
            BinOp::Ne,
            Box::new(Expr::Literal(Lit::String("❤️".to_string()))),
        );

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_var_decl_1() {
        let source = Source::new("var x = \"❤️\";".to_string());
        let tokens = tokenize(source);
        let ast = parse(tokens);

        let mut decls = Vec::new();
        decls.push(Decl::VarDecl(
            Token::new(TokenType::Identifier, "x".to_string(), 1),
            Expr::Literal(Lit::String("❤️".to_string())),
        ));
        let expected = Program::new(decls);

        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_var_decl_2() {
        let source = Source::new("var sum = 4 + 5;".to_string());
        let tokens = tokenize(source);
        let ast = parse(tokens);

        let mut decls = Vec::new();
        let subexpr = Expr::Binary(
            Box::new(Expr::Literal(Lit::Number(4 as f64))),
            BinOp::Plus,
            Box::new(Expr::Literal(Lit::Number(5 as f64))),
        );

        decls.push(Decl::VarDecl(
            Token::new(TokenType::Identifier, "sum".to_string(), 1),
            subexpr,
        ));
        let expected = Program::new(decls);

        assert_eq!(ast, expected);
    }
}
