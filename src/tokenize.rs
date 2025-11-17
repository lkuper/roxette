use crate::reader::Source;

// A token type for Lox, straight from the book
// ( https://craftinginterpreters.com/scanning.html#token-type ).
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    Ge,
    Less,
    Le,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

// This is an attempt at a Rust port of the `Token` class from the book.
// I left out the `literal` field because it seems kind of needless.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: i32, // Hopefully 32 bits is enough for anyone!
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: i32) -> Token {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

// I don't know if this really even has to be a struct.
// Do I want to stuff more stuff in here?
#[derive(Debug, PartialEq, Clone)]
pub struct Tokens {
    // This is `Vec<Token>` instead of just `[Token]`
    // because it has to be of statically known size.
    pub tokens: Vec<Token>,
}

impl Tokens {
    pub fn new(tokens: Vec<Token>) -> Tokens {
        Tokens { tokens }
    }
}

fn add_token(mut ts: Tokens, t: Token) -> Tokens {
    ts.tokens.push(t);
    ts
}

fn make_ident_token(s: String, line: i32) -> Token {
    let token_type = match s.as_str() {
        // Keywords.
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        // Otherwise, it's an identifier.
        _ => TokenType::Identifier,
    };

    Token::new(token_type, s, line)
}

pub fn tokenize(source: Source) -> Tokens {
    println!("Tokenizing...");

    let mut result = Tokens::new(Vec::new());
    let mut line = 1;

    // Iterate over characters in the raw text.
    let mut iter = source.raw_text.chars().peekable();
    while iter.peek().is_some() {
        // Presumably this `unwrap` is safe.
        let c = iter.next().unwrap();

        let token = match c {
            // Handle the one-character lexemes.
            '(' => Token::new(TokenType::LeftParen, c.to_string(), line),
            ')' => Token::new(TokenType::RightParen, c.to_string(), line),
            '{' => Token::new(TokenType::LeftBrace, c.to_string(), line),
            '}' => Token::new(TokenType::RightBrace, c.to_string(), line),
            ',' => Token::new(TokenType::Comma, c.to_string(), line),
            '.' => Token::new(TokenType::Dot, c.to_string(), line),
            '-' => Token::new(TokenType::Minus, c.to_string(), line),
            '+' => Token::new(TokenType::Plus, c.to_string(), line),
            ';' => Token::new(TokenType::Semicolon, c.to_string(), line),
            '*' => Token::new(TokenType::Star, c.to_string(), line),

            // Ignore whitespace.
            ' ' => continue,
            '\r' => continue,
            '\t' => continue,
            '\n' => {
                line += 1;
                continue;
            }

            // Handle operators that might have more than one token.
            '!' => match iter.peek() {
                Some('=') => {
                    // We've got a `!=` token!
                    iter.next();
                    Token::new(TokenType::BangEq, "!=".to_string(), line)
                }
                // We've got a `!` token!
                _ => Token::new(TokenType::Bang, c.to_string(), line),
            },
            '=' => match iter.peek() {
                Some('=') => {
                    // We've got a `==` token!
                    iter.next();
                    Token::new(TokenType::EqEq, "==".to_string(), line)
                }
                // We've got a `=` token!
                _ => Token::new(TokenType::Eq, c.to_string(), line),
            },
            '<' => match iter.peek() {
                Some('=') => {
                    // We've got a `<=` token!
                    iter.next();
                    Token::new(TokenType::Le, "<=".to_string(), line)
                }
                // We've got a `<` token!
                _ => Token::new(TokenType::Less, c.to_string(), line),
            },
            '>' => match iter.peek() {
                Some('=') => {
                    // We've got a `>=` token!
                    iter.next();
                    Token::new(TokenType::Ge, ">=".to_string(), line)
                }
                // We've got a `>` token!
                _ => Token::new(TokenType::Greater, c.to_string(), line),
            },
            '/' => match iter.peek() {
                Some('/') => {
                    // This is a comment, so go to the end of the line.
                    iter.next(); // we are now past `//`
                    while iter.peek().is_some() {
                        match iter.peek() {
                            // If we're at a newline, we're done with the comment
                            Some('\n') => {
                                iter.next();
                                line += 1;
                                break;
                            }
                            // otherwise, keep going through the comment
                            _ => {
                                iter.next();
                                continue;
                            }
                        }
                    }
                    continue;
                }
                // We've got a `/` token!
                _ => Token::new(TokenType::Slash, c.to_string(), line),
            },

            // Handle string literals.
            '"' => {
                let mut string_contents = String::new();
                // Now we're in the body of the string.
                while iter.peek().is_some() {
                    match iter.peek() {
                        // Lox supports multi-line strings!
                        // So, if this is a multi-line string,
                        // increment the line count.
                        Some('\n') => {
                            iter.next();
                            line += 1;
                            string_contents.push('\n');
                            continue;
                        }
                        // We're at the end of the string.
                        Some('"') => {
                            iter.next();
                            break;
                        }
                        // Any other character.
                        Some(next_char) => {
                            // For some reason the order of the next two lines matters
                            // to the borrow checker, and I don't know why!
                            string_contents.push(*next_char);
                            iter.next();
                            continue;
                        }
                        None => panic!("Unterminated string!"),
                    }
                }
                Token::new(
                    TokenType::String,
                    string_contents,
                    // TODO: Which line is a multi-line string on?
                    // I guess I'll go with last because it's easier.
                    line,
                )
            }

            // Handle number literals.
            n if n.is_digit(10) => {
                // This is a number.
                let mut num_contents = String::new();
                num_contents.push(n);

                // Handle the rest of the number.
                while iter.peek().is_some() {
                    match iter.peek() {
                        // It's a digit.
                        Some(next_char) if next_char.is_digit(10) => {
                            num_contents.push(*next_char);
                            iter.next();
                            continue;
                        }
                        // It's a decimal point.
                        Some('.') => {
                            // after_decimal = true;
                            num_contents.push('.');
                            iter.next();
                            continue;
                        }
                        // If it's not a digit or a decimal point,
                        // we're done with this number.
                        _ => break,
                        // TODO: We aren't doing any error handling
                        // here for if a number has, say, more than
                        // one decimal point. Maybe later.
                    }
                }
                Token::new(TokenType::Number, num_contents, line)
            }

            // What's left ought to be just identifiers and keywords.
            // We'll deal with them together.
            ch if (ch.is_alphabetic() || ch == '_') => {
                let mut ident_contents = String::new();
                ident_contents.push(ch);

                // Until we hit a non-alphanumeric character,
                // we're still dealing with the identifier.
                while iter.peek().is_some() {
                    match iter.peek() {
                        Some(next_char) if next_char.is_alphanumeric() => {
                            ident_contents.push(*next_char);
                            iter.next();
                            continue;
                        }
                        _ => break,
                    }
                }
                make_ident_token(ident_contents, line)
            }

            // TODO: probably need to do some better error handling here
            _ => {
                println!("{:?} is not implemented yet", c);
                continue;
            }
        };

        result = add_token(result, token)
    }

    let eof_token = Token::new(TokenType::Eof, "".to_string(), line);
    result = add_token(result, eof_token);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer_runs() {
        let source = Source::new("".to_string());
        tokenize(source);
        assert!(true);
    }

    #[test]
    fn tokenize_parens() {
        let source = Source::new("()".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::LeftParen, "(".to_string(), 1));
        vec.push(Token::new(TokenType::RightParen, ")".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_braces() {
        let source = Source::new("{}".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::LeftBrace, "{".to_string(), 1));
        vec.push(Token::new(TokenType::RightBrace, "}".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_math_ops() {
        let source = Source::new("+-*".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Plus, "+".to_string(), 1));
        vec.push(Token::new(TokenType::Minus, "-".to_string(), 1));
        vec.push(Token::new(TokenType::Star, "*".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));

        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_punctuation() {
        let source = Source::new(",.;".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Comma, ",".to_string(), 1));
        vec.push(Token::new(TokenType::Dot, ".".to_string(), 1));
        vec.push(Token::new(TokenType::Semicolon, ";".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_whitespace() {
        let source = Source::new("(\n\r \n\r \n\r \n)".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::LeftParen, "(".to_string(), 1));
        vec.push(Token::new(TokenType::RightParen, ")".to_string(), 5));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 5));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_bang_eq() {
        let source = Source::new("!=".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::BangEq, "!=".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_bang() {
        let source = Source::new("!".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Bang, "!".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_operators() {
        let source = Source::new("!= ! == = <= < >= >".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::BangEq, "!=".to_string(), 1));
        vec.push(Token::new(TokenType::Bang, "!".to_string(), 1));
        vec.push(Token::new(TokenType::EqEq, "==".to_string(), 1));
        vec.push(Token::new(TokenType::Eq, "=".to_string(), 1));
        vec.push(Token::new(TokenType::Le, "<=".to_string(), 1));
        vec.push(Token::new(TokenType::Less, "<".to_string(), 1));
        vec.push(Token::new(TokenType::Ge, ">=".to_string(), 1));
        vec.push(Token::new(TokenType::Greater, ">".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_comments() {
        let source = Source::new("// Hi, I'm a comment!\n// And here's another one!".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Eof, "".to_string(), 2));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_string() {
        let source = Source::new("\"Hello!\"".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::String, "Hello!".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_multiline_string() {
        let source = Source::new("\"Hello!\nI'm a multiline string\"".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(
            TokenType::String,
            "Hello!\nI'm a multiline string".to_string(),
            2,
        ));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 2));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_number() {
        let source = Source::new("42".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Number, "42".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_ident() {
        let source = Source::new("lindsey".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::Identifier, "lindsey".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_keywords() {
        let source = Source::new(
            "and class else false for fun if nil or \
             print return super this true var while"
                .to_string(),
        );

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::And, "and".to_string(), 1));
        vec.push(Token::new(TokenType::Class, "class".to_string(), 1));
        vec.push(Token::new(TokenType::Else, "else".to_string(), 1));
        vec.push(Token::new(TokenType::False, "false".to_string(), 1));
        vec.push(Token::new(TokenType::For, "for".to_string(), 1));
        vec.push(Token::new(TokenType::Fun, "fun".to_string(), 1));
        vec.push(Token::new(TokenType::If, "if".to_string(), 1));
        vec.push(Token::new(TokenType::Nil, "nil".to_string(), 1));
        vec.push(Token::new(TokenType::Or, "or".to_string(), 1));
        vec.push(Token::new(TokenType::Print, "print".to_string(), 1));
        vec.push(Token::new(TokenType::Return, "return".to_string(), 1));
        vec.push(Token::new(TokenType::Super, "super".to_string(), 1));
        vec.push(Token::new(TokenType::This, "this".to_string(), 1));
        vec.push(Token::new(TokenType::True, "true".to_string(), 1));
        vec.push(Token::new(TokenType::Var, "var".to_string(), 1));
        vec.push(Token::new(TokenType::While, "while".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_01() {
        let source = Source::new("( != )".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::LeftParen, "(".to_string(), 1));
        vec.push(Token::new(TokenType::BangEq, "!=".to_string(), 1));
        vec.push(Token::new(TokenType::RightParen, ")".to_string(), 1));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 1));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_02() {
        let source = Source::new("( \n != \r )".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();
        vec.push(Token::new(TokenType::LeftParen, "(".to_string(), 1));
        vec.push(Token::new(TokenType::BangEq, "!=".to_string(), 2));
        vec.push(Token::new(TokenType::RightParen, ")".to_string(), 2));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 2));
        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_03() {
        let source = Source::new("// Here's a Lox program!\n\"Hello, world!\"".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();

        vec.push(Token::new(
            TokenType::String,
            "Hello, world!".to_string(),
            2,
        ));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 2));

        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_04() {
        let source = Source::new("// Hello!\n3 < 4".to_string());

        let tokens = tokenize(source);

        let mut vec = Vec::new();

        vec.push(Token::new(TokenType::Number, "3".to_string(), 2));
        vec.push(Token::new(TokenType::Less, "<".to_string(), 2));
        vec.push(Token::new(TokenType::Number, "4".to_string(), 2));
        vec.push(Token::new(TokenType::Eof, "".to_string(), 2));

        let expected = Tokens::new(vec);

        assert_eq!(tokens, expected);
    }
}
