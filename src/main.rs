mod evaluate;
mod parser;
mod reader;
mod tokenize;

use std::env;

fn main() {
    // Get our command-line argument.
    let args: Vec<String> = env::args().collect();

    // I don't really know why we need `as_slice()` here.
    let input = match args.as_slice() {
        // A zero-length list should never happen,
        // since the first element should be the executable name.
        [] => panic!("This shouldn't happen"),

        // If the user didn't specify a file, for now we'll use `hello.lox`.
        // TODO: this should actually just tell the user not to do this.
        // But this is a hack for now so we can test `main`.
        [_] => "src/hello.lox",

        // The user specified a file on the command line. Good.
        [_, filename] => filename,

        // If the user passed more than one argument,
        // kindly let them know not to do that.
        _ => {
            println!("Only pass one argument, please");
            std::process::exit(0);
        }
    };

    let source = reader::read_source(input);
    let tokens = tokenize::tokenize(source);
    let ast = parser::parse(tokens);
    evaluate::evaluate(ast);
}

#[test]
fn interpreter_works() {
    main();
    assert!(true);
}
