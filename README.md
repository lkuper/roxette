## Roxette

Roxette is an interpreter for the Lox programming language. Lox is the toy language described in the book [_Crafting Interpreters_](https://craftinginterpreters.com/) by Bob Nystrom.

Roxette is implemented in Rust, in a style that is fairly different from the Nystrom book's Java implementation.

Roxette is incomplete -- I only got as far as section ~~8.1~~ 8.3 of the book -- but it runs and does stuff.  Give `cargo run` a whirl.

Roxette will evaluate the program in `src/hello.lox` by default.  You can also pass in your own Lox programs on the command line by running `cargo run -- myprogram.lox`.
