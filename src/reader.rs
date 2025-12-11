use std::fs; // for file I/O

// Minimal data type, this'll change later
pub struct Source {
    pub raw_text: String,
}

impl Source {
    pub fn new(raw_text: String) -> Source {
        Source { raw_text }
    }
}

pub fn read_source(filename: &str) -> Source {
    // Try to read from our file
    match fs::read_to_string(filename) {
        Ok(s) => Source::new(s),

        // TODO: this probably isn't great;
        // maybe I should thread error handling through?
        Err(_) => panic!("Couldn't read the file"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reader_works() {
        // TODO: really this file for test purposes should live somewhere else,
        // like a test directory or something...
        let filename = "src/hello.lox";
        dbg!(filename);
        read_source(filename);
        assert!(true);
    }
}
