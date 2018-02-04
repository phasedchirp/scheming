use std::io::{stdout, stdin, Write};
use std::collections::HashSet;

fn is_op(c: &char, tbl: &HashSet<char>) -> bool {
    tbl.contains(c)
}

#[derive(Debug, PartialEq)]
enum LispVal {
    Atom(String)
}

use::LispVal::*;

#[derive(Debug, PartialEq)]
struct Tokens {
    vals: Vec<LispVal>
}

impl Tokens {
    fn new() -> Tokens {
        Tokens{vals: Vec::new()}
    }

    fn push(&mut self, val: LispVal) {
        self.vals.push(val);
    }

    fn pop(&mut self) -> Option<LispVal> {
        self.vals.pop()
    }
}

fn tokenize(s: &str) -> Tokens {
    let mut tokens = Tokens::new();
    let mut input = s.chars().peekable();
    loop {
        match input.peek() {
            Some(&'(') => tokens.push(
                Atom(input.next().unwrap().to_string())
            ),
            Some(&')') =>  tokens.push(
                Atom(input.next().unwrap().to_string())
            ),
            Some(_)    => {
                let mut token = String::new();
                while input.peek() != Some(&')')
                   && input.peek() != Some(&'(') {
                       token.push(input.next().unwrap());
                   }
                tokens.push(Atom(token));
            },
            None => return tokens

        }
    }
}

fn repl() {
    loop {
        let mut user_input = String::new();
        print!("> ");
        stdout().flush().expect("Something went wrong?");
        stdin().read_line(&mut user_input)
                   .expect("Failed to read input");
        let mut tokens = tokenize(&user_input.trim());
    }

}

fn main() {
    let test = tokenize("(stuff(nested(inside(of(stuff)))))");
    println!("{:?}",test);
    // repl();
 }
