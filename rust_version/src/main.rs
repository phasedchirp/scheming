// following pointers from http://norvig.com/lispy.html
use std::io::{stdout, stdin, Write};
use std::collections::HashSet;

// special characters
fn is_id(c: &char, tbl: &HashSet<char>) -> bool {
    tbl.contains(c)
}

// whitespace
fn is_ws(c: &char) -> bool {
    c == &' ' || c == &'\n' || c == &'\t'
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

fn tokenize(s: &str, ids: &HashSet<char>) -> Tokens {
    let mut tokens = Tokens::new();
    let mut token = String::new();
    for c in s.chars() {
        if is_id(&c,ids) {
            if !token.is_empty() {
                tokens.push(Atom(token.clone()));
                token = String::new();
            }
            tokens.push(Atom(c.to_string()));
        } else if is_ws(&c) {
            if !token.is_empty() {
                tokens.push(Atom(token.clone()));
                token = String::new();
            }
            continue;
        } else {
            token.push(c);
        }
    }
    if !token.is_empty() {
        tokens.push(Atom(token));
    }
    tokens
}

// fn make_ast()

fn repl(ids: &HashSet<char>) {
    loop {
        let mut user_input = String::new();
        print!("> ");
        stdout().flush().expect("Something went wrong?");
        stdin().read_line(&mut user_input)
                   .expect("Failed to read input");
        let mut tokens = tokenize(&user_input.trim(),ids);
    }

}

fn main() {
    let mut ids = HashSet::new();
    for id in vec!['(',')'] {
        ids.insert(id);
    }
    let test = tokenize("(stuff(nested(inside(of(stuff)))))",&ids);
    println!("{:?}",test);
    // repl();
 }
