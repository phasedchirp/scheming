use std::io::{stdout, stdin, Write};

// #[derive(Debug, PartialEq)]
// enum LispVal {
//     Atom(String)
// }

#[derive(Debug, PartialEq)]
struct Stack {
    stack: Vec<char>,
    valid: bool
}

impl Stack {
    fn new() -> Stack {
        Stack{stack: Vec::new(), valid: true}
    }

    fn push(&mut self, val: char) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Option<char> {
        self.stack.pop()
    }
}

fn parse_input(s: &str) {
    let mut stack = Stack::new();
    let mut input = s.chars().peekable();
    loop {
        match input.peek() {
            Some(&'(') => stack.push(input.next().unwrap()),
            Some(&')') => {
                if let None = stack.pop() {
                    stack.valid = false;
                    break;
                }
                input.next().unwrap();
            },
            _         => break

        }

    }
    if stack.valid {
        println!("Balanced");
    } else {
        println!("Unbalanced");
    }
}

fn repl() {
    loop {
        let mut user_input = String::new();
        print!("> ");
        stdout().flush().expect("Something went wrong?");
        stdin().read_line(&mut user_input)
                   .expect("Failed to read input");
        parse_input(&user_input.trim())
    }

}

fn main() {

    repl();
}
