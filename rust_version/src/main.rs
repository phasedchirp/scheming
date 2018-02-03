use std::io::{stdout, stdin, Write};
// use std::io::Write;

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
    for c in s.chars() {
        if c == '(' {
            stack.push('(')
        } else if c == ')' {
            if let None = stack.pop() {
                stack.valid = false;
                break;
            }
        } else {
            println!("Invalid input");
            break;
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
