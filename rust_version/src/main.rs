use std::io::{stdout, stdin, Write};
// use std::io::Write;

fn repl() {
    loop {
        let mut user_input = String::new();
        print!("> ");
        stdout().flush().expect("Something went wrong?");
        stdin().read_line(&mut user_input)
                   .expect("Failed to read input");
        println!("{:?}", user_input.trim());
    }

}

fn main() {
    repl();
}
