use std::env;

fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    println!("Hello, {}!",arg_vec[1]);
}
