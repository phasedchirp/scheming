extern crate nom;

use std::env;
mod scheme;
use scheme::*;

fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    let test = LispVal::List(vec![LispVal::Number(64),LispVal::Number(24)]);
    println!("Hello, {:?}!",test);
    println!("{}", arg_vec[1]);
}
