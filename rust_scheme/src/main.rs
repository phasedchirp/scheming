extern crate nom;

use std::env;

// feels excessively haskell-y maybe?
#[derive(Clone, Debug, PartialEq)]
enum LispVal {
    Atom(String),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>,Box<LispVal>),
    Number(u64),
    LString(String),
    LBool(bool),
    LChar(char)
}

fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    let test = LispVal::List(vec![LispVal::Number(64),LispVal::Number(24)]);
    println!("Hello, {:?}!",test);
    println!("{}", arg_vec[1]);
}
