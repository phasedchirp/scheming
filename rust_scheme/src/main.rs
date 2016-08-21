#[macro_use]
extern crate nom;
extern crate num;

use std::env;
mod scheme;
use scheme::*;
use nom::*;

named!(test_parser,tag!("#\\"));

fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    let test = test_parser(b"#\\axyz");
    println!("{:?}",test);
    println!("Hello, {}!", arg_vec[1]);
}
