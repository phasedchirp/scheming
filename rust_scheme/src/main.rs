#[macro_use]
extern crate nom;

use std::env;
mod scheme;
use scheme::*;

fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    // let test = parse_bool("#tstuff");
    // println!("{:?}",test);
    println!("Hello, {}!", arg_vec[1]);
}
