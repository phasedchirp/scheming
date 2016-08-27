#[macro_use]
extern crate nom;
extern crate num;

use std::env;
// use std::str::from_utf8;
use std::string::String;
mod scheme;
use scheme::*;
use nom::*;



fn main() {
    let arg_vec: Vec<String> = env::args().collect();
    let test_thing: Vec<&[u8]> = arg_vec.iter().map(|x| {x.as_bytes()}).collect();
    let parsed = parse_bool(test_thing[1]);
    let string_parse = parse_string(test_thing[2]);
    let char_parse = parse_char(test_thing[3]);
    println!("{:?}",parsed);
    println!("{:?}",string_parse);
    println!("{:?}",char_parse);
}
