#[macro_use]
// extern crate nom;
extern crate combine;
extern crate num;

use std::env;
// use std::str::from_utf8;
use std::string::String;
mod scheme;
use scheme::*;
// use combine::*;
use combine::{spaces,many1, sep_by, Parser, ParserExt, ParseError};
use combine::char::{char,digit};



fn main() {
    let arg_vec: Vec<String> = env::args().collect();


    //Parse integers separated by commas, skipping whitespace
    // let mut integer_list = sep_by(integer, spaces().skip(char(',')));

    //Call parse with the input to execute the parser
    let mut input = "1234";
    // let input = "1234, 45,78";
    // let result: Result<(Vec<i32>, &str), ParseError<&str>> = integer_list.parse(input);
    let result = parse_number(input);
    match result {
        Ok((value, _remaining_input)) => println!("{:?}", value),
        Err(err) => println!("{}", err)
    }

    input = "3.1415926";

    let result2 = parse_float(input).unwrap();
    // match result2 {
        // Ok((value, _remaining_input)) => println!("{:?}", value),
        // Err(err) => println!("{}", err)
    // }
    println!("{:?}", result2);

    // let thingy : () = (many1(digit()),char('.'),many1(digit()));
    // let test = thingy.parse(input);
    // println!("{:?}", test);
    // let test_thing: Vec<&[u8]> = arg_vec.iter().map(|x| {x.as_bytes()}).collect();
    // let parsed = parse_bool(test_thing[1]);
    // let string_parse = parse_string(test_thing[2]);
    // let char_parse = parse_char(test_thing[3]);
    // println!("{:?}",parsed);
    // println!("{:?}",string_parse);
    // println!("{:?}",char_parse);
    println!("{:?}", arg_vec[1]);
}
