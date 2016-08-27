// extern crate nom;
use num::Complex;
use nom::*;
use std::str::from_utf8;

// Data types:
#[derive(Clone, Debug, PartialEq)]
pub enum LispVal {
    Atom(String),
    List(Vec<LispVal>), // Possibly should be singly-linked list instead?
    DottedList(Vec<LispVal>,Box<LispVal>), // not quite proper definition.
    Vector(Vec<LispVal>), // Scheme standard calls for both but not different from List...
    Number(u64),
    Rational(u64,u64),
    Float(f64),
    Complex(Complex<f64>),
    LString(String),
    LBool(bool),
    LChar(char)
}


fn handle_parse(res : IResult<&[u8],&[u8]>) -> String{
    let matched = match res {
        IResult::Done(r,m) => from_utf8(m).unwrap(),
        // Need to clean this up once stuff is working.
        IResult::Error(_) => {
            println!("{:?}", res);
            "parse error"
        },
        IResult::Incomplete(_) => "incomplete parse"
    };
    return matched.to_string()
}

// Parsers:
// named!(symbols,one_of!([b"!",b"$",b"%",b"&",b"|",b"*",b"+",b"-",b"/",b":",b"<",b"=",b">",b"?",b"@",b"^",b"_",b"~"]));

// pub fn parse_symbol(s: &[u8]) -> String {
    // return handle_parse(symbols(s))
// }

// Parse Booleans
named!(bools,alt!(tag!("#t") | tag!("#f")));
pub fn parse_bool(s: &[u8]) -> String {
    return handle_parse(bools(s))
}

// Parse Characters
named!(chars,delimited!(tag!("#\\"),is_not!(" "),space));
pub fn parse_char(s: &[u8]) -> String {
    return handle_parse(chars(s))
}

// named!(parse_atom,unimplemented!());
// named!(parse_list,delimited!(char!("("),,char!(")")));
// named!(parse_vector,unimplemented!());
// named!(parse_dottedlist,unimplemented!());

// Parse Strings
named!(strings,delimited!(char!('\"'), is_not!("\""), char!('\"')));
pub fn parse_string(s: &[u8]) -> String {
    return handle_parse(strings(s))
}
// named!(parse_quoted,unimplemented!());
// named!(parse_quasiquote,unimplemented!());
// named!(parse_unquote,unimplemented!());
//
// // parsing numeric values:
// named!(parse_number,unimplemented!());
// named!(parse_rational,unimplemented!());
// named!(parse_decimal,unimplemented!());
// named!(parse_hex,unimplemented!());
// named!(parse_octal,unimplemented!());
// named!(parse_binary,unimplemented!());
// named!(parse_complex,unimplemented!());
// named!(parse_base,unimplemented!());
//
// // Sticking everything together:
// named!(parse_expr,unimplemented!());
