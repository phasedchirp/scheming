// extern crate nom;
use num::Complex;
use combine::*;
use std::str::from_utf8;

// Data types:
#[derive(Clone, Debug, PartialEq)]
pub enum LispVal {
    Atom(String),
    List(Vec<LispVal>), // Possibly should be singly-linked list instead?
    DottedList(Vec<LispVal>,Box<LispVal>), // not quite proper definition.
    Vector(Vec<LispVal>), // Scheme standard calls for both but not different from List...
    Number(i64),
    Rational(i64,i64),
    Float(f64),
    ComplexInt(Complex<i64>),
    ComplexFloat(Complex<f64>),
    LString(String),
    LBool(bool),
    LChar(char)
}


pub fn parse_number(s: &str) -> Result<(i64,&str),ParseError<&str>> {
    let mut integer = spaces().with(many1(digit()).map(|string: String| string.parse::<i64>().unwrap()));
    let val = integer.parse(s);
    return val
}

pub fn parse_float(s: &str) -> Result<(f64,&str),ParseError<&str>> {
    let mut float = spaces().with(many1(digit().or(char('.')))).map(|string: String| string.parse::<f64>().unwrap());
    let val = float.parse(s);
    return val
}

// pub fn parse_complex(s: &str) -> Result<(Compex<f64>,&str),ParseError<&str>> {
//     let c = (spaces(),many1(digit()),char('+'),many1(digit()),char('j'));
//     let mut complex_num = spaces().with(many1(digit()).map(|string: String| string.parse::<f64>().unwrap()));
//     let val = complex_num.parse(s);
//     return Complex {re: r, im: j}
// }

// pub fn parse_rational(s: &str) -> Result<(Compex<f64>,&str),ParseError<&str>> {
//     let c = (spaces(),many1(digit()),char('+'),many1(digit()),char('j'));
//     let mut complex_num = spaces().with(many1(digit()).map(|string: String| string.parse::<f64>().unwrap()));
//     let val = complex_num.parse(s);
//     return Complex {re: r, im: j}
// }

// pub fn parse_char(s: &str) -> String

// pub fn parse_atom(s: &str) -> Bool {
//     let res = spaces().
// }

// fn handle_parse(res : IResult<&[u8],&[u8]>) -> String{
//     let matched = match res {
//         IResult::Done(r,m) => from_utf8(m).unwrap(),
//         // Need to clean this up once stuff is working.
//         IResult::Error(_) => {
//             println!("{:?}", res);
//             "parse error"
//         },
//         IResult::Incomplete(_) => "incomplete parse"
//     };
//     return matched.to_string()
// }
//
// // Parsers:
// named!(symbols,alt!(tag!("!")|
//                     tag!("$")|
//                     tag!("%")|
//                     tag!("&")|
//                     tag!("|")|
//                     tag!("*")|
//                     tag!("+")|
//                     tag!("-")|
//                     tag!("/")|
//                     tag!(":")|
//                     tag!("<")|
//                     tag!("=")|
//                     tag!(">")|
//                     tag!("?")|
//                     tag!("@")|
//                     tag!("^")|
//                     tag!("_")|
//                     tag!("~")));
//
//
//     // one_of!(['!','$','%','&','|','*','+','-','/",b":",b"<",b"=",b">",b"?",b"@",b"^",b"_",b"~"]));
//
// // pub fn parse_symbol(s: &[u8]) -> String {
//     // return handle_parse(symbols(s))
// // }
//
// // Parse Booleans
// named!(bools,alt!(tag!("#t") | tag!("#f")));
// pub fn parse_bool(s: &[u8]) -> String {
//     return handle_parse(bools(s))
// }
//
// // Parse Characters
// named!(chars,delimited!(tag!("#\\"),is_not!(" "),many0!(space)));
// pub fn parse_char(s: &[u8]) -> String {
//     return handle_parse(chars(s))
// }
//
// // named!(parse_atom,unimplemented!());
// // named!(lists,delimited!(char!("("),many0!(exprs),char!(")")));
// // named!(parse_vector,unimplemented!());
// // named!(parse_dottedlist,unimplemented!());
//
// // Parse Strings
// named!(strings,delimited!(char!('\"'), is_not!("\""), char!('\"')));
// pub fn parse_string(s: &[u8]) -> String {
//     return handle_parse(strings(s))
// }
// // named!(parse_quoted,unimplemented!());
// // named!(parse_quasiquote,unimplemented!());
// // named!(parse_unquote,unimplemented!());
// //
// // // parsing numeric values:
// named!(numbers,delimited!(many0!(space),digit,many0!(space)));
// pub fn parse_number(s: &[u8]) -> String {
//     return handle_parse(numbers(s))
// }
// named!(parse_rational,unimplemented!());
// named!(decimals,delimited!(tag!("#d"),tuple!(digit,char!('.'),digit),many0!(space)));
// pub fn parse_decimal(s: &[u8]) -> String {
//     let res = decimals(s);
//     let matched = match res {
//         IResult::Done(r,(a,_,b)) => format!("{}.{}",from_utf8(a).unwrap(),from_utf8(b).unwrap()),
//         IResult::Error(_) => {
//             println!("{:?}", res);
//             "parse error".to_string()
//         },
//         IResult::Incomplete(_) => "incomplete parse".to_string()
//     };
//     return matched
// }
// named!(parse_hex,unimplemented!());
// named!(parse_octal,unimplemented!());
// named!(parse_binary,unimplemented!());
// named!(parse_complex,unimplemented!());
// named!(parse_base,unimplemented!());
//
// // Sticking everything together:
// named!(exprs,unimplemented!());
