// extern crate nom;
use num::Complex;
use nom::*;

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

// Parsers:
// named!(parse_symbol,one_of!("!$%&|*+-/:<=>?@^_~"));
// named!(parse_bool,alt!(tag!("#t") | tag!("#f")));
// named!(parse_atom,unimplemented!());
// named!(parse_list,unimplemented!());
// named!(parse_vector,unimplemented!());
// named!(parse_dottedlist,unimplemented!());
// named!(parse_string,unimplemented!());
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
