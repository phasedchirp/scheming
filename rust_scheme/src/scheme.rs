extern crate nom;

// Data types:
#[derive(Clone, Debug, PartialEq)]
pub enum LispVal {
    Atom(String),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>,Box<LispVal>),
    Number(u64),
    LString(String),
    LBool(bool),
    LChar(char)
}
