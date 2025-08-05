pub mod runtime;

pub struct Value {
    pub tag: u64,
    pub data: u64,
}

pub struct Context {}

pub struct Literal {}

pub enum Control {}
