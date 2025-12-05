// use crate::prelude::*;

pub fn powi(v: i64, exp: i64) -> i64 {
    v.pow(exp as u32)
}

pub fn powf(v: f64, exp: f64) -> f64 {
    v.powf(exp)
}
