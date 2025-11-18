use crate::prelude::*;

pub fn panic() -> HebiResult<()> {
    Err(error("explicit panic"))
}
