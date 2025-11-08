use crate::prelude::*;

pub fn panic(_cx: Context<'_>) -> HebiResult<()> {
    Err(error("explicit panic"))
}
