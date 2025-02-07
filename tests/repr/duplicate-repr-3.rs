use scryer_modular_bitfield::prelude::*;

#[bitfield]
#[repr(u32)]
#[cfg_attr(not(test), repr(u32))]
pub struct SignedInt {
    sign: bool,
    value: B31,
}

fn main() {}
