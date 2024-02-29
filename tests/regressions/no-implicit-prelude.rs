// Checks that no implicit paths are generated by the `#[bitfield]` proc. macro
// and `#[derive(BitfieldSpecifier)]` derive macro.

#![no_implicit_prelude]

use ::scryer_modular_bitfield::prelude::*;

#[bitfield]
pub struct TestBitfield {
    a: ::core::primitive::bool,
    b: ::scryer_modular_bitfield::specifiers::B3,
    c: ::scryer_modular_bitfield::specifiers::B4,
    d: ::scryer_modular_bitfield::specifiers::B24,
}

#[derive(BitfieldSpecifier, Debug)]
pub enum TestSpecifier {
    A = 0,
    B = 1,
}

fn main() {}
