use scryer_modular_bitfield::prelude::*;

#[bitfield]
pub struct Sparse {
    #[skip(setters)] #[skip(setters)]
    unused_1: B10,
    a: bool,
    #[skip]
    unused_2: B10,
    b: bool,
    #[skip]
    unused_3: B10,
}

fn main() {}
