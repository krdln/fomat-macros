#[macro_use] extern crate fomat_macros;

pub fn called_from_other_crate() {
    pintln!("nocapture"(1));
}

