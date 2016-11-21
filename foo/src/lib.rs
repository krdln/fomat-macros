#[macro_use] extern crate fomat_macros;

pub fn lib_function() {
    pintln!("running: " "lib_function");
}

#[test]
pub fn unit_test() {
    pintln!("running: " "unit_test");
}
