#[macro_use] extern crate fomat_macros;

fn foo() {
    pint!("stdout"(1));
    pintln!("stdout"(2));
    perr!("stderr"(1));
    perrln!("stderr"(2));
}

fn main() {
    foo();
}

#[test]
fn capturing() {
    foo();
}
