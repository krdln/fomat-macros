extern crate pint;

#[test]
fn nocapture() {
    pint::called_from_other_crate();
}
