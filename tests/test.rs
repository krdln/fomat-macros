#[macro_use] extern crate pint_macros;

#[test]
fn macro_use() {
    let c: Vec<String> = vec!["a".into()];
    pintln!(
        "<ul>"
        for x in &c { "<li>" (x) "</li>" }
        "</ul>"
    );
}
