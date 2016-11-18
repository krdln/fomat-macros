#[macro_use] extern crate fomat_macros;

#[test]
fn macro_use() {
    let c: Vec<String> = vec!["a".into()];
    let _ = fomat!(
        "<ul>"
        for x in &c { "<li>" (x) "</li>" }
        "</ul>"
    );
}
