extern crate tempdir;
extern crate tar;

use std::path::Path;

const FILES: &'static [(&'static str, &'static str)] = &[
    ("Cargo.toml", r#"
        [package]
        name = "pint"
        version = "0.1.0"

        [dependencies]
        fomat-macros = { path = "PATH" }
    "#),
    ("src/main.rs", r#"
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
    "#),
    ("src/lib.rs", r#"
        #[macro_use] extern crate fomat_macros;

        pub fn called_from_other_crate() {
            pintln!("nocapture"(1));
        }
    "#),
    ("tests/nocapture.rs", r#"
        extern crate pint;

        #[test]
        fn nocapture() {
            pint::called_from_other_crate();
        }
    "#),
];

fn unpack_files(directory: &Path, replace_path: &str) {
    use tar::{Builder, Header, Archive};

    let mut builder = Builder::new(Vec::new());
    for &(name, data) in FILES {
        let data = data.replace("PATH", replace_path);
        let mut header = Header::new_gnu();
        header.set_path(name).unwrap();
        header.set_size(data.len() as u64);
        header.set_cksum();
        builder.append(&header, data.as_bytes()).unwrap();
    }

    let archive: &[u8] = &builder.into_inner().unwrap();
    Archive::new(archive).unpack(directory)
        .expect("Can't unpack test crate");
}

#[test]
fn capturing() {
    use std::process::Command;
    use std::env;
    use std::str::from_utf8;

    let rootdir = {
        let mut rootdir = env::current_exe().unwrap();
        while rootdir.file_name() != Some("target".as_ref()) {
            assert!( rootdir.pop() );
        }
        assert!( rootdir.pop() );
        rootdir
    };

    let pintdir = tempdir::TempDir::new("fomat-macros-capturing-test")
        .expect("Can't create tempdir");
    unpack_files(pintdir.as_ref(), rootdir.to_str().unwrap());

    assert!(
        Command::new("cargo").arg("build")
            .current_dir(&pintdir)
            .status().unwrap().success()
    );

    let expected_stdout = "stdout1stdout2\n";
    let expected_stderr = "stderr1stderr2\n";

    let output = Command::new("target/debug/pint")
        .current_dir(&pintdir)
        .output().unwrap();
    assert!(output.status.success());
    assert_eq!(from_utf8(&output.stdout).unwrap(), expected_stdout);
    assert_eq!(from_utf8(&output.stderr).unwrap(), expected_stderr);

    let output = Command::new("cargo")
        .arg("test")
        .current_dir(&pintdir)
        .output().unwrap();
    assert!(output.status.success());
    assert!(!from_utf8(&output.stdout).unwrap().contains(expected_stdout));
    assert!(from_utf8(&output.stderr).unwrap().contains(expected_stderr));
    assert!(from_utf8(&output.stdout).unwrap().contains("nocapture1"));

    let output = Command::new("cargo")
        .args(&["test", "--", "--nocapture"])
        .current_dir(&pintdir)
        .output().unwrap();
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().contains(expected_stdout));
    assert!(from_utf8(&output.stderr).unwrap().contains(expected_stderr));
}
