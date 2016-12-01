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

#[test]
fn printing() {
    use std::process::Command;
    use std::env;
    use std::str::from_utf8;

    let rootdir = env::current_exe().unwrap();
    let rootdir = rootdir.parent().unwrap(); // deps
    let rootdir = rootdir.parent().unwrap(); // debug
    let rootdir = rootdir.parent().unwrap(); // target
    let rootdir = rootdir.parent().unwrap();

    let pintdir = rootdir.join("tests").join("pint");

    assert!(
        Command::new("cargo").arg("update")
            .current_dir(&pintdir)
            .status().unwrap().success()
    );

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

    assert!(
        Command::new("cargo").arg("clean")
            .current_dir(&pintdir)
            .status().unwrap().success()
    );
}
