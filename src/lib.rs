macro_rules! swrite {
    (@one $writer:expr, ($e:expr)) => { write!($writer, "{}", $e) };
    (@one $writer:expr, [$e:expr]) => { write!($writer, "{:?}", $e) };
    (@one $writer:expr, {$e:tt : $($fmt:tt)*}) => {
        write!($writer, concat!("{:", stringify!($($fmt)*), "}"), $e)
    };
    (@one $writer:expr, {$($arg:tt)*}) => {
        write!($writer, $($arg)*)
    };
    (@one $writer:expr, $string:tt) => {
        write!($writer, "{}", { let s: &str = $string; s })
    };
    ($writer:expr, $($part:tt)*) => {
        (||{
            let mut _w = $writer;
            $(
                try!( swrite!(@one _w, $part) );
            )*
            let ret: ::std::fmt::Result = Ok(());
            ret
        })()
    };
    ($writer:expr) => { swrite!($writer,) };
}

macro_rules! swriteln {
    ($($arg:tt)*) => { swrite!($($arg)* "\n") }
}

macro_rules! sprint {
    ($($arg:tt)*) => { {
        let o = ::std::io::stdout();
        swrite!(o.lock(), $($arg)*).unwrap();
    } }
}

macro_rules! sprintln {
    ($($arg:tt)*) => { sprint!($($arg)* "\n") }
}

macro_rules! sformat {
    () => { String::new() };
    ($($arg:tt)*) => { {
        use ::std::fmt::Write;
        let mut _s = String::new(); // TODO with capacity
        swrite!(&mut _s, $($arg)*).ok();
        _s
    } }
}

#[test]
fn basics() {
    let world = "World";
    assert_eq!(sformat!("Hello, "(world)"!"), "Hello, World!");
    let x = 3;
    assert_eq!(sformat!((x)" * 2 = "(x * 2)), "3 * 2 = 6");
}

#[test]
fn empty() {
    assert_eq!(sformat!(), "");
}

#[test]
fn vec() {
    let v = vec![1,2,3];
    assert_eq!(sformat!([v]), "[1, 2, 3]");
}

#[test]
fn write() {
    use std::io::Write;
    let mut v = Vec::new();
    writeln!(&mut v, "hi!").unwrap();
    assert_eq!(v, "hi!\n".as_bytes());
}

#[test]
fn format() {
    assert_eq!( sformat!({5:02}), "05" );
    assert_eq!( sformat!({"{}-{}", 4, 2}), "4-2" );
}
