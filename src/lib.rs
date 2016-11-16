macro_rules! swrite {
    (@one $w:ident, ($e:expr)) => { write!($w, "{}", $e) };
    (@one $w:ident, [$e:expr]) => { write!($w, "{:?}", $e) };
    (@one $w:ident, {$e:tt : $($fmt:tt)*}) => {
        write!($w, concat!("{:", stringify!($($fmt)*), "}"), $e)
    };
    (@one $w:ident, {$($arg:tt)*}) => {
        write!($w, $($arg)*)
    };
    (@one $w:ident, $string:tt) => {
        write!($w, "{}", concat!($string))
    };

    (@rec $w:ident, for $p:pat in ($e:expr) { $($body:tt)* } $($rest:tt)* ) => {
        {
            for $p in $e {
                swrite!(@rec $w, $($body)*);
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, for $($rest:tt)* ) => {
        {
            let NOTE: () = "use parens around expression: for PAT in (EXPR) { BODY }";
        }
    };
    (@rec $w:ident, if ($cond:expr) { $($then:tt)* } else { $($els:tt)* } $($rest:tt)* ) => {
        {
            if $cond {
                swrite!(@rec $w, $($then)*);
            } else {
                swrite!(@rec $w, $($els)*);
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, if ($cont:expr) { $($then:tt)* } else if $($rest:tt)* ) => {
        {
            let ERROR: () = "`else if` is unsupported";
            let NOTE: () = "use `match` or `else { if ... }` instead";
        }
    };
    (@rec $w:ident, if ($cond:expr) { $($then:tt)* } $($rest:tt)* ) => {
        swrite!(@rec $w, if ($cond) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if $($rest:tt)* ) => {
        {
            let () = "note: use parens around expression: if (EXPR) { BODY }";
        }
    };
    (@rec $w:ident, $part:tt $($rest:tt)*) => {
        {
            match swrite!(@one $w, $part) {
                Ok(_) => (),
                error => return error,
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, ) => { () };

    ($writer:expr, $($part:tt)*) => {
        (||{
            let mut _w = $writer;
            swrite!(@rec _w, $($part)*);
            Ok(())
        })()
    };
    ($writer:expr) => { swrite!($writer,) };
}

macro_rules! swriteln {
    ($($arg:tt)*) => { swrite!($($arg)* "\n") }
}

macro_rules! sprint {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stdout();
            swrite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

macro_rules! sprintln {
    ($($arg:tt)*) => { sprint!($($arg)* "\n") }
}

macro_rules! sprerr {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stderr();
            swrite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

macro_rules! sprerrln {
    ($($arg:tt)*) => { sprerr!($($arg)* "\n") }
}

macro_rules! sformat {
    () => { String::new() };
    ($($arg:tt)*) => {
        {
            use ::std::fmt::Write;
            let mut _s = String::new(); // TODO with capacity
            swrite!(&mut _s, $($arg)*).ok();
            _s
        }
    }
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
    swriteln!(&mut v, "hi" "!").unwrap();
    assert_eq!(v, "hi!\n".as_bytes());
}

#[test]
fn format() {
    assert_eq!( sformat!({5:02}), "05" );
    assert_eq!( sformat!({"{}-{}", 4, 2}), "4-2" );
}

#[test]
fn hello() {
    let foo = "foo";
    let x = 2;
    let y = 4;
    sprintln!("Bar "(foo)" and "(x));
    sprintln!( (x)"<"(y) );
    sprintln!( for x in (&[1,2,3]) { (x)" :: " } "nil" );
}

#[test]
fn matrix() {
    let matrix = vec![vec![0]];
    assert_eq!(sformat!( for row in (&matrix) { for x in (row) { {x:3} } "\n" } ), "  0\n");
}
