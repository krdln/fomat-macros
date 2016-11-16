#[macro_export]
macro_rules! swrite {
    // single tt rules ---------------------------------------------------------
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

    // expression parsing (manually, because we can't use :expr before `{`)
    (@expr $w:ident {$($before:tt)*} ($($e:tt)*) {$($block:tt)*} $($rest:tt)* ) => {
        swrite!(@rec $w, $($before)* ($($e)*) {$($block)*} $($rest)*)
    };
    (@expr $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        swrite!(@expr $w {$($before)*} ($($expr)* $tt) $($rest)*)
    };

    // recursive parsing -------------------------------------------------------
    // for
    (@rec $w:ident, for $p:pat in ($e:expr) { $($body:tt)* } $($rest:tt)* ) => {
        {
            for $p in $e {
                swrite!(@rec $w, $($body)*);
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, for $p:pat in $($tt:tt)* ) => {
        swrite!(@expr $w { for $p in } () $($tt)*)
    };

    // match
    (
        @rec $w:ident,
        match ($e:expr) {
            $( $($p:pat)|+ $(if $g:expr)* => { $($body:tt)* } )*
        }
        $($rest:tt)*
    ) => {
        {
            match $e {
                $(
                    $($p)|+ $(if $g)* => {
                        swrite!(@rec $w, $($body)*)
                    }
                )*
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, match $($tt:tt)* ) => {
        swrite!(@expr $w { match } () $($tt)*)
    };

    // if let
    (
        @rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        else { $($els:tt)* }
        $($rest:tt)*
    ) => {
        {
            if let $p = $e {
                swrite!(@rec $w, $($then)*);
            } else {
                swrite!(@rec $w, $($els)*);
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (
        @rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        swrite!(@ifelseerror)
    };
    (
        @rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        $($rest:tt)*
    ) => {
        swrite!(@rec $w, if let $p = ($e) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if let $p:pat = $($tt:tt)* ) => {
        swrite!(@expr $w { if let $p = } () $($tt)*)
    };

    // if
    (
        @rec $w:ident,
        if ($cond:expr) { $($then:tt)* }
        else { $($els:tt)* }
        $($rest:tt)*
    ) => {
        {
            if $cond {
                swrite!(@rec $w, $($then)*);
            } else {
                swrite!(@rec $w, $($els)*);
            }
            swrite!(@rec $w, $($rest)*);
        }
    };
    (
        @rec $w:ident,
        if ($cont:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        swrite!(@ifelseerror)
    };
    (@rec $w:ident, if ($cond:expr) { $($then:tt)* } $($rest:tt)* ) => {
        swrite!(@rec $w, if ($cond) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if $($tt:tt)* ) => {
        swrite!(@expr $w { if } () $($tt)*)
    };

    // single tt
    (@rec $w:ident, $part:tt $($rest:tt)*) => {
        {
            match swrite!(@one $w, $part) {
                Ok(_) => (),
                error => return error,
            }
            swrite!(@rec $w, $($rest)*);
        }
    };

    // terminator
    (@rec $w:ident, ) => { () };

    (@ifelseerror) => {
        {
            let ERROR: () = "`else if` is unsupported";
            let NOTE: () = "use `match` or `else { if ... }` instead";
        }
    };

    // entry point -------------------------------------------------------------
    ($writer:expr, $($part:tt)*) => {
        (||{
            let mut _w = $writer;
            swrite!(@rec _w, $($part)*);
            Ok(())
        })()
    };
    ($writer:expr) => { swrite!($writer,) };
}

#[macro_export]
macro_rules! swriteln {
    ($($arg:tt)*) => { swrite!($($arg)* "\n") }
}

#[macro_export]
macro_rules! sprint {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stdout();
            swrite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

#[macro_export]
macro_rules! sprintln {
    ($($arg:tt)*) => { sprint!($($arg)* "\n") }
}

#[macro_export]
macro_rules! sprerr {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stderr();
            swrite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

#[macro_export]
macro_rules! sprerrln {
    ($($arg:tt)*) => { sprerr!($($arg)* "\n") }
}

#[macro_export]
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
    sprintln!( (x)" < "(y) );
    sprintln!( for x in (&[1,2,3]) { (x)" :: " } "nil" );
}

#[test]
fn matrix() {
    let matrix = vec![vec![0]];
    assert_eq!(
        sformat!( for row in &matrix { for x in row { {x:3} } "\n" } ),
        "  0\n"
    );
}

#[test]
fn boo() {
    let a = Some(5);
    sprintln!(if let Some(_) = a { "yes" });
}

#[test]
fn test_match() {
    let s = sformat!(
        match Some(5) {
            Some(x) if x > 3 => { (x) }
            Some(2) | None => {}
            _ => {}
        }
        "."
    );
    assert_eq!(s, "5.");
}
