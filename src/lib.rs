use std::io;

#[doc(hidden)]
// Shortcut for writing string literals,
// so they don't go through write_fmt machinery.
pub trait WriteStrExt {
    type Result;
    fn write_str(&mut self, s: &str) -> Self::Result;
}

// The fmt::Write already has this method,
// so we implement it only for W: io::Write.
impl<W: io::Write> WriteStrExt for W {
    type Result = io::Result<()>;
    fn write_str(&mut self, s: &str) -> Self::Result {
        self.write_all(s.as_bytes())
    }
}

#[macro_export]
macro_rules! wite {
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
        {
            #[allow(unused_imports)]
            use $crate::WriteStrExt;

            $w.write_str(concat!($string))
        }
    };

    // expression parsing (manually, because we can't use :expr before `{`)
    (@expr $w:ident {$($before:tt)*} ($($e:tt)*) {$($block:tt)*} $($rest:tt)* ) => {
        wite!(@rec $w, $($before)* ($($e)*) {$($block)*} $($rest)*)
    };
    (@expr $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        wite!(@expr $w {$($before)*} ($($expr)* $tt) $($rest)*)
    };

    // recursive parsing -------------------------------------------------------
    // for
    (@rec $w:ident, for $p:pat in ($e:expr) { $($body:tt)* } $($rest:tt)* ) => {
        {
            for $p in $e {
                wite!(@rec $w, $($body)*);
            }
            wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, for $p:pat in $($tt:tt)* ) => {
        wite!(@expr $w { for $p in } () $($tt)*)
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
                        wite!(@rec $w, $($body)*)
                    }
                )*
            }
            wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, match $($tt:tt)* ) => {
        wite!(@expr $w { match } () $($tt)*)
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
                wite!(@rec $w, $($then)*);
            } else {
                wite!(@rec $w, $($els)*);
            }
            wite!(@rec $w, $($rest)*);
        }
    };
    (
        @rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        wite!(@ifelseerror)
    };
    (
        @rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        $($rest:tt)*
    ) => {
        wite!(@rec $w, if let $p = ($e) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if let $p:pat = $($tt:tt)* ) => {
        wite!(@expr $w { if let $p = } () $($tt)*)
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
                wite!(@rec $w, $($then)*);
            } else {
                wite!(@rec $w, $($els)*);
            }
            wite!(@rec $w, $($rest)*);
        }
    };
    (
        @rec $w:ident,
        if ($cont:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        wite!(@ifelseerror)
    };
    (@rec $w:ident, if ($cond:expr) { $($then:tt)* } $($rest:tt)* ) => {
        wite!(@rec $w, if ($cond) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if $($tt:tt)* ) => {
        wite!(@expr $w { if } () $($tt)*)
    };

    // equal-sign debugging
    (@rec $w:ident, (= $e:expr) $($rest:tt)*) => {
        wite!(@rec $w, (concat!(stringify!($e), " = ")) ($e) $($rest)*)
    };
    (@rec $w:ident, [= $e:expr] $($rest:tt)*) => {
        wite!(@rec $w, (concat!(stringify!($e), " = ")) [$e] $($rest)*)
    };
    (@rec $w:ident, {= $e:tt : $($fmt:tt)*} $($rest:tt)*) => {
        wite!(@rec $w, (concat!(stringify!($e), " = ")) {$e : $($fmt)*} $($rest)*)
    };

    // single tt
    (@rec $w:ident, $part:tt $($rest:tt)*) => {
        {
            match wite!(@one $w, $part) {
                Ok(_) => (),
                error => return error,
            }
            wite!(@rec $w, $($rest)*);
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
            if false {
                // This dummy write's purpose is to silence a warning
                // about "unused import `Write`". We can't just
                // do `let _: Option<&Write> = None;` though, because
                // in case when writing to Formatter, the `Write` trait
                // doesn't have to be in scope.
                let _ = write!(_w, "");
            }
            wite!(@rec _w, $($part)*);
            Ok(())
        })()
    };
}

#[macro_export]
macro_rules! witeln {
    ($($arg:tt)*) => { wite!($($arg)* "\n") }
}

#[macro_export]
macro_rules! pint {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stdout();
            wite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

#[macro_export]
macro_rules! pintln {
    ($($arg:tt)*) => { pint!($($arg)* "\n") }
}

#[macro_export]
macro_rules! perr {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stderr();
            wite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

#[macro_export]
macro_rules! perrln {
    ($($arg:tt)*) => { perr!($($arg)* "\n") }
}

#[macro_export]
macro_rules! fomat {
    // capacity estimation -----------------------------------------------------
    (@cap ($len:expr, $multiplier:expr)) => {
        ($len, $multiplier)
    };

    // skip all irrelevant tts and conditional bodies
    (@cap ($($lm:tt)*) for $p:pat in $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) if let $p:pat = $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) if $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) else $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) match $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };

    // When there's any unconditional string interpolation,
    // we multiply the initial capacity by 2
    // (which would probably happen anyway).
    (@cap ($len:expr, $mul:expr) ($($x:tt)*) $($rest:tt)*) => {
        fomat!(@cap ($len, 2) $($rest)*)
    };
    (@cap ($len:expr, $mul:expr) [$($x:tt)*] $($rest:tt)*) => {
        fomat!(@cap ($len, 2) $($rest)*)
    };
    (@cap ($len:expr, $mul:expr) {$($x:tt)*} $($rest:tt)*) => {
        fomat!(@cap ($len, 2) $($rest)*)
    };

    // Now the only legal tt is a string literal
    (@cap ($len:expr, $mul:expr) $string:tt $($rest:tt)*) => {
        // Concat forces the token to be a string literal.
        fomat!(@cap ($len + concat!($string).len(), $mul) $($rest)*)
    };

    // Ignores everything till after next block
    (@cap-ignore ($($lm:tt)*) { $($block:tt)* } $($rest:tt)*) => {
        fomat!(@cap ($($lm)*) $($rest)*)
    };
    (@cap-ignore ($($lm:tt)*) $tt:tt $($rest:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($rest)*)
    };

    // entry points ------------------------------------------------------------
    () => { String::new() };
    ($($arg:tt)*) => {
        {
            use ::std::fmt::Write;
            let (len, mul) = fomat!(@cap (0, 1) $($arg)*);
            let mut _s = String::with_capacity(len * mul);
            wite!(&mut _s, $($arg)*).ok();
            _s
        }
    }
}

#[test]
fn basics() {
    let world = "World";
    assert_eq!(fomat!("Hello, "(world)"!"), "Hello, World!");
    let x = 3;
    assert_eq!(fomat!((x)" * 2 = "(x * 2)), "3 * 2 = 6");
}

#[test]
fn empty() {
    assert_eq!(fomat!(), "");
}

#[test]
fn vec() {
    let v = vec![1,2,3];
    assert_eq!(fomat!([v]), "[1, 2, 3]");
}

#[test]
fn write() {
    use std::io::Write;
    let mut v = Vec::new();
    witeln!(&mut v, "hi" "!").unwrap();
    assert_eq!(v, "hi!\n".as_bytes());
}

#[test]
fn format() {
    assert_eq!( fomat!({5:02}), "05" );
    assert_eq!( fomat!({"{}-{}", 4, 2}), "4-2" );
}

#[test]
fn hello() {
    let foo = "foo";
    let x = 2;
    let y = 4;
    pintln!("Bar "(foo)" and "(x));
    pintln!( (x)" < "(y) );
    pintln!( for x in (&[1,2,3]) { (x)" :: " } "nil" );
}

#[test]
fn matrix() {
    let matrix = vec![vec![0]];
    assert_eq!(
        fomat!( for row in &matrix { for x in row { {x:3} } "\n" } ),
        "  0\n"
    );
}

#[test]
fn boo() {
    let a = Some(5);
    pintln!(if let Some(_) = a { "yes" });
}

#[test]
fn test_match() {
    let s = fomat!(
        match Some(5) {
            Some(x) if x > 3 => { (x) }
            Some(2) | None => {}
            _ => {}
        }
        "."
    );
    assert_eq!(s, "5.");
}

#[test]
fn capacity() {
    assert_eq!(fomat!("Hello, " "world!").capacity(), 13);
    assert_eq!(fomat!("Hello, "[40+2]).capacity(), 14);
    let s = fomat!(
        "Hello"
        for x in [1][1..].iter() { (x) "a" }
        if let Some(()) = None { "b" }
        if false { "c" } else {}
        match 1 { 2 => { "e" } _ => {} }
        "!"
    );
    assert_eq!(s.capacity(), 6);
}

#[test]
fn fmt_write() {
    use std::fmt;
    struct Foo;

    impl fmt::Display for Foo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            wite!(f, "foo"(42))
        }
    }

    assert_eq!(format!("{}", Foo), "foo42");
}

#[test]
fn equal_sign() {
    let x = 5;
    let v = vec![10];
    assert_eq!(fomat!((=x) "."), "x = 5.");
    assert_eq!(fomat!([=&v] "."), "&v = [10].");
    assert_eq!(fomat!({=13:05b} "."), "13 = 01101.");
}
