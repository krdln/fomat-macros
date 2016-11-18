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

/// Writes to a specified writer. Analogous to `write!`.
///
/// See the crate root for general help on the syntax.
///
/// The first argument should be something that implements either `io::Write`
/// or `fmt::Write`. This expression will be evaluated once.
/// The writer can't be a variable name, you have to add `&mut` explicitly,
/// as if you'd be calling a function.
///
/// The list of things to write should be written after
/// the first comma, without any further delimiters.
///
/// # Return value
///
/// This macro returns `io::Result<()>` or `fmt::Result`,
/// just as `write!` from `std`.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// use ::std::io::Write;
/// let mut v = vec![];
/// let world = "World";
/// wite!(&mut v, "Hello, "(world)"!").unwrap();
/// assert_eq!(v, "Hello, World!".as_bytes());
/// # }
/// ```
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
    (@expr.. $w:ident {$($before:tt)*} ($($e:tt)*) {$($block:tt)*} $($rest:tt)* ) => {
        wite!(@rec $w, $($before)* ($($e)*) {$($block)*} $($rest)*)
    };
    (@expr.. $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        wite!(@expr.. $w {$($before)*} ($($expr)* $tt) $($rest)*)
    };
    (@expr $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        wite!(@expr.. $w {$($before)*} ($($expr)* $tt) $($rest)*)
    };

    // recursive parsing -------------------------------------------------------
    // for
    (@rec $w:ident,
        for $p:pat in ($e:expr) { $($body:tt)* }
        sep { $($sep:tt)* }
        $($rest:tt)*
    ) => {
        {
            let mut first_iteration = true;
            for $p in $e {
                if first_iteration {
                    first_iteration = false;
                } else {
                    wite!(@rec $w, $($sep)*);
                }
                wite!(@rec $w, $($body)*);
            }
            wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident,
        for $p:pat in ($e:expr) { $($body:tt)* }
        separated { $($sep:tt)* }
        $($rest:tt)*
    ) => {
        wite!(@rec $w, for $p in ($e) { $($body)* } sep { $($sep)* }$($rest)*)
    };
    (@rec $w:ident, for $p:pat in ($e:expr) { $($body:tt)* } $($rest:tt)*) => {
        wite!(@rec $w, for $p in ($e) { $($body)* } sep {} $($rest)*)
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

/// Writes to a specified writer, with an appended newline. Analogous to `writeln!`.
///
/// See the documentation for [`wite!`](macro.wite.html).
///
/// When there are no arguments, the comma may be omitted.
///
/// # Examples
///
/// ```no_run
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// # use ::std::io::Write;
/// # let mut file = vec![];
/// witeln!(&mut file).unwrap();
/// witeln!(&mut file, "Hi").unwrap();
/// # }
/// ```
#[macro_export]
macro_rules! witeln {
    ($writer:expr, $($arg:tt)*) => { wite!($writer, $($arg)* "\n") };
    ($writer:expr) => { wite!($writer, "\n") };
}

/// Prints to stdout. Analogous to `print!`.
///
/// See the crate root for general help on the syntax.
///
/// # Return value
///
/// The macro returns `()`.
///
/// # Panics
///
/// The macro panics when printing was not successful.
///
/// # Examples
///
/// ```no_run
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// pint!("four = "(2+2));
/// # }
/// ```
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

/// Prints to stdout, with an appended newline. Analoguous to `println!`.
///
/// See the docs for [`print!`](macro.pint.html) for more details.
///
/// # Examples
///
/// ```no_run
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// pintln!();
/// pintln!((2 * 2));
/// # }
/// ```
#[macro_export]
macro_rules! pintln {
    ($($arg:tt)*) => { pint!($($arg)* "\n") }
}

/// Prints to stderr.
///
/// See the crate root for general help on the syntax.
///
/// # Return value
///
/// None
///
/// # Panics
///
/// This macro, in contrary to `pint!`, silently ignores
/// all errors.
///
/// # Examples
///
/// ```no_run
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// perr!("foo")
/// # }
/// ```
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

/// Prints to stderr, with an appended newline.
///
/// See the docs for [`perr!`](macro.perr.html) for more info.
///
/// # Examples
///
/// ```no_run
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// let x = 3;
/// perrln!((=x));
/// # }
/// ```
#[macro_export]
macro_rules! perrln {
    ($($arg:tt)*) => { perr!($($arg)* "\n") }
}

/// Creates a formatted string. Analogous to `format!`.
///
/// See the crate root for general help on the syntax.
///
/// This macro returns `String` containing the formatted text.
///
/// # Panics
///
/// The macro will panic if formatting fails (which shoudn't happen for any
/// of `std` types).
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate pint_macros;
/// # fn main() {
/// let v = vec![1, 2];
///
/// let s = fomat!("Hello, "[v]);
/// assert_eq!(s, "Hello, [1, 2]");
///
/// let s = fomat!(for x in &v { (x*x) ";" });
/// assert_eq!(s, "1;4;");
/// # }
/// ```
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
    (@cap ($($lm:tt)*) sep $($tt:tt)*) => {
        fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) separated $($tt:tt)*) => {
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
fn debug() {
    let v = vec![1,2,3];
    assert_eq!(fomat!([v] "."), "[1, 2, 3].");
}

#[test]
fn test_if() {
    let s = fomat!(
        if true { "A" "A" } else { "X" }
        if false { "X" } else { "D" "D" }
        if true { "T" "T" }
        if false { "X" }
        if let Some(x) = Some(5) { (x) (x) } else { "E" "E" }
        if let None = Some(5) { "X" } else { "F" "F" }
        if let Some(x) = Some(5) { (x) }
        if let None = Some(5) { "X" }
        if {let t = true; t} { "K" }
        "."
    );
    assert_eq!(s, "AADDTT55FF5K.");
}

#[test]
fn format() {
    assert_eq!( fomat!({5:02}), "05" );
    assert_eq!( fomat!({"{}-{}", 4, 2}), "4-2" );
}

#[test]
fn matrix() {
    let matrix = vec![vec![0], vec![1]];
    assert_eq!(
        fomat!( for row in &matrix { for x in row { {x:3} } "\n" } ),
        "  0\n  1\n"
    );
}

#[test]
fn separator() {
    let v = vec![1, 2, 3];
    let s1 = fomat!( for x in &v { (x) } separated { "-" "-" } "." );
    let s2 = fomat!( for x in &v { (x) } sep { "--" } "." );
    assert_eq!(s1, "1--2--3.");
    assert_eq!(s2, "1--2--3.");
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

#[test]
fn depth() {
    let _ = fomat!(
        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
    );
}
