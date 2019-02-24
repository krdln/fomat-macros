//! This crate provides alternative syntax for
//! `write!`, `writeln!`, `print!`, `println!` and `format!` macros.
//! It also introduces macros to print on `stderr`.
//!
//! The names of macros in this crate are formed by
//! removing the letter `r` from their `std` counterparts.
//!
//! Index: [**examples**](#examples) • 
//! [**syntax**](#syntax-overview):
//! [`"string"`](#string-literals),
//! [`()`, `[]`](#expressions-in--and--brackets),
//! [`{}`](#curly-braces),
//! [`for`](#for-loops),
//! [`if`](#if-and-if-let),
//! [`match`](#match),
//! [`=`](#debugging-shorthand) • 
//! [**troubleshooting**](#troubleshooting) • 
//! [**macros**](#macros)
//!
//! # Examples
//!
//! ```
//! use fomat_macros::pintln;
//!
//! fn main() {
//!     pintln!("Hello, World!");
//!     pintln!("Display trait: "(2+2));
//!     pintln!("Debug trait: "[vec![1, 2, 3]]);
//!     pintln!("Multiple " "parameters" (1) " " [2]);
//!
//!     pintln!("Formatting parameters: " {(1./3.):5.2}); // 0.333
//!     pintln!("Debug: "[= 2 + 2]); // Debug: 2 + 2 = 4
//! }
//! ```
//!
//! This crate also contains a small templating language,
//! allowing you to mix constructs like `for` with
//! the printing syntax. The following should print `1 :: 2 :: 3 :: nil`.
//!
//! ```
//! # use fomat_macros::pintln;
//! # fn main() {
//! let list = [1, 2, 3];
//! pintln!( for x in &list { (x) " :: " } "nil" );
//! # }
//! ```
//!
//! You can also use the macros without importing them
//! ```
//! fomat_macros::pintln!("2 + 2 = "(2 + 2));
//! ```
//!
//! # Syntax overview
//!
//! All the macros share the same syntax, so
//! it will be described in this section.
//!
//! The macros take list of *things* to print as an argument.
//! Each *thing* could be either a string literal, something
//! inside brackets (`()`, `[]` or `{}`) or a Rust construct
//! (`for`, `if let`, `if` or `match`). There has to be
//! no separator (like a comma) between those *things*.
//!
//! Whitespace is ignored outside the string literals.
//!
//! ## String literals
//!
//! String literals will be formatted directly as they are.
//! Note that this also applies to `{` and `}` characters.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let s = fomat!("Hi." "{}");
//! assert_eq!(s, "Hi.{}");
//! # }
//! ```
//!
//! ## Expressions in `()` and `[]` brackets.
//!
//! Expressions in these brackets will be evaluated and
//! printed using:
//!
//! * `Display` trait for `(expr)` (equivalent to `{}` format).
//! * `Debug` trait for `[expr]` (equivalent to `{:?}` format).
//!
//! Like in `std`, they are implicitly borrowed.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let s = fomat!( ("string") (2 + 2) ", " [vec![1]] );
//! assert_eq!(s, "string4, [1]")
//! # }
//! ```
//!
//! ## Curly braces
//!
//! ### `write!` passthrough
//!
//! If you want to use regular `format!` syntax for some
//! part of your string, place `format!` arguments
//! inside the curly braces:
//!
//! ```
//! # use fomat_macros::wite;
//! # fn main() {
//! use std::io::Write;
//!
//! let mut v = vec![];
//! wite!(v, "foo " {"{} baz {}", "bar", "quux"});
//! assert_eq!(v, "foo bar baz quux".as_bytes());
//! # }
//! ```
//!
//! ### Single argument
//!
//! If you only want to print a single argument
//! with a custom format parameters,
//! you can use the `{token_tree:format_parameters}`
//! syntax.
//!
//! The following will use binary format,
//! zero-aligned to 8 places.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let s = fomat!({13:08b});
//! assert_eq!(s, "00001101");
//! # }
//! ```
//!
//! Please note that there can be only a single
//! token tree before the colon – usually
//! a literal or an identifier. Anything
//! longer has to be wrapped in parentheses
//! (like that `{(10+3):08b}`).
//!
//! ## For loops
//!
//! For loops use the regular Rust syntax,
//! except the body
//! will use this printing syntax again.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let list = [1, 2, 3];
//! let s = fomat!( for x in &list { (x) " :: " } "nil" );
//! assert_eq!(s, "1 :: 2 :: 3 :: nil");
//! # }
//! ```
//!
//! For loops can also use an optional separator,
//! denoted by `sep` or `separated` keyword.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! # let list = ["a", "b"];
//! let s = fomat!(
//!    for (i, x) in list.iter().enumerate() { (i) " → " (x) }
//!    separated { ", " }
//! );
//! assert_eq!(s, "0 → a, 1 → b");
//! # }
//! ```
//!
//! For loops (and other syntax elements) can also be nested:
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let matrix = [[0, 1], [2, 3]];
//! assert_eq!(
//!     fomat!( for row in &matrix { for x in row { {x:3} } "\n" } ),
//!     "  0  1\n  2  3\n"
//! );
//! # }
//! ```
//!
//! ## If and if let
//!
//! They use the regular Rust syntax,
//! except of the body (inside `{}`),
//! which uses the printing syntax.
//!
//! The benefits of using this syntax instead
//! of getting `if` "outside" of the printing
//! macros is apparent when the conditional is
//! a part of a longer string (you don't
//! have to split this into three separate `write!`s):
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let opt = Some(5);
//! let s = fomat!(
//!     "a\n"
//!     if let Some(x) = opt { (x) "\n" } else { "nothing\n" }
//!     "b\n"
//! );
//! assert_eq!(s, "a\n5\nb\n");
//! # }
//! ```
//!
//! The `else` clause is optional.
//!
//! `else if`-chaining is not supported. As a workaround,
//! use `else { if ... }` or `match`.
//!
//! ## Match
//!
//! Match uses the regular Rust syntax,
//! except arms has to use `{}` blocks,
//! which will be interpreted using printing syntax.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let v = [Some(1), None, Some(2)];
//! let s = fomat!(
//!     for x in &v {
//!         match *x {
//!             Some(x) => { (x) }
//!             None => { "_" }
//!         }
//!     }
//! );
//! assert_eq!(s, "1_2");
//! # }
//! ```
//!
//! Match arms should not be separated by commas.
//!
//! ## Debugging shorthand
//!
//! If you want to print both the expression and the value,
//! place equal sign as a first character in brackets.
//! The trait used to print the value will depend on
//! the kind of brackets used.
//!
//! ```
//! # use fomat_macros::fomat;
//! # fn main() {
//! let word = "foo";
//! let arr = [10];
//! let s = fomat!( (=word) ", " [=&arr] ", " {=5:#b} );
//! assert_eq!(s, "word = foo, &arr = [10], 5 = 0b101");
//! # }
//! ```
//!
//! # Troubleshooting
//!
//! ## Recursion depth
//!
//! If you hit the error about recursion depth,
//! which occurs when you try to print more than
//! about 50 elements, you can use this workaround
//! instead of increasing the limit: split everything
//! into two (or more) dummy `if true` blocks.
//!
//! ## Errors in macro parsing
//!
//! If you hit `expected a literal`, that either means
//! either you've made a syntactic mistake
//! or really a string literal is expected here.
//! Remember, naked identifiers won't be printed
//! unless you put them in parentheses.

use std::fmt;

#[doc(hidden)]
pub struct DisplayOnce<F> {
    closure: std::cell::Cell<Option<F>>
}

impl<F> DisplayOnce<F>
where
    F: FnOnce(&mut fmt::Formatter) -> fmt::Result
{
    pub fn new(f: F) -> Self {
        Self { closure: std::cell::Cell::new(Some(f)) }
    }
}

impl<F> fmt::Display for DisplayOnce<F>
where
    F: FnOnce(&mut fmt::Formatter) -> fmt::Result
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.closure.replace(None).take() {
            Some(closure) => closure(f),
            None => Ok(())
        }
    }
}

/// Wrapper implementing Display for every closure with matching signature
///
/// This wrapper implements Display for every closure implementing
/// `Fn(&mut fmt::Formatter) -> fmt::Result`.
///
/// Can be create using [`lazy_fomat`][lazy_fomat]
pub struct DisplayFn<F>(F);

impl<F> DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result
{
    /// Creates an object which `Display::fmt` impl will call this closure.
    pub fn new(f: F) -> Self { Self(f) }
}


impl<F> fmt::Display for DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(f)
    }
}


/// Writes to a specified writer. Analogous to `write!`.
///
/// See the crate root for general help on the syntax.
///
/// The first argument should be something that implements either `io::Write`
/// or `fmt::Write`. This expression will be evaluated once.
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
/// # use fomat_macros::wite;
/// # fn main() {
/// use ::std::io::Write;
/// use ::std::io::BufWriter;
/// let mut v = vec![];
/// let world = "World";
/// wite!(v, "Hello, "(world)"!").unwrap();
/// wite!(BufWriter::new(&mut v), " "(2+2)).unwrap();
/// assert_eq!(v, "Hello, World! 4".as_bytes());
/// # }
/// ```
#[macro_export]
macro_rules! wite {
    // single tt rules ---------------------------------------------------------
    (@one $w:ident, ($e:expr)) => { ::std::fmt::Display::fmt(&$e, $w) };
    (@one $w:ident, [$e:expr]) => { ::std::fmt::Debug::fmt(&$e, $w) };
    (@one $w:ident, {$e:tt : $($fmt:tt)*}) => {
        write!($w, concat!("{:", $crate::wite!(@stringify-dense $($fmt)*), "}"), $e)
    };
    (@one $w:ident, {$($arg:tt)*}) => {
        write!($w, $($arg)*)
    };
    (@one $w:ident, $string:tt) => { $w.write_str(concat!($string)) };

    (@stringify-dense) => { "" };
    (@stringify-dense $($tt:tt)+) => { concat!( $(stringify!($tt)),+ ) };

    // expression parsing (manually, because we can't use :expr before `{`)
    (@expr.. $w:ident {$($before:tt)*} ($($e:tt)*) {$($block:tt)*} $($rest:tt)* ) => {
        $crate::wite!(@rec $w, $($before)* ($($e)*) {$($block)*} $($rest)*)
    };
    (@expr.. $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        $crate::wite!(@expr.. $w {$($before)*} ($($expr)* $tt) $($rest)*)
    };
    (@expr $w:ident {$($before:tt)*} ($($expr:tt)*) $tt:tt $($rest:tt)* ) => {
        $crate::wite!(@expr.. $w {$($before)*} ($($expr)* $tt) $($rest)*)
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
                    $crate::wite!(@rec $w, $($sep)*);
                }
                $crate::wite!(@rec $w, $($body)*);
            }
            $crate::wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident,
        for $p:pat in ($e:expr) { $($body:tt)* }
        separated { $($sep:tt)* }
        $($rest:tt)*
    ) => {
        $crate::wite!(@rec $w, for $p in ($e) { $($body)* } sep { $($sep)* }$($rest)*)
    };
    (@rec $w:ident, for $p:pat in ($e:expr) { $($body:tt)* } $($rest:tt)*) => {
        $crate::wite!(@rec $w, for $p in ($e) { $($body)* } sep {} $($rest)*)
    };
    (@rec $w:ident, for $p:pat in $($tt:tt)* ) => {
        $crate::wite!(@expr $w { for $p in } () $($tt)*)
    };

    // match
    (@rec $w:ident,
        match ($e:expr) {
            $( $($p:pat)|+ $(if $g:expr)* => { $($body:tt)* } )*
        }
        $($rest:tt)*
    ) => {
        {
            match $e {
                $(
                    $($p)|+ $(if $g)* => {
                        $crate::wite!(@rec $w, $($body)*)
                    }
                )*
            }
            $crate::wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident, match $($tt:tt)* ) => {
        $crate::wite!(@expr $w { match } () $($tt)*)
    };

    // if let
    (@rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        else { $($els:tt)* }
        $($rest:tt)*
    ) => {
        {
            if let $p = $e {
                $crate::wite!(@rec $w, $($then)*);
            } else {
                $crate::wite!(@rec $w, $($els)*);
            }
            $crate::wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        $crate::wite!(@ifelseerror)
    };
    (@rec $w:ident,
        if let $p:pat = ($e:expr) { $($then:tt)* }
        $($rest:tt)*
    ) => {
        $crate::wite!(@rec $w, if let $p = ($e) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if let $p:pat = $($tt:tt)* ) => {
        $crate::wite!(@expr $w { if let $p = } () $($tt)*)
    };

    // if
    (@rec $w:ident,
        if ($cond:expr) { $($then:tt)* }
        else { $($els:tt)* }
        $($rest:tt)*
    ) => {
        {
            if $cond {
                $crate::wite!(@rec $w, $($then)*);
            } else {
                $crate::wite!(@rec $w, $($els)*);
            }
            $crate::wite!(@rec $w, $($rest)*);
        }
    };
    (@rec $w:ident,
        if ($cont:expr) { $($then:tt)* }
        else if $($rest:tt)*
    ) => {
        $crate::wite!(@ifelseerror)
    };
    (@rec $w:ident, if ($cond:expr) { $($then:tt)* } $($rest:tt)* ) => {
        $crate::wite!(@rec $w, if ($cond) { $($then)* } else {} $($rest)*);
    };
    (@rec $w:ident, if $($tt:tt)* ) => {
        $crate::wite!(@expr $w { if } () $($tt)*)
    };

    // equal-sign debugging
    (@rec $w:ident, (= $e:expr) $($rest:tt)*) => {
        $crate::wite!(@rec $w, (concat!(stringify!($e), " = ")) ($e) $($rest)*)
    };
    (@rec $w:ident, [= $e:expr] $($rest:tt)*) => {
        $crate::wite!(@rec $w, (concat!(stringify!($e), " = ")) [$e] $($rest)*)
    };
    (@rec $w:ident, {= $e:tt : $($fmt:tt)*} $($rest:tt)*) => {
        $crate::wite!(@rec $w, (concat!(stringify!($e), " = ")) {$e : $($fmt)*} $($rest)*)
    };

    // single tt
    (@rec $w:ident, $part:tt $($rest:tt)*) => {
        {
            match $crate::wite!(@one $w, $part) {
                Ok(_) => (),
                error => return error,
            }
            $crate::wite!(@rec $w, $($rest)*);
        }
    };

    // terminator
    (@rec $w:ident, ) => { () };

    (@ifelseerror) => {
        {
            let ERROR: () = "`else if` is not supported";
            let NOTE: () = "use `match` or `else { if ... }` instead";
        }
    };

    // entry point -------------------------------------------------------------
    ($writer:expr, $($part:tt)*) => {
        write!(
            $writer,
            "{}",
            $crate::DisplayOnce::new(|f| {
                $crate::wite!(@rec f, $($part)*);
                Ok(())
            })
        )
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
/// # use fomat_macros::witeln;
/// # fn main() {
/// # use ::std::io::Write;
/// # let mut file = vec![];
/// witeln!(file).unwrap();
/// witeln!(file, "Hi").unwrap();
/// # }
/// ```
#[macro_export]
macro_rules! witeln {
    ($writer:expr, $($arg:tt)*) => { $crate::wite!($writer, $($arg)* "\n") };
    ($writer:expr) => { $crate::wite!($writer, "\n") };
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
/// # Behaviour in `#[test]`
///
/// The behaviour when testing is similar to `print!`:
/// the output of this macro will be captured by the
/// testing framework (meaning that by default `cargo test`
/// won't show the output).
///
/// The only limitation and difference from `print!` is
/// that when `pint!` is called by a different crate
/// than the one being tested, the output won't be captured
/// and will allways be printed to stdout.
///
/// # Examples
///
/// ```no_run
/// # use fomat_macros::pint;
/// # fn main() {
/// pint!("four = "(2+2));
/// # }
/// ```
#[macro_export]
macro_rules! pint {
    ($($arg:tt)*) => {
        {
            {
                #[cfg(not(test))] {
                    use ::std::io::Write;
                    let o = ::std::io::stdout();
                    $crate::wite!(o.lock(), $($arg)*).unwrap();
                }
                #[cfg(test)] {
                    print!("{}", $crate::fomat!($($arg)*))
                }
            }
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
/// # use fomat_macros::pintln;
/// # fn main() {
/// pintln!();
/// pintln!((2 * 2));
/// # }
/// ```
#[macro_export]
macro_rules! pintln {
    ($($arg:tt)*) => {
        {
            #[cfg(not(test))] {
                $crate::pint!($($arg)* "\n")
            }
            #[cfg(test)] {
                print!("{}", fomat!($($arg)* "\n"))
            }
        }
    }
}

/// Prints to stderr. Analogous to `eprint!`.
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
/// # use fomat_macros::epint;
/// # fn main() {
/// epint!("foo")
/// # }
/// ```
#[macro_export]
macro_rules! epint {
    ($($arg:tt)*) => {
        {
            use ::std::io::Write;
            let o = ::std::io::stderr();
            $crate::wite!(o.lock(), $($arg)*).unwrap();
        }
    }
}

/// Same as `epint`
#[macro_export]
#[deprecated(since="0.2.1", note="use `epint` instead")]
macro_rules! perr { ($($arg:tt)*) => { $crate::epint!($($arg)*) } }

/// Prints to stderr, with an appended newline. Analogous to `eprintln!`.
///
/// See the docs for [`epint!`](macro.epint.html) for more info.
///
/// # Examples
///
/// ```no_run
/// # use fomat_macros::epintln;
/// # fn main() {
/// let x = 3;
/// epintln!((=x));
/// # }
/// ```
#[macro_export]
macro_rules! epintln {
    ($($arg:tt)*) => { $crate::epint!($($arg)* "\n") }
}

/// Same as `epintln`
#[macro_export]
#[deprecated(since="0.2.1", note="use `epint` instead")]
macro_rules! perrln { ($($arg:tt)*) => { $crate::epintln!($($arg)*) } }

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
/// # use fomat_macros::fomat;
/// # fn main() {
/// let v = [1, 2];
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
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) sep $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) separated $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) if let $p:pat = $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) if $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) else $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };
    (@cap ($($lm:tt)*) match $($tt:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($tt)*)
    };

    // When there's any unconditional string interpolation,
    // we multiply the initial capacity by 2
    // (which would probably happen anyway).
    (@cap ($len:expr, $mul:expr) ($($x:tt)*) $($rest:tt)*) => {
        $crate::fomat!(@cap ($len, 2) $($rest)*)
    };
    (@cap ($len:expr, $mul:expr) [$($x:tt)*] $($rest:tt)*) => {
        $crate::fomat!(@cap ($len, 2) $($rest)*)
    };
    (@cap ($len:expr, $mul:expr) {$($x:tt)*} $($rest:tt)*) => {
        $crate::fomat!(@cap ($len, 2) $($rest)*)
    };

    // Now the only legal tt is a string literal
    (@cap ($len:expr, $mul:expr) $string:tt $($rest:tt)*) => {
        // Concat forces the token to be a string literal.
        $crate::fomat!(@cap ($len + concat!($string).len(), $mul) $($rest)*)
    };

    // Ignores everything till after next block
    (@cap-ignore ($($lm:tt)*) { $($block:tt)* } $($rest:tt)*) => {
        $crate::fomat!(@cap ($($lm)*) $($rest)*)
    };
    (@cap-ignore ($($lm:tt)*) $tt:tt $($rest:tt)*) => {
        $crate::fomat!(@cap-ignore ($($lm)*) $($rest)*)
    };

    // entry points ------------------------------------------------------------
    () => { String::new() };
    ($($arg:tt)*) => {
        {
            use ::std::fmt::Write;
            let (len, mul) = $crate::fomat!(@cap (0, 1) $($arg)*);
            let mut _s = String::with_capacity(len * mul);
            $crate::wite!(_s, $($arg)*).ok();
            _s
        }
    }
}

/// Creates a displayable object based on its arguments.
///
/// This macro works in a similar way to [`fomat`](fomat),
/// but instead of `String` it returns an object ([`DisplayFn`](DisplayFn))
/// that implements [`Display`][std::fmt::Display] and can be printed later
/// (using `format`, `fomat` or calling `Display::fmt` directly).
///
/// See the [crate root](crate) for general help on the syntax.
///
/// Prefix the arguments with `move` to force moving all variables
/// (can help when `'static` bound is required).
///
/// # Examples
///
/// Direct usage
///
/// ```
/// # use fomat_macros::{fomat, lazy_fomat};
/// let fence = lazy_fomat!(for _ in 0..5 { "-" });
/// let s = fomat!((fence)" hello "(fence));
///
/// assert_eq!(s, "----- hello -----");
/// ```
///
/// Returning `impl Display`
///
/// ```
/// # use fomat_macros::lazy_fomat;
/// fn greet(name: String) -> impl ::std::fmt::Display {
///     lazy_fomat!(move "Hello, "(name)"!")
/// }
///
/// assert_eq!(greet("World".into()).to_string(), "Hello, World!");
/// ```
#[macro_export]
macro_rules! lazy_fomat {
    (move $($arg:tt)*) => {
        $crate::DisplayFn::new(move |f| {
            $crate::wite!(@rec f, $($arg)*);
            Ok(())
        })
    };
    ($($arg:tt)*) => {
        $crate::DisplayFn::new(|f| {
            $crate::wite!(@rec f, $($arg)*);
            Ok(())
        })
    };
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
    let v = [1,2,3];
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
fn separator() {
    let v = [1, 2, 3];
    let s1 = fomat!( for x in &v { (x) } separated { "-" "-" } "." );
    let s2 = fomat!( for x in &v { (x) } sep { "--" } "." );
    assert_eq!(s1, "1--2--3.");
    assert_eq!(s2, "1--2--3.");
}

#[test]
fn test_match() {
    let s = fomat!(
        match Some(5) {
            Some(x) if x > 3 => { (x) "!" }
            Some(2) | None => {}
            _ => {}
        }
        "."
    );
    assert_eq!(s, "5!.");
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
    let v = [10];
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

#[test]
fn non_static_writer() {
    use std::io::Write;
    use std::io::Result;
    use std::fmt::Arguments;

    struct Prepender<'a, T: Write> {
        prefix: &'a str,
        writer: T,
    }

    impl<'a, T: Write> Write for Prepender<'a, T> {
        fn write(&mut self, buf: &[u8]) -> Result<usize> {
            self.writer.write(buf)
        }

        fn flush(&mut self) -> Result<()> {
            self.writer.flush()
        }

        fn write_fmt(&mut self, fmt: Arguments) -> Result<()> {
            self.writer.write_all(self.prefix.as_bytes())?;
            self.writer.write_fmt(fmt)
        }
    }

    let mut buf = vec![];
    witeln!(
        Prepender { prefix: &"foo ".to_owned(), writer: &mut buf },
        (2+2)
    ).unwrap();
    assert_eq!(buf, "foo 4\n".as_bytes());
}

#[test]
fn no_semicolon() {
    if true { pint!("foo") } else { epint!("bar") }
    pintln!("foo" "bar")
}

#[test]
fn move_and_borrow() {
    // Test if fomat! arguments can move some and borrow other variables.
    let iter = vec![1, 2, 3].into_iter();
    let borrow_me = vec![1, 2, 3];
    let s = fomat!(for x in iter { (x) } (borrow_me.len()));
    assert_eq!(s, "1233");
    assert_eq!(borrow_me, [1, 2, 3]);
}
