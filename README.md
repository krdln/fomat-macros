# fomat-macros

[**documentation**](https://docs.rs/fomat-macros),
[**crate**](https://crates.io/crates/fomat-macros)

This crate provides alternative syntax for
`write!`, `writeln!`, `print!`, `println!`, `eprint!`, `eprintln!` and `format!` macros
from the [Rust](https://www.rust-lang.org/) standard library.

The names of macros in this crate
are formed by removing the letter `r` from their `std` counterparts:
`wite!`, `witeln!`, `pint!`, `pintln!`,
`epint!`, `epintln!`, `fomat!`.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
fomat-macros = "0.3.1"
```

And `use` the macros in your `.rs` file, eg.:

```rust
use fomat_macros::pintln;
```

This version requires Rust 1.30. For support for older versions,
[see version 0.2.1](https://github.com/krdln/fomat-macros/tree/v0.2.1).

## Examples

```rust
pintln!("Hello, world!");
pintln!(); // empty line
pintln!("The answer is "(40 + 2)); // parentheses use the Display trait
pintln!([vec![1, 2, 3]] " -- numbers"); // brackets use the Debug trait
```

As you can see, instead the format string and arguments,
we have a list of *things* to print
without any separators.
Each *thing* may be a string literal or an expression in brackets
(apart of `()` and `[]` there are also braces `{}`, which may
be used for more advanced format specifiers, see the docs).

You can also use `if`, `if let`, `match` and `for` constructs
inside the macro. They use regular Rust syntax, except
everything inside the `{}` blocks will use the
list-of-things-to-print syntax again.

```rust
let list = vec![1, 2, 3];
let s = fomat!( for x in &list { (x) " :: " } "nil" );
// s == "1 :: 2 :: 3 :: nil"
```

For loops can also use an optional separator.
[For details, see the docs.](https://docs.rs/fomat-macros/0.2/fomat_macros/#for-loops)

There's also a shorthand for debugging, which prints both
the expression and value. To enable, put `=` as the first
character inside the any kind of brackets.

```rust
let list = vec![1, 2, 3];
epintln!([=list]); // prints list = [1, 2, 3]
```

## Why?

What was the motivation to create this crate?

* More locality – everything is written in the same order
  it will be printed. But that might a personal preference.
* Easier to refactor – especially when you suddenly want
  to add a conditional print inside a long format string. Compare:

  ```rust
  let s = fomat!(
      "first line\n"
      if condition() { (foo) }
      "third line\n"
  );
  ```

  ```rust
  let s = {
      use ::std::fmt::Write;
      let mut s = "first line\n".to_owned();
      if condition() { write!(s, "{}", foo).unwrap() }
      write!(s, "third line\n").unwrap();
      s
  };
  ```

* Speed! `fomat!` may be faster than `format!`
  (see `cargo bench`). That's because there's
  just one virtual call for single invocation
  of `fomat!` instead of one per each argument
  in `std::format!`.

## Limitations

The `write!` and `writeln!` macros work on everything that
has a `.write_fmt` method. This crate requires also
the `.write_str` method. It works for any `io::Write` or
`fmt::Write`, but in unlikely circumstances if you're using something
custom, you should consult the source.

## Is it a templating language?

Kind of, but please don't use it as HTML-templating language
for security critical code, as it performs no escaping
of special characters.

