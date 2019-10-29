# August

August is a Rust crate & program for converting HTML to plain text.
It is specifically intended for rendering HTML emails as text.

## Usage

Add this to your `Cargo.toml`:
```toml
[dependencies]
august = "^2"
```

and this to your code:
```rust
use august;

let input = "<p>Hello</p><i>Here's some HTML!</i>";
println!("{}", august::convert(input, 79));
```

The output now looks like this:
```
Hello

/Here's some HTML!/
```

## Command line program

Cargo comes with a little command-line program `august` that reads
HTML from stdin and prints text to stdout. If you've enabled the
`term-size` feature, it uses the terminal width as the default width,
otherwise it uses 79. You can override this by passing `-w WIDTH`
as an argument.

## Known issues

1. There's no CSS support currently. Some support will probably happen
   sometime, but it's still unclear what is worth implementing.

## Changes

### 2.3.0

* Switch to more stream based functions
* Update cargo config to use semver versions to prevent broken 0.x
  dependencies.

### 2.2.0

* Add more documentation.
* Use terminal widdth as default width when run from terminal size.
* Disable term-size by default to reduce static linking size.
* Reduce memory usage by about 30% for large files.
* Reduce use of regexes.

### 2.1.0

* Add support for more inline elements: code, dfn, kbd, mark, q, samp,
  var, del, input, select.
* Add support for the pre element
* Show unsupported inline elements inline instead of block.

### 2.0

Intital Python rewrite (https://alantrick.ca/writings/programming/python_to_rust).
