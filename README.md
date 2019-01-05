# August

August is a Rust crate & program for converting HTML to plain text.
It is specifically intended for rendering HTML emails as text.

## Usage

Add this to your `Cargo.toml`:
```toml
[dependencies]
august = "^1"
```

and this to your code:
```rust
use august;

let input = "<p>Hello</p><i>Here's some HTML!</i>"
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