# August

August is a Rust crate & program for converting HTML to plain text.
It is specifically intended for rendering HTML emails as text.

## Usage

Add this to your `Cargo.toml`:
```toml
[dependencies]
august = "*"
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
