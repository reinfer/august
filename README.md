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

## Known issues

1. There's a few tags that are still not yet supported (which could
   benefit from some support) like <pre>, <var>, <tt>, and probably
   a bunch that I forgot. These are not commonly seen in emails so they
   are not high priority
2. There's no CSS support currently. Some support will probably happen
   sometime, but it's still unclear what is worth implementing.