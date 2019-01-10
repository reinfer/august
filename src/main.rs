// use the system allocator so valgrind works
use std::alloc::System;
#[global_allocator]
static A: System = System;

use argparse;
use std::io;


const DEFAULT_WIDTH: august::Width = 79;

#[cfg(feature="term-size")]
fn default_width() -> august::Width {
    if let Some((w, _)) = term_size::dimensions() {
        w
    } else {
        DEFAULT_WIDTH
    }
}

#[cfg(not(feature="term-size"))]
fn default_width() -> august::Width {
    DEFAULT_WIDTH
}

fn main() -> io::Result<()> {
    let mut width: Option<august::Width> = None;
    {
        let width_text: &'static str = if cfg!(feature = "term-size") {
                "Set document width, defaults to terminal width"
            } else {
                "Set document width, defaults to 79"
            };

        let mut ap = argparse::ArgumentParser::new();
        ap.set_description("Convert an HTML document into plain text.");
        ap.refer(&mut width)
            .add_option(&["-w", "--width"], argparse::StoreOption, width_text);
        ap.parse_args_or_exit();
    }
    let width = width.unwrap_or_else(default_width);
    august::convert_io(io::stdin().lock(), io::stdout().lock(), width)?;
    Ok(())
}
