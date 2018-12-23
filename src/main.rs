use argparse;
use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    let mut width: august::Width = 79;
    {
        let mut ap = argparse::ArgumentParser::new();
        ap.set_description("Convert an HTML document into plain text.");
        ap.refer(&mut width)
            .add_option(&["-w", "--width"], argparse::Store,
            "Set document width, defaults to 79");
        ap.parse_args_or_exit();
    }
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut inlock = stdin.lock();
    inlock.read_to_string(&mut buffer)?;
    let stdout = io::stdout();
    let mut outlock = stdout.lock();
    outlock.write(august::convert(buffer.as_str(), width).as_bytes())?;
    Ok(())
}
