use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut inlock = stdin.lock();
    inlock.read_to_string(&mut buffer)?;
    let stdout = io::stdout();
    let mut outlock = stdout.lock();
    outlock.write(august::convert(&buffer[..], 80).as_bytes())?;
    Ok(())
}
