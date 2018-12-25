use august;
use std::fs;
use std::path::Path;
use std::io::Read;

fn test_sample(html_path: &Path, txt_path: &Path) {
    let mut html_file = fs::File::open(html_path).expect(
        "Unable to open HTML sample");
    let mut html_contents = String::new();
    html_file.read_to_string(&mut html_contents).expect(
        "Unable to read HTML sample");

    let mut txt_file = fs::File::open(txt_path).expect(
        "Unable to open text sample");
    let mut txt_contents = String::new();
    txt_file.read_to_string(&mut txt_contents).expect(
        "Unable to read text sample");
    assert_eq!(august::convert(html_contents.as_str(), 79), txt_contents)
}

#[test]
fn test_blockquote() {
    let html_path = Path::new("tests/samples/blockquote.html");
    let txt_path = Path::new("tests/samples/blockquote.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_break() {
    let html_path = Path::new("tests/samples/break.html");
    let txt_path = Path::new("tests/samples/break.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_garden() {
    let html_path = Path::new("tests/samples/garden.html");
    let txt_path = Path::new("tests/samples/garden.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_inline() {
    let html_path = Path::new("tests/samples/inline.html");
    let txt_path = Path::new("tests/samples/inline.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_invoice() {
    let html_path = Path::new("tests/samples/invoice.html");
    let txt_path = Path::new("tests/samples/invoice.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_pre() {
    let html_path = Path::new("tests/samples/pre.html");
    let txt_path = Path::new("tests/samples/pre.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_table() {
    let html_path = Path::new("tests/samples/table.html");
    let txt_path = Path::new("tests/samples/table.txt");
    test_sample(&html_path, &txt_path);
}

#[test]
fn test_weird() {
    let html_path = Path::new("tests/samples/weird.html");
    let txt_path = Path::new("tests/samples/weird.txt");
    test_sample(&html_path, &txt_path);
}
