use august;

#[test]
fn test_whitespace_collapse() {
    let input = " If I\n\n\nwere a\trich man";
    let output = "If I were a rich man";
    assert_eq!(output, august::convert(input, 80));
}
