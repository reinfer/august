use august;

#[test]
fn test_single_col_w_colspan() {
    let input = "<table>
	<tr><th colspan='2'>A</th></tr>
	<tr><td colspan='2'>B</td></tr>
</table>";
    let output = "A\nB";
    assert_eq!(output, august::convert(input, 80));
}
