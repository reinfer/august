//!
//! August is a library for converting HTML to plain text.
//!
//! # Design
//!
//! The main goal of this library is to provide readable and efficent
//! results when converting HTML emails into text, and so it is designed
//! with that in mind. For example
//!
//! * There's no way to reliably convert the output of this program back
//!   into HTML. Adding the extra markup for that impeeds readability and
//!   isn’t useful in an email anyway.
//! * A fair bit of work is done to make sure that tables are rendered
//!   nicely. Emails often use tables for layout because CSS support is
//!   patchy.
//! * We try hard to get whitespace correct so you don't end up
//!   withtextlikethis  or  like  this around element boundaries.
//!
//! # Limitations
//!
//! * Currently we don't support CSS at all
//! * There are a few elements \<bdo\>, \<sup\>, and \<sub\> that we should
//!   support but don’t.
//! * We don’t support \<ruby\> and related elements. Ruby was intentionally
//!   designed to fallback, so that’s probably fine.
//!
//! # Usage
//!
//! Just call the [`convert`](fn.convert.html) or
//! [`convert_io`](fn.convert_io.html) functions.
//!

#[macro_use]
extern crate lazy_static;

use std::borrow::Cow;
use std::collections::HashSet;
use std::default::Default;
use std::io;
use std::str::FromStr;
use std::string::String;

use html5ever::{
    interface::Attribute,
    parse_document,
    rcdom::{Handle, NodeData, RcDom},
    tendril::{
        fmt::UTF8,
        {Tendril, TendrilSink},
    },
};
use itertools::Itertools;
use num_rational::Ratio;
use num_traits::Zero;
use regex::Regex;
use textwrap;
use unicode_segmentation::UnicodeSegmentation;

lazy_static! {
    static ref NEWLINE_EDGES: Regex = Regex::new("(^\\r?\\n?)|(\\r?\\n?$)").unwrap();
    static ref WHITESPACE_AFFIX: Regex = Regex::new("\\s+").unwrap();
}

const COLUMN_SEP: &str = "  ";
const LINE_SEP: &str = "\n";

/// Grapheme width of text
pub type Width = usize;

type DocState = HashSet<String>;
/// A style transformation function
///
/// We currently use these to create new `StyleData` objects
/// to pass down to child elements for cascading.
type StyleFn = fn(StyleData) -> StyleData;
/// A text element replacement function
///
/// For the most part these text replacement functions are just
/// substituting one string for another. The slight difference
/// is that they also set `EdgeState` values (as )
///
type ReplaceFn = fn(&ElementData, &mut DocState) -> VNodeType;

struct ElementData {
    attrs: Vec<Attribute>,
    children: Vec<VNodeType>,
    style: StyleData,
}

/// Struct for cascading style information as we render
///
/// There should be a separate variable here for each
/// type of style that August supports.
///
/// Note that we currently cascade all styles all the time.
/// This is fine for now because the styles we implement normally
/// cascade, but it should change in the future.
#[derive(Copy, Clone)]
struct StyleData {
    preserve_whitespace: bool,
    underline: bool,
    strike: bool,
    uppercase: bool,
}

/// Enumeration of the fundamental Block types
///
/// For the most part, you can reduce each type of HTML element
/// into a combination of `BlockType` and (optionally) some
/// style transformation. For example, a `<pre>` element would
/// be a `Block` `BlockType` and it uses the
/// `StyleData::set_preserve_whitespace` style transformation.
enum BlockType {
    /// Basic block type, most elements use this type
    Block,
    /// Similar to `Block`, but lines are prefixed with `> `
    Quote,
    /// Block type for <table> elements, chidren should be TPart or TRow
    Table,
    /// Block type for <thead/tbody/tfoot> elements, chidren should be TRow
    TPart,
    /// A table row, the children of these are displayed side-by-side
    TRow,
    /// Block type for <ol>
    UList,
    /// Block type for <ol>
    OList,
    /// Block type for <li>
    ///
    /// It contains extra data for tracking ordering and number.
    ListItem(ItemizedData),
    /// Block type for <hr>
    Rule,
}

/// An instance of a block element
struct Block {
    block_type: BlockType,
    data: ElementData,
}

/// HTML -> our DOM mappings
struct ElementMapping {
    node_type: ElementType,
    style: Option<StyleFn>,
}

/// All of our basic element types
enum ElementType {
    /// These elements are ignored when rendering
    Blank,
    /// These are replaced elements
    ///
    /// In a replaced element the entire content of the element is
    /// replaced by some other content (whatever is dictated by `ReplaceFn`)
    Replace(ReplaceFn),
    /// Inline elements typically just contain text
    ///
    /// They are displayed as-is, sometimes with some style transformations.
    Inline,
    /// A block element
    ///
    /// Roughly speaking, block elements are displayed “in a block” (i.e.
    /// on their own lines). They can also have more complicated display
    /// rules (as is dictated by the `BlockType`)
    Block(BlockType),
}

/// Function for replacing an element via some sort of format string
///
/// Note that in the long run, these nodes should use styled elements
/// and not a replaced elemenet.
macro_rules! formatted_element {
    ($fmt:expr) => {
        |element: &ElementData, _: &mut DocState| {
            let text = element.get_text();
            VNodeType::from_text(format!($fmt, text).as_str(), element.style)
        }
    };
}

/// Strike out text
///
/// This function returns a string with the unicode “Combining Long
/// Strike Overlay” added after every letter in the source string.
fn strike(text: &str) -> Cow<str> {
    let mut result = String::new();
    for graph in text.graphemes(true) {
        result.push_str(graph);
        result.push('\u{0336}');
    }
    Cow::from(result)
}

#[test]
fn test_strike() {
    let output = "s̶t̶r̶i̶k̶e̶";
    let input = "strike";
    assert_eq!(output, strike(&input));
}

/// Underline text
///
/// This function returns a string with the unicode “Combining Low
/// Line” added after every letter in the source string.
fn underline(text: &str) -> Cow<str> {
    let mut result = String::new();
    for graph in text.graphemes(true) {
        result.push_str(graph);
        result.push('\u{0332}');
    }
    Cow::from(result)
}

#[test]
fn test_underline() {
    let output = "u̲n̲d̲e̲r̲l̲i̲n̲e̲";
    let input = "underline";
    assert_eq!(output, underline(&input));
}

/// Function for <a> replaced element
fn a_element(element: &ElementData, _: &mut DocState) -> VNodeType {
    let text = element.get_text();
    let href_opt = element.get_attr("href");
    let text = match href_opt {
        None => text,
        Some(x) => {
            if x.starts_with("mailto:") {
                Cow::from(format!("{} <{}>", text, &x[7..]))
            } else if x.starts_with("http:") || x.starts_with("https:") {
                Cow::from(format!("{} ({})", text, &x))
            } else {
                text
            }
        }
    };
    VNodeType::from_text(&text, element.style)
}

/// Function for <abbr> replaced element
fn abbr_element(element: &ElementData, doc_state: &mut DocState) -> VNodeType {
    let text = element.get_text();
    let title = element.get_attr("title");
    let text = match title {
        None => text,
        Some(x) => {
            let key = format!("abbr_{}", text);
            if doc_state.contains(&key) {
                text
            } else {
                doc_state.insert(key);
                Cow::from(format!("{} ({})", text, &x))
            }
        }
    };
    VNodeType::from_text(&text, element.style)
}

/// Function for <img> replaced element
fn img_element(element: &ElementData, _: &mut DocState) -> VNodeType {
    VNodeType::from_text(
        &match element.get_attr("alt") {
            Some(x) => Cow::from(x.to_string()),
            None => Cow::from(""),
        },
        element.style,
    )
}

/// Function for <input> replaced element
fn input_element(element: &ElementData, _: &mut DocState) -> VNodeType {
    VNodeType::from_text(
        &match element.get_attr("alt") {
            Some(x) => {
                let mut result = String::with_capacity(x.len() + 2);
                result.push('[');
                result.push_str(&x[..]);
                result.push(']');
                Cow::from(result)
            }
            None => Cow::from("[]"),
        },
        element.style,
    )
}

/// Function for <br> replaced element
fn br_element(_: &ElementData, _: &mut DocState) -> VNodeType {
    VNodeType::Text("\n".to_owned(), EdgeState::Trim, EdgeState::Trim)
}

struct ItemizedData {
    ordered: bool,
    count: usize,
    count_length: usize,
}

impl ElementMapping {
    fn with_replace(func: ReplaceFn) -> ElementMapping {
        ElementMapping {
            node_type: ElementType::Replace(func),
            style: None,
        }
    }

    fn with_block(block_type: BlockType) -> ElementMapping {
        ElementMapping {
            node_type: ElementType::Block(block_type),
            style: None,
        }
    }

    fn with_style(style: StyleFn) -> ElementMapping {
        ElementMapping {
            node_type: ElementType::Inline,
            style: Some(style),
        }
    }

    fn plain() -> ElementMapping {
        ElementMapping {
            node_type: ElementType::Inline,
            style: None,
        }
    }

    fn blank() -> ElementMapping {
        ElementMapping {
            node_type: ElementType::Blank,
            style: None,
        }
    }

    fn get(tag_name: &str) -> ElementMapping {
        let header_mapping = ElementMapping {
            node_type: ElementType::Block(BlockType::Block),
            style: Some(StyleData::set_uppercase),
        };
        match tag_name {
            // root elements
            "html" => ElementMapping::with_block(BlockType::Block),
            "body" => ElementMapping::with_block(BlockType::Block),
            //sectioning
            "address" => ElementMapping::with_block(BlockType::Block),
            "article" => ElementMapping::with_block(BlockType::Block),
            "aside" => ElementMapping::with_block(BlockType::Block),
            "footer" => ElementMapping::with_block(BlockType::Block),
            "header" => ElementMapping::with_block(BlockType::Block),
            "h1" => header_mapping,
            "h2" => header_mapping,
            "h3" => header_mapping,
            "h4" => header_mapping,
            "h5" => header_mapping,
            "h6" => header_mapping,
            "hgroup" => ElementMapping::with_block(BlockType::Block),
            "main" => ElementMapping::with_block(BlockType::Block),
            "nav" => ElementMapping::with_block(BlockType::Block),
            "section" => ElementMapping::with_block(BlockType::Block),
            // text content
            "blockquote" => ElementMapping::with_block(BlockType::Quote),
            "dd" => ElementMapping::with_block(BlockType::Block),
            "div" => ElementMapping::with_block(BlockType::Block),
            "dl" => ElementMapping::with_block(BlockType::Block),
            "dt" => ElementMapping::with_block(BlockType::Block),
            "figcaption" => ElementMapping::with_block(BlockType::Block),
            "figure" => ElementMapping::with_block(BlockType::Block),
            "hr" => ElementMapping::with_block(BlockType::Rule),
            "li" => ElementMapping::with_block(BlockType::ListItem(ItemizedData::new())),
            "ol" => ElementMapping::with_block(BlockType::OList),
            "p" => ElementMapping::with_block(BlockType::Block),
            "pre" => ElementMapping {
                node_type: ElementType::Block(BlockType::Block),
                style: Some(StyleData::set_preserve_whitespace),
            },
            "ul" => ElementMapping::with_block(BlockType::UList),
            // table
            "caption" => ElementMapping::with_block(BlockType::Block),
            "col" => ElementMapping::blank(),
            "colgroup" => ElementMapping::blank(),
            "table" => ElementMapping::with_block(BlockType::Table),
            "tbody" => ElementMapping::with_block(BlockType::TPart),
            "td" => ElementMapping::with_block(BlockType::Block),
            "tfoot" => ElementMapping::with_block(BlockType::TPart),
            "th" => header_mapping,
            "thead" => ElementMapping::with_block(BlockType::TPart),
            "tr" => ElementMapping::with_block(BlockType::TRow),
            // document metadata
            "base" => ElementMapping::blank(),
            "head" => ElementMapping::blank(),
            "link" => ElementMapping::blank(),
            "meta" => ElementMapping::blank(),
            "style" => ElementMapping::blank(),
            "title" => ElementMapping::blank(),
            // media, this is good enough, img is inline
            "video" => ElementMapping::with_block(BlockType::Block),
            "area" => ElementMapping::blank(),
            "map" => ElementMapping::blank(),
            "track" => ElementMapping::blank(),
            // embedded
            "picture" => ElementMapping::with_block(BlockType::Block),
            "source" => ElementMapping::blank(),
            "iframe" => ElementMapping::blank(),
            // scripting
            "canvas" => ElementMapping::blank(),
            "noscript" => ElementMapping::with_block(BlockType::Block),
            "script" => ElementMapping::blank(),
            // forms
            "button" => ElementMapping::blank(),
            "datalist" => ElementMapping::blank(),
            "fieldset" => ElementMapping::with_block(BlockType::Block),
            "form" => ElementMapping::with_block(BlockType::Block),
            "legend" => ElementMapping::with_block(BlockType::Block),
            "meter" => ElementMapping::blank(),
            "optgroup" => ElementMapping::blank(),
            "option" => ElementMapping::blank(),
            "output" => ElementMapping::blank(),
            "progress" => ElementMapping::blank(),
            "textarea" => ElementMapping::with_block(BlockType::Block),
            // interactive
            "details" => ElementMapping::with_block(BlockType::Block),
            "dialog" => ElementMapping::with_block(BlockType::Block),
            "menu" => ElementMapping::blank(),
            "menuitem" => ElementMapping::blank(),
            "summary" => ElementMapping::with_block(BlockType::Block),
            // web components
            "slot" => ElementMapping::with_block(BlockType::Block),
            "template" => ElementMapping::with_block(BlockType::Block),
            // textual
            "a" => ElementMapping::with_replace(a_element),
            "abbr" => ElementMapping::with_replace(abbr_element),
            "b" => ElementMapping::with_replace(formatted_element!("*{}*")),
            "bdi" => ElementMapping::plain(), // no idea how to support this
            "bdo" => ElementMapping::plain(), // TODO: we can support this
            "br" => ElementMapping::with_replace(br_element),
            "cite" => ElementMapping::plain(),
            "code" => ElementMapping::with_replace(formatted_element!("`{}`")),
            "data" => ElementMapping::plain(),
            "dfn" => ElementMapping::with_replace(formatted_element!("/{}/")),
            "em" => ElementMapping::with_replace(formatted_element!("/{}/")),
            "i" => ElementMapping::with_replace(formatted_element!("/{}/")),
            "kbd" => ElementMapping::with_style(StyleData::set_uppercase),
            "mark" => ElementMapping::with_replace(formatted_element!(">{}<")),
            "q" => ElementMapping::with_replace(formatted_element!("“{}”")),
            "rb" => ElementMapping::plain(),
            "rp" => ElementMapping::plain(),
            "rt" => ElementMapping::plain(),
            "rtc" => ElementMapping::plain(),
            "ruby" => ElementMapping::plain(),
            "s" => ElementMapping::with_style(StyleData::set_strike),
            "samp" => ElementMapping::with_replace(formatted_element!("“{}”")),
            "small" => ElementMapping::plain(), // no good way to support this
            "span" => ElementMapping::plain(),
            "strong" => ElementMapping::with_replace(formatted_element!("*{}*")),
            "sup" => ElementMapping::plain(), // TODO: we can support this
            "sub" => ElementMapping::plain(), // TODO: we can support this
            "time" => ElementMapping::plain(),
            "tt" => ElementMapping::plain(),
            "var" => ElementMapping::with_replace(formatted_element!("`{}`")),
            // intentionally ignoring "wbr"
            // multimedia/images
            "img" => ElementMapping::with_replace(img_element),
            // edits
            "del" => ElementMapping::with_style(StyleData::set_strike),
            "ins" => ElementMapping::plain(),
            // forms
            "input" => ElementMapping::with_replace(input_element),
            "label" => ElementMapping::plain(),
            "select" => ElementMapping::with_replace(input_element),
            "u" => ElementMapping::with_style(StyleData::set_underline),
            // By default we just treat things as plain "inline"
            _ => ElementMapping::plain(),
        }
    }
}

impl ItemizedData {
    /// Initialize with dummy values
    fn new() -> ItemizedData {
        ItemizedData {
            ordered: false,
            count: 0,
            count_length: 0,
        }
    }

    /// Set ordering values
    ///
    /// These are set when the parent list element is being created
    fn set_ordering(&mut self, count: usize, count_length: usize) {
        self.ordered = true;
        self.count = count;
        self.count_length = count_length;
    }
}

impl Block {
    /// Initialize a block element from a BlockType and ElementData
    fn from_data(block_type: BlockType, mut data: ElementData) -> Block {
        if let BlockType::OList = block_type {
            let mut total = 0;

            for child in data.children.iter() {
                if let VNodeType::Block(el) = child {
                    if let BlockType::ListItem(_) = el.block_type {
                        total += 1;
                    }
                }
            }
            let count_length = format!("{}", total).len();
            let mut count = 0;
            for child in data.children.iter_mut() {
                if let VNodeType::Block(el) = child {
                    if let BlockType::ListItem(ref mut item_data) = &mut el.block_type {
                        count += 1;
                        item_data.set_ordering(count, count_length);
                    }
                }
            }
        }
        Block { block_type, data }
    }

    fn block_write(&self, output: &mut StringWriter, width: Option<Width>) -> io::Result<()> {
        if let BlockType::Rule = self.block_type {
            output.write_str(&"-".repeat(width.unwrap_or(3)))
        } else {
            let data = match width {
                Some(w) => Some(self.get_block_data(w)),
                None => None,
            };
            generic_block_write(&self.data.children, &data, output)
        }
    }

    fn get_block_data(&self, width: Width) -> BlockData {
        BlockData {
            child_block_sep: self.get_child_block_sep(),
            first_line_prefix: self.get_first_line_prefix(),
            next_line_prefix: self.get_next_line_prefix(),
            width,
        }
    }

    fn get_child_block_sep(&self) -> &str {
        match self.block_type {
            BlockType::OList => "\n",
            BlockType::UList => "\n",
            BlockType::Quote => "\n>\n",
            _ => "\n\n",
        }
    }

    fn get_first_line_prefix(&self) -> String {
        match &self.block_type {
            BlockType::ListItem(id) => {
                if id.ordered {
                    format!("{:>width$}. ", count = id.count, width = id.count_length)
                } else {
                    "* ".to_owned()
                }
            }
            BlockType::Quote => "> ".to_owned(),
            _ => "".to_owned(),
        }
    }

    fn get_next_line_prefix(&self) -> String {
        match &self.block_type {
            BlockType::ListItem(id) => {
                if id.ordered {
                    " ".repeat(id.count_length + 2)
                } else {
                    "  ".to_owned()
                }
            }
            BlockType::Quote => "> ".to_owned(),
            _ => "".to_owned(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum EdgeState {
    Trim,
    White,
    Blank,
}

impl EdgeState {
    fn from(text: &str, style: StyleData) -> (EdgeState, EdgeState, &str) {
        if style.preserve_whitespace {
            (EdgeState::Blank, EdgeState::Blank, text)
        } else {
            let ts = text.trim_start();
            let te = ts.trim_end();
            (
                EdgeState::from_white(ts.len() < text.len()),
                EdgeState::from_white(te.len() < ts.len()),
                te,
            )
        }
    }

    fn from_white(is_white: bool) -> EdgeState {
        if is_white {
            EdgeState::White
        } else {
            EdgeState::Blank
        }
    }

    fn join_str(&self, other: &EdgeState) -> &str {
        match self {
            EdgeState::Trim => "",
            EdgeState::Blank => match other {
                EdgeState::White => " ",
                _ => "",
            },
            EdgeState::White => match other {
                EdgeState::Trim => "",
                _ => " ",
            },
        }
    }
}

enum VNodeType {
    /// A text element
    Text(String, EdgeState, EdgeState),
    Block(Block),
}

impl VNodeType {
    fn is_empty(&self) -> bool {
        match self {
            VNodeType::Text(text, _, _) => text == "",
            VNodeType::Block(_) => false,
        }
    }

    fn width_hint(&self) -> Width {
        match self {
            VNodeType::Text(text, _, _) => text.as_str().graphemes(true).count(),
            VNodeType::Block(block) => {
                if let BlockType::Rule = block.block_type {
                    3
                } else {
                    let mut width = 0;
                    for child in &block.data.children {
                        let child_size = child.width_hint();
                        if child_size > width {
                            width = child_size
                        }
                    }
                    width
                }
            }
        }
    }

    fn from_text(text: &str, style: StyleData) -> VNodeType {
        let (prefix, suffix, text) = EdgeState::from(&text, style);
        let text = if style.preserve_whitespace {
            NEWLINE_EDGES.replace_all(text, "")
        } else {
            WHITESPACE_AFFIX.replace_all(text, " ")
        };
        let text = if style.underline {
            underline(&text)
        } else {
            text
        };
        let text = if style.strike { strike(&text) } else { text };
        let text = if style.uppercase {
            Cow::from(text.to_uppercase())
        } else {
            text
        };
        // It would be nice to store a Cow here, but that
        // would require getting lifetimes correct.
        VNodeType::Text(text.into(), prefix, suffix)
    }
}

fn get_virtual_elements(
    node: &Handle,
    style: Option<StyleData>,
    doc_state: &mut DocState,
) -> Vec<VNodeType> {
    let styledata = match style {
        Some(s) => s,
        None => StyleData::new()
    };
    match node.data {
        NodeData::Text { ref contents } => {
            let b = contents.borrow();
            if b.is_empty() {
                vec![]
            } else {
                vec![VNodeType::from_text(&b, styledata)]
            }
        }
        NodeData::Element {
            ref name,
            ref attrs,
            ..
        } => {
            // build vecs
            let tag_name = &name.local[..];
            let mapping = ElementMapping::get(tag_name);
            let style = match (style, mapping.style) {
                (Some(st), Some(ma)) => {
                    Some(ma(st))
                }
                _ => style
            };

            let mut child_vec = Vec::new();
            for child in node.children.borrow().iter() {
                for el in get_virtual_elements(&child.clone(), style, doc_state) {
                    child_vec.push(el);
                }
            }

            let mut attrs_vec = Vec::new();
            for attr in attrs.borrow().iter() {
                attrs_vec.push(attr.clone())
            }

            match mapping.node_type {
                ElementType::Blank => vec![],
                ElementType::Inline => child_vec,
                ElementType::Replace(func) => {
                    let data = ElementData::from(attrs_vec, child_vec, styledata);
                    vec![func(&data, doc_state)]
                }
                ElementType::Block(block_type) => {
                    let data = ElementData::from(attrs_vec, child_vec, styledata);
                    vec![VNodeType::Block(Block::from_data(block_type, data))]
                }
            }
        }
        NodeData::Document => {
            let mut child_vec = Vec::new();
            for child in node.children.borrow().iter() {
                for el in get_virtual_elements(&child.clone(), style, doc_state) {
                    child_vec.push(el);
                }
            }
            let data = ElementData::from(vec![], child_vec, styledata);
            vec![VNodeType::Block(Block::from_data(BlockType::Block, data))]
        }
        _ => vec![],
    }
}

impl ElementData {
    fn from(attrs: Vec<Attribute>, children: Vec<VNodeType>, style: StyleData) -> ElementData {
        ElementData {
            attrs,
            children,
            style,
        }
    }

    /// Gets the contents as text
    ///
    /// Note that if there are child block elements, they will get
    /// displayed as if they were inline
    fn get_text<'a>(&self) -> Cow<'a, str> {
        let mut s = String::new();
        let mut sw = StringWriter::Str(&mut s);
        generic_block_write(&self.children, &None, &mut sw).unwrap();
        Cow::from(s)
    }

    /// Get attribute value
    fn get_attr(&self, key: &str) -> Option<Tendril<UTF8>> {
        let mut result: Option<Tendril<UTF8>> = None;
        for attr in self.attrs.iter() {
            if &attr.name.local[..] == key {
                result = Some(attr.value.clone());
                break;
            }
        }
        result
    }
}

impl StyleData {
    fn new() -> StyleData {
        StyleData {
            preserve_whitespace: false,
            underline: false,
            strike: false,
            uppercase: false,
        }
    }

    /// Return a `StyleData` with strike style turned on
    fn set_strike(self) -> Self {
        Self {
            strike: true,
            ..self
        }
    }

    /// Return a `StyleData` with underline style turned on
    fn set_underline(self) -> Self {
        Self {
            underline: true,
            ..self
        }
    }

    /// Return a `StyleData` with preserve whitespace style turned on
    fn set_preserve_whitespace(self) -> StyleData {
        Self {
            preserve_whitespace: true,
            ..self
        }
    }

    /// Return a `StyleData` with uppercase style turned on
    fn set_uppercase(self) -> StyleData {
        Self {
            uppercase: true,
            ..self
        }
    }
}

/// Contains a variety of options for rendering a block element
///
/// Most of these are set based on the parent BlockType
struct BlockData<'a> {
    child_block_sep: &'a str,
    first_line_prefix: String,
    next_line_prefix: String,
    width: Width,
}

impl<'a> BlockData<'a> {
    fn get_sep(&self) -> String {
        format!("{}{}", LINE_SEP, self.next_line_prefix)
    }
}

fn inline_block_write(text: &str, data: &BlockData, output: &mut StringWriter) -> io::Result<()> {
    let wrapper = textwrap::Wrapper::new(data.width);
    let mut wrapped_lines = wrapper.wrap(&text).into_iter();
    if let Some(line) = wrapped_lines.next() {
        output.write_str(&line)?;
    }
    for line in wrapped_lines {
        output.write_str(&data.get_sep())?;
        output.write_str(&line)?;
    }
    Ok(())
}

/// Big large rendering function
///
/// If `block_data` is set, it will be rendered as a block. Otherwise
/// it will be rendered inline.
fn generic_block_write(
    children: &[VNodeType],
    block_data: &Option<BlockData>,
    output: &mut StringWriter,
) -> io::Result<()> {
    let mut first_block = true;
    let mut last_inline_text = String::new();
    let mut had_white_suffix = EdgeState::Trim;
    for child in children.iter() {
        match child {
            VNodeType::Text(text, prefix, suffix) => {
                if text == "" {
                    if let EdgeState::Blank = had_white_suffix {
                        had_white_suffix = EdgeState::White;
                    }
                } else {
                    last_inline_text.push_str(had_white_suffix.join_str(prefix));
                    last_inline_text.push_str(text);
                    had_white_suffix = *suffix;
                }
            }
            VNodeType::Block(el) => {
                if let Some(data) = &block_data {
                    if last_inline_text != "" {
                        if first_block {
                            first_block = false;
                        } else {
                            output.write_str(data.child_block_sep)?;
                        }
                        inline_block_write(&last_inline_text, data, output)?;
                    }
                    let width = Some(data.width - data.first_line_prefix.len());
                    let mut text = String::new();
                    if let BlockType::Table = el.block_type {
                        let column_widths = table_column_widths(&el.data.children);
                        let column_widths = recalculate_column_widths(&column_widths, data.width);
                        let rows = table_rows(el, width, &column_widths);
                        text.push_str(&rows.iter().filter(|x| !x.is_empty()).join("\n"))
                    } else {
                        let mut sw = StringWriter::Str(&mut text);
                        el.block_write(&mut sw, width)?
                    };
                    if !text.is_empty() {
                        let mut block_lines = text.split(LINE_SEP);
                        if first_block {
                            first_block = false;
                        } else {
                            output.write_str(data.child_block_sep)?;
                        }
                        output.write_str(&data.first_line_prefix)?;
                        output.write_str(&block_lines.join(&data.get_sep()))?;
                    }
                    last_inline_text.clear();
                } else {
                    last_inline_text.push_str(had_white_suffix.join_str(&EdgeState::Blank));
                    let mut sw = StringWriter::Str(&mut last_inline_text);
                    el.block_write(&mut sw, None)?
                }
                had_white_suffix = EdgeState::Trim;
            }
        };
    }
    if let Some(data) = &block_data {
        if !last_inline_text.is_empty() {
            let wrapper = textwrap::Wrapper::new(data.width);
            let wrapped_lines = wrapper.wrap(&last_inline_text).into_iter();
            for line in wrapped_lines {
                if first_block {
                    first_block = false;
                    output.write_str(&data.first_line_prefix)?;
                } else {
                    output.write_str(LINE_SEP)?;
                    output.write_str(&data.next_line_prefix)?;
                }
                output.write_str(&line)?;
            }
        }
        Ok(())
    } else {
        output.write_str(&last_inline_text)
    }
}

/// Calculate the column withds of a whole table
fn table_column_widths(rows: &[VNodeType]) -> Vec<Ratio<Width>> {
    let mut result = Vec::new();
    for child in rows.iter() {
        let child_results = tr_column_widths(&child);
        for child_result in child_results.iter() {
            let min_len = if result.len() < child_result.len() {
                result.len()
            } else {
                child_result.len()
            };
            for idx in 0..min_len {
                if result[idx] < child_result[idx] {
                    result[idx] = child_result[idx];
                }
            }
            if min_len < child_result.len() {
                result.extend_from_slice(&child_result[min_len..]);
            }
        }
    }
    result
}

/// Calculate the column withds of an individual table row
fn tr_column_widths(row: &VNodeType) -> Vec<Vec<Ratio<Width>>> {
    match row {
        VNodeType::Text(_, _, _) => vec![vec![Ratio::new(row.width_hint(), 1)]],
        VNodeType::Block(block) => match block.block_type {
            BlockType::TPart => {
                let mut result = Vec::new();
                for child in block.data.children.iter() {
                    result.extend(tr_column_widths(&child));
                }
                result
            }
            BlockType::TRow => {
                let mut result = Vec::new();
                for child in block.data.children.iter() {
                    if child.is_empty() {
                        continue;
                    }
                    let span: Width = match child {
                        VNodeType::Block(block) => match block.data.get_attr("colspan") {
                            Some(s) => s.parse::<Width>().unwrap_or(1),
                            None => 1,
                        },
                        _ => 1,
                    };
                    for _ in 0..span {
                        result.push(Ratio::new(child.width_hint(), span));
                    }
                }
                vec![result]
            }
            _ => vec![vec![Ratio::new(row.width_hint(), 1)]],
        },
    }
}

/// Given a Table or TPart and widths, return text for that Block
fn table_rows(table: &Block, max_width: Option<Width>, column_widths: &[Width]) -> Vec<String> {
    let mut rows = Vec::new();
    for maybe_row in table.data.children.iter() {
        if maybe_row.is_empty() {
            continue;
        }
        match maybe_row {
            VNodeType::Text(text, _, _) => rows.push(text.to_owned()),
            VNodeType::Block(block) => match block.block_type {
                BlockType::TPart => {
                    let inner_rows = table_rows(block, max_width, column_widths);
                    rows.extend(inner_rows);
                }
                BlockType::TRow => rows.push(tr_text(&block, max_width, &column_widths)),
                _ => {
                    let mut s = String::new();
                    let mut sw = StringWriter::Str(&mut s);
                    block.block_write(&mut sw, max_width).unwrap();
                    rows.push(s);
                }
            },
        }
    }
    rows
}

/// Given a row and widths, return text for that row
fn tr_text(row: &Block, max_width: Option<Width>, column_widths: &[Width]) -> String {
    if max_width.is_none() {
        let mut s = String::new();
        let mut sw = StringWriter::Str(&mut s);
        row.block_write(&mut sw, None).unwrap();
        s
    } else {
        let mut cells = Vec::new();
        let mut idx = 0;
        let mut max_height = 0;
        for child in row.data.children.iter() {
            if child.is_empty() {
                continue;
            }
            let span = if let VNodeType::Block(b) = child {
                match b.data.get_attr("colspan") {
                    Some(s) => s.parse::<Width>().unwrap_or(1),
                    None => 1,
                }
            } else {
                1
            };

            let width = column_widths[idx..idx + span].iter().sum();
            let text = match child {
                VNodeType::Text(ref text, _, _) => Cow::from(text),
                VNodeType::Block(b) => {
                    let mut s = String::new();
                    let mut sw = StringWriter::Str(&mut s);
                    b.block_write(&mut sw, Some(width)).unwrap();
                    Cow::from(s)
                }
            };

            let height = text.lines().count();
            if max_height < height {
                max_height = height
            }
            cells.push((width, text));
            idx += span;
            if idx > column_widths.len() {
                break;
            }
        }

        let mut lines = vec![String::new(); max_height];
        let mut first = true;

        for (width, content) in cells {
            let mut height = 0;
            for (idx, c_line) in content.lines().enumerate() {
                height = idx + 1;
                if !first {
                    lines[idx].push_str(COLUMN_SEP);
                }
                lines[idx].push_str(c_line);
                let c_line_len = c_line.graphemes(true).count();
                if width > c_line_len {
                    lines[idx].push_str(" ".repeat(width - c_line_len).as_str());
                }
            }
            // here we're just evening out all the cell heights
            for item in lines.iter_mut().take(max_height).skip(height) {
                if !first {
                    item.push_str(COLUMN_SEP);
                }
                item.push_str(" ".repeat(width).as_str())
            }
            first = false;
        }
        lines.iter().map(|l| l.trim_end()).join("\n")
    }
}

fn recalculate_column_widths(widths: &[Ratio<Width>], max_width: Width) -> Vec<Width> {
    let mut result: Vec<Width> = vec![0; widths.len()];
    if !widths.is_empty() {
        let usable_max = Ratio::new(max_width - COLUMN_SEP.len() * (widths.len() - 1), 1);
        let desired_max: Ratio<Width> = widths.iter().sum();

        if !desired_max.is_zero() {
            let ratio = if usable_max < desired_max {
                usable_max / desired_max
            } else {
                Ratio::new(1, 1)
            };
            let mut float_widths = widths
                .iter()
                .map(|x| ratio * x)
                .enumerate()
                .sorted_by(|a, b| Ord::cmp(&b.1, &a.1))
                .collect::<Vec<(usize, Ratio<usize>)>>();
            let mut balance: Ratio<usize> = Ratio::from_integer(0);
            let mut positive = true;
            let mut start_idx = 0;
            let mut end_idx = widths.len() - 1;
            for _ in 0..widths.len() {
                let idx = if positive { start_idx } else { end_idx };
                if positive {
                    start_idx += 1
                } else {
                    end_idx -= 1
                }
                let (width_idx, mut width_value) = float_widths[idx];
                let mut new_width_ratio = if positive {
                    width_value.floor()
                } else {
                    width_value.ceil()
                };

                float_widths[idx] = (width_idx, new_width_ratio);
                if positive {
                    new_width_ratio += balance
                } else {
                    width_value += balance
                }
                if new_width_ratio >= width_value {
                    balance = new_width_ratio - width_value;
                    positive = true;
                } else {
                    balance = width_value - new_width_ratio;
                    positive = false;
                }
            }
            // now mutate our original widths
            float_widths.sort_by_key(|x| x.0);
            for (idx, (_, ratio)) in float_widths.iter().enumerate() {
                result[idx] = ratio.to_integer() as Width;
            }
        }
    }
    result
}

#[test]
fn test_short_recalculate() {
    let start_widths: Vec<Ratio<Width>> =
        vec![Ratio::new(2, 1), Ratio::new(4, 1), Ratio::new(6, 1)];
    let end_widths: Vec<Width> = vec![2, 4, 6];
    assert_eq!(end_widths, recalculate_column_widths(&start_widths, 80));
}

#[test]
fn test_long_recalculate() {
    let start_widths: Vec<Ratio<Width>> =
        vec![Ratio::new(12, 1), Ratio::new(66, 1), Ratio::new(65, 1)];
    let expected_widths: Vec<Width> = vec![7, 35, 34];
    assert_eq!(
        expected_widths,
        recalculate_column_widths(&start_widths, 80)
    );
}

#[test]
fn test_long_recalculate2() {
    let start_widths: Vec<Ratio<Width>> =
        vec![Ratio::new(10, 1), Ratio::new(66, 1), Ratio::new(65, 1)];
    let expected_widths: Vec<Width> = vec![6, 35, 35];
    assert_eq!(
        expected_widths,
        recalculate_column_widths(&start_widths, 80)
    );
}

/// Provides a consistent interface between String and std::io::Write,
enum StringWriter<'a> {
    Str(&'a mut String),
    Io(&'a mut dyn std::io::Write),
}

impl<'a> StringWriter<'a> {
    fn write_str(&mut self, s: &str) -> Result<(), std::io::Error> {
        match self {
            StringWriter::Str(x) => {
                x.push_str(s);
                Ok(())
            }
            StringWriter::Io(x) => match x.write(s.as_bytes()) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
        }
    }
}

/// Converts HTML text into plain text
///
/// # Example
///
/// ```
/// let text = "Hello\nWorld";
/// let html = "Hello<br>World";
/// assert_eq!(text, august::convert(html, 79));
/// ```

pub fn convert(input: &str, width: Width) -> String {
    let parser = parse_document(RcDom::default(), Default::default());
    let dril = Tendril::from_str(input).unwrap();
    let dom = parser.one(dril);
    convert_dom(&dom, width)
}

/// Converts HTML text into unstyled plain text
///
/// # Example
///
/// ```
/// let text = "Hello\nWorld";
/// let html = "<s><u>Hello<br>World</u></s>";
/// assert_eq!(text, august::convert_unstyled(html, 79));
/// ```

pub fn convert_unstyled(input: &str, width: Width) -> String {
    let parser = parse_document(RcDom::default(), Default::default());
    let dril = Tendril::from_str(input).unwrap();
    let dom = parser.one(dril);
    convert_dom_unstyled(&dom, width)
}

/// Converts HTML text into plain text, using an I/O reader & writer
///
/// This method is a fair bit faster and more memory efficient if
/// you don’t already have strings.
///
/// # Example
///
/// ```
/// use std::io;
/// august::convert_io(io::stdin().lock(), io::stdout().lock(), 79);
/// ```

pub fn convert_io(
    mut input: impl std::io::Read,
    output: impl std::io::Write,
    width: Width,
) -> io::Result<()> {
    let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut input)?;
    convert_dom_io(&dom, width, output)
}

/// Take a loaded markup5ever DOM, and send the converted text to an I/O writer
pub fn convert_dom_io(
    dom: &RcDom,
    width: Width,
    mut output: impl std::io::Write,
) -> io::Result<()> {
    let mut sw = StringWriter::Io(&mut output);
    convert_dom_string_writer(dom, true, width, &mut sw)
}

/// Take a loaded markup5ever DOM, and send the converted unstyled text to an I/O writer
pub fn convert_dom_io_unstyled(
    dom: &RcDom,
    width: Width,
    mut output: impl std::io::Write,
) -> io::Result<()> {
    let mut sw = StringWriter::Io(&mut output);
    convert_dom_string_writer(dom, false, width, &mut sw)
}

/// Converts a loaded markup5ever DOM into a text string
pub fn convert_dom(dom: &RcDom, width: Width) -> String {
    let mut result = String::new();
    let mut sw = StringWriter::Str(&mut result);
    // note, we ignore the result here, because string writing
    // can't cause an I/O Error
    convert_dom_string_writer(dom, true, width, &mut sw).unwrap();
    result
}

/// Converts a loaded markup5ever DOM into an unstyled text string
pub fn convert_dom_unstyled(dom: &RcDom, width: Width) -> String {
    let mut result = String::new();
    let mut sw = StringWriter::Str(&mut result);
    // as above, string writing can't cause an I/O Error
    convert_dom_string_writer(dom, false, width, &mut sw).unwrap();
    result
}

/// Internal method that actually does the writing
fn convert_dom_string_writer<'a>(
    dom: &RcDom,
    styling: bool,
    width: Width,
    output: &'a mut StringWriter,
) -> io::Result<()> {
    let mut doc_state = HashSet::new();
    let styling = match styling {
        true => Some(StyleData::new()),
        _ => None,
    };
    let virt_dom = get_virtual_elements(&dom.document, styling, &mut doc_state);
    for el_type in virt_dom {
        match el_type {
            // TODO: this should word-wrap
            VNodeType::Text(in_text, _, _) => output.write_str(&in_text)?,
            VNodeType::Block(e) => e.block_write(output, Some(width))?,
        }
    }
    Ok(())
}
