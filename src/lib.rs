#[macro_use] extern crate lazy_static;

use std::collections::HashSet;
use std::default::Default;
use std::string::String;
use std::str::FromStr;
use std::borrow::Cow;

use regex::Regex;
use textwrap;
use num_rational::Ratio;
use num_traits::Zero;
use itertools::Itertools;
use html5ever::{
    parse_document,
    rcdom::{NodeData, RcDom, Handle},
    interface::Attribute,
    tendril::{{Tendril, TendrilSink},fmt::UTF8},
};
use unicode_segmentation::UnicodeSegmentation;


lazy_static! {
    static ref NEWLINE_EDGES: Regex = Regex::new("(^\\r?\\n?)|(\\r?\\n?$)").unwrap();
    static ref WHITESPACE_AFFIX: Regex = Regex::new("\\s+").unwrap();
    static ref WHITESPACE_PREFIX: Regex = Regex::new("^\\s+").unwrap();
    static ref WHITESPACE_SUFFIX: Regex = Regex::new("\\s+$").unwrap();
}

const COLUMN_SEP: &str = "  ";
const LINE_SEP: &str = "\n";

// Data structures for our virtual DOM
pub type Width = usize;

type DocState = HashSet<String>;
type StyleFn = fn(StyleData) -> StyleData;
type ReplaceFn = fn(&ElementData, &mut DocState) -> VElementType;

struct ElementData {
    attrs: Vec<Attribute>,
    children: Vec<VElementType>,
    style: StyleData,
}

#[derive(Copy, Clone)]
struct StyleData {
    preserve_whitespace: bool,
    underline: bool,
    strike: bool,
    uppercase: bool,
}

enum BlockType {
    Block,
    Quote,
    Table,
    TPart,
    TRow,
    UList,
    OList,
    ListItem(ItemizedData),
    Rule,
}

struct Block {
    block_type: BlockType,
    data: ElementData,
}

// HTML -> our DOM mappings
struct NodeMapping {
    node_type: NodeType,
    style: Option<StyleFn>,
}

enum NodeType {
    Blank,
    Replace(ReplaceFn),
    Inline,
    Block(BlockType),
}


macro_rules! formatted_element {
    ($fmt:expr) => {
        |element: &ElementData, _: &mut DocState| {
            let text = element.get_text();
            VElementType::from_text(format!($fmt, text).as_str(), element.style)
        }
    };
}

macro_rules! fn_element {
    ($fname:ident) => {
        |element: &ElementData, _: &mut DocState| {
            let text = element.get_text();
            VElementType::from_text(&$fname(text.as_str()), element.style)
        }
    };
}

fn strike (text: &str) -> Cow<str> {
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


fn underline (text: &str) -> Cow<str> {
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

fn uppercase (text: &str) -> Cow<str> {
    Cow::from(text.to_uppercase())
}

fn a_element(element: &ElementData, _: &mut DocState) -> VElementType {
    let text = element.get_text();
    let href_opt = element.get_attr("href");
    let text = match href_opt {
        None => text,
        Some(x) => {
            if x.starts_with("mailto:") {
                format!("{} <{}>", text, &x[7..])
            } else if x.starts_with("http:") || x.starts_with("https:") {
                format!("{} ({})", text, &x)
            } else {
                text
            }
        }
    };
    VElementType::from_text(&text, element.style)
}

fn abbr_element(element: &ElementData, doc_state: &mut DocState) -> VElementType {
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
                format!("{} ({})", text, &x)
            }
        }
    };
    VElementType::from_text(&text, element.style)
}

fn img_element(element: &ElementData, _: &mut DocState) -> VElementType {
    VElementType::from_text(
        &match element.get_attr("alt") {
            Some(x) => Cow::from(x.to_string()),
            None => Cow::from(""),
        }
    , element.style)
}

fn input_element(element: &ElementData, _: &mut DocState) -> VElementType {
    VElementType::from_text(
        &match element.get_attr("alt") {
            Some(x) => {
                let mut result = String::with_capacity(x.len() + 2);
                result.push('[');
                result.push_str(&x[..]);
                result.push(']');
                Cow::from(result)
            },
            None => Cow::from("[]"),
        }
    , element.style)
}

fn br_element(_: &ElementData, _: &mut DocState) -> VElementType {
    VElementType::Text("\n".to_owned(), EdgeState::Trim, EdgeState::Trim)
}


struct ItemizedData {
    ordered: bool,
    count: usize,
    count_length: usize,
}

impl NodeMapping {
    fn with_replace(func: ReplaceFn) -> NodeMapping {
        NodeMapping { node_type: NodeType::Replace(func), style: None }
    }

    fn with_block(block_type: BlockType) -> NodeMapping {
        NodeMapping { node_type: NodeType::Block(block_type), style: None }
    }

    fn with_style(style: StyleFn) -> NodeMapping {
        NodeMapping { node_type: NodeType::Inline, style: Some(style) }
    }

    fn plain() -> NodeMapping {
        NodeMapping { node_type: NodeType::Inline, style: None }
    }

    fn blank() -> NodeMapping {
        NodeMapping { node_type: NodeType::Blank, style: None }
    }


    fn get(tag_name: &str) -> NodeMapping {
        let header_mapping = NodeMapping {
            node_type: NodeType::Block(BlockType::Block),
            style: Some(StyleData::set_uppercase),
        };
        match tag_name {
            // root elements
            "html" => NodeMapping::with_block(BlockType::Block),
            "body" => NodeMapping::with_block(BlockType::Block),
            //sectioning
            "address" => NodeMapping::with_block(BlockType::Block),
            "article" => NodeMapping::with_block(BlockType::Block),
            "aside" => NodeMapping::with_block(BlockType::Block),
            "footer" => NodeMapping::with_block(BlockType::Block),
            "header" => NodeMapping::with_block(BlockType::Block),
            "h1" => header_mapping,
            "h2" => header_mapping,
            "h3" => header_mapping,
            "h4" => header_mapping,
            "h5" => header_mapping,
            "h6" => header_mapping,
            "hgroup" => NodeMapping::with_block(BlockType::Block),
            "main" => NodeMapping::with_block(BlockType::Block),
            "nav" => NodeMapping::with_block(BlockType::Block),
            "section" => NodeMapping::with_block(BlockType::Block),
            // text content
            "blockquote" => NodeMapping::with_block(BlockType::Quote),
            "dd" => NodeMapping::with_block(BlockType::Block),
            "div" => NodeMapping::with_block(BlockType::Block),
            "dl" => NodeMapping::with_block(BlockType::Block),
            "dt" => NodeMapping::with_block(BlockType::Block),
            "figcaption" => NodeMapping::with_block(BlockType::Block),
            "figure" => NodeMapping::with_block(BlockType::Block),
            "hr" => NodeMapping::with_block(BlockType::Rule),
            "li" => NodeMapping::with_block(
                BlockType::ListItem(ItemizedData::new())),
            "ol" => NodeMapping::with_block(BlockType::OList),
            "p" => NodeMapping::with_block(BlockType::Block),
            "pre" => NodeMapping {
                node_type: NodeType::Block(BlockType::Block),
                style: Some(StyleData::set_preserve_whitespace),
            },
            "ul" => NodeMapping::with_block(BlockType::UList),
            // table
            "caption" => NodeMapping::with_block(BlockType::Block),
            "col" => NodeMapping::blank(),
            "colgroup" => NodeMapping::blank(),
            "table" => NodeMapping::with_block(BlockType::Table),
            "tbody" => NodeMapping::with_block(BlockType::TPart),
            "td" => NodeMapping::with_block(BlockType::Block),
            "tfoot" => NodeMapping::with_block(BlockType::TPart),
            "th" => header_mapping,
            "thead" => NodeMapping::with_block(BlockType::TPart),
            "tr" => NodeMapping::with_block(BlockType::TRow),
            // document metadata
            "base" => NodeMapping::blank(),
            "head" => NodeMapping::blank(),
            "link" => NodeMapping::blank(),
            "meta" => NodeMapping::blank(),
            "style" => NodeMapping::blank(),
            "title" => NodeMapping::blank(),
            // media, this is good enough, img is inline
            "video" => NodeMapping::with_block(BlockType::Block),
            "area" => NodeMapping::blank(),
            "map" => NodeMapping::blank(),
            "track" => NodeMapping::blank(),
            // embedded
            "picture" => NodeMapping::with_block(BlockType::Block),
            "source" => NodeMapping::blank(),
            "iframe" => NodeMapping::blank(),
            // scripting
            "canvas" => NodeMapping::blank(),
            "noscript" => NodeMapping::with_block(BlockType::Block),
            "script" => NodeMapping::blank(),
            // forms
            "button" => NodeMapping::blank(),
            "datalist" => NodeMapping::blank(),
            "fieldset" => NodeMapping::with_block(BlockType::Block),
            "form" => NodeMapping::with_block(BlockType::Block),
            "legend" => NodeMapping::with_block(BlockType::Block),
            "meter" => NodeMapping::blank(),
            "optgroup" => NodeMapping::blank(),
            "option" => NodeMapping::blank(),
            "output" => NodeMapping::blank(),
            "progress" => NodeMapping::blank(),
            "textarea" => NodeMapping::with_block(BlockType::Block),
            // interactive
            "details" => NodeMapping::with_block(BlockType::Block),
            "dialog" => NodeMapping::with_block(BlockType::Block),
            "menu" => NodeMapping::blank(),
            "menuitem" => NodeMapping::blank(),
            "summary" => NodeMapping::with_block(BlockType::Block),
            // web components
            "slot" => NodeMapping::with_block(BlockType::Block),
            "template" => NodeMapping::with_block(BlockType::Block),
            // textual
            "a" => NodeMapping::with_replace(a_element),
            "abbr" => NodeMapping::with_replace(abbr_element),
            "b" => NodeMapping::with_replace(formatted_element!("*{}*")),
            "bdi" => NodeMapping::plain(), // no idea how to support this
            "bdo" => NodeMapping::plain(), // TODO: we can support this
            "br" => NodeMapping::with_replace(br_element),
            "cite" => NodeMapping::plain(),
            "code" => NodeMapping::with_replace(formatted_element!("`{}`")),
            "data" => NodeMapping::plain(),
            "dfn" => NodeMapping::with_replace(formatted_element!("/{}/")),
            "em" => NodeMapping::with_replace(formatted_element!("/{}/")),
            "i" => NodeMapping::with_replace(formatted_element!("/{}/")),
            "kbd" => NodeMapping::with_style(StyleData::set_uppercase),
            "mark" => NodeMapping::with_replace(formatted_element!(">{}<")),
            "q" => NodeMapping::with_replace(formatted_element!("“{}”")),
            "rb" => NodeMapping::plain(),
            "rp" => NodeMapping::plain(),
            "rt" => NodeMapping::plain(),
            "rtc" => NodeMapping::plain(),
            "ruby" => NodeMapping::plain(),
            "s" => NodeMapping::with_style(StyleData::set_strike),
            "samp" => NodeMapping::with_replace(formatted_element!("“{}”")),
            "small" => NodeMapping::plain(), // no good way to support this
            "span" => NodeMapping::plain(),
            "strong" => NodeMapping::with_replace(formatted_element!("*{}*")),
            "sup" => NodeMapping::plain(), // TODO: we can support this
            "sub" => NodeMapping::plain(), // TODO: we can support this
            "time" => NodeMapping::plain(),
            "tt" => NodeMapping::plain(),
            "var" => NodeMapping::with_replace(formatted_element!("`{}`")),
            // intentionally ignoring "wbr"
            // multimedia/images
            "img" => NodeMapping::with_replace(img_element),
            // edits
            "del" => NodeMapping::with_replace(fn_element!(strike)),
            "ins" => NodeMapping::plain(),
            // forms
            "input" => NodeMapping::with_replace(input_element),
            "label" => NodeMapping::plain(),
            "select" => NodeMapping::with_replace(input_element),
            "u" => NodeMapping::with_style(StyleData::set_underline),
            // By default we just treat things as plain "inline"
            _ => NodeMapping::plain(),
        }
    }
}

impl ItemizedData {
    fn new() -> ItemizedData {
        ItemizedData { ordered: false, count: 0, count_length: 0}
    }
    fn set_ordering(&mut self, count: usize, count_length: usize) {
        self.ordered = true;
        self.count = count;
        self.count_length = count_length;
    }
}


impl Block {

    fn from_data(block_type: BlockType, mut data: ElementData) -> Block {
        if let BlockType::OList = block_type {
            let mut total = 0;

            for child in data.children.iter() {
                if let VElementType::Block(el) = child {
                    if let BlockType::ListItem(_) = el.block_type {
                        total += 1;
                    }
                }
            }
            let count_length = format!("{}", total).len();
            let mut count = 0;
            for child in data.children.iter_mut() {
                if let VElementType::Block(el) = child {
                    if let BlockType::ListItem(ref mut item_data) = &mut el.block_type {
                        count += 1;
                        item_data.set_ordering(count, count_length);
                    }
                }
            }
        }
        Block { block_type, data }
    }

    fn block_text(&self, width: Option<Width>) -> String {
        if let BlockType::Rule = self.block_type {
            "-".repeat(width.unwrap_or(3))
        } else {
            let data = match width {
                Some(w) => Some(self.get_block_data(w)),
                None => None,
            };
            generic_block_text(&self.data.children, &data)
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
                    format!("{:>width$}. ", count=id.count, width=id.count_length)
                } else {
                    "* ".to_owned()
                }
            },
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
            },
            BlockType::Quote => "> ".to_owned(),
            _ => "".to_owned(),
        }
    }

    fn get_attr(&self, key: &str) -> Option<Tendril<UTF8>> {
        self.data.get_attr(key)
    }
}

#[derive(Debug, Copy, Clone)]
enum EdgeState {
    Trim,
    White,
    Blank,
}

impl EdgeState {
    fn from(text: &str, style: StyleData) -> (EdgeState, EdgeState) {
        if style.preserve_whitespace {
            (EdgeState::Blank, EdgeState::Blank)
        } else {
            (EdgeState::from_re_find(WHITESPACE_PREFIX.find(text).is_some()),
            EdgeState::from_re_find(WHITESPACE_SUFFIX.find(text).is_some()))
        }
    }

    fn from_re_find(b: bool) -> EdgeState {
        if b { EdgeState::White } else { EdgeState::Blank }
    }

    fn join_str(&self, other: &EdgeState) -> &str {
        match self {
            EdgeState::Trim => "",
            EdgeState::Blank => match other {
                EdgeState::White => " ",
                _ => ""
            },
            EdgeState::White => match other {
                EdgeState::Trim => "",
                _ => " "
            },
        }
    }
}

enum VElementType {
    Text(String, EdgeState, EdgeState),
    Block(Block),
}

impl VElementType {

    fn is_empty(&self) -> bool {
        match self {
            VElementType::Text(text, _, _) => text == "",
            VElementType::Block(_) => false,
        }
    }

    fn width_hint(&self) -> Width {
        match self {
            VElementType::Text(text, _, _) => {
                text.as_str().graphemes(true).count()
            },
            VElementType::Block(block) => {
                if let BlockType::Rule = block.block_type {
                    3
                } else {
                    let mut width = 0;
                    for child in &block.data.children {
                        let child_size = child.width_hint();
                        if child_size > width { width = child_size }
                    }
                    width
                }
            }
        }
    }

    fn from_text(text: &str, style: StyleData) -> VElementType {
        let (prefix, suffix) = EdgeState::from(&text, style);
        let text = if style.preserve_whitespace {
            NEWLINE_EDGES.replace_all(&text, "")
        } else {
            WHITESPACE_AFFIX.replace_all(text.trim(), " ")
        };
        let text = if style.underline { underline(&text) } else { text };
        let text = if style.strike { strike(&text) } else { text };
        let text = if style.uppercase { uppercase(&text) } else { text };
        VElementType::Text(text.to_string(), prefix, suffix)
    }
}

fn get_virtual_elements(node: &Handle, style: StyleData, doc_state: &mut DocState) -> Vec<VElementType> {
    match node.data {
        NodeData::Text { ref contents } => {
                let b = contents.borrow();
                match b.to_string().as_str() {
                    "" => vec![],
                    _ => vec![VElementType::from_text(&b, style)],
                }
            },
        NodeData::Element { ref name, ref attrs, .. } =>  {
                // build vecs
                let tag_name = &name.local[..];
                let mapping = NodeMapping::get(tag_name);
                let style = if let Some(s) = mapping.style { s(style) } else { style };

                let mut child_vec = Vec::new();
                for child in node.children.borrow().iter() {
                    for el in get_virtual_elements(&child.clone(), style, doc_state) {
                        child_vec.push(el);
                    }
                };

                let mut attrs_vec = Vec::new();
                for attr in attrs.borrow().iter() {
                    attrs_vec.push(attr.clone())
                }

                match mapping.node_type {
                    NodeType::Blank => vec![],
                    NodeType::Inline => child_vec,
                    NodeType::Replace(func) => {
                        let data = ElementData::from(attrs_vec, child_vec, style);
                        vec![func(&data, doc_state)]
                    },
                    NodeType::Block(block_type) => {
                        let data = ElementData::from(attrs_vec, child_vec, style);
                        vec![VElementType::Block(Block::from_data(block_type, data))]
                    }
                }
            },
        NodeData::Document => {
            let mut child_vec = Vec::new();
            for child in node.children.borrow().iter() {
                for el in get_virtual_elements(&child.clone(), style, doc_state) {
                    child_vec.push(el);
                }
            };
            let data = ElementData::from(vec![], child_vec, style);
            vec![VElementType::Block(Block::from_data(BlockType::Block, data))]
        }
        _ => vec![],
    }
}

impl ElementData {
    fn from(
            attrs: Vec<Attribute>,
            children: Vec<VElementType>,
            style: StyleData) -> ElementData {
        ElementData { attrs, children, style }
    }

    fn get_text(&self) -> String {
        generic_block_text(&self.children, &None)
    }

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

    fn set_strike(self) -> Self {
        Self {
            strike: true,
            ..self
        }
    }

    fn set_underline(self) -> Self {
        Self {
            underline: true,
            ..self
        }
    }

    fn set_preserve_whitespace(self) -> StyleData {
        Self {
            preserve_whitespace: true,
            ..self
        }
    }

    fn set_uppercase(self) -> StyleData {
        Self {
            uppercase: true,
            ..self
        }
    }
}

struct BlockData<'a> {
    child_block_sep: &'a str,
    first_line_prefix: String,
    next_line_prefix: String,
    width: Width,
}

impl<'a> BlockData<'a> {

    fn get_sub_width(&self) -> Width {
        self.width - self.first_line_prefix.len()
    }

    fn get_sep(&self) -> String {
        format!("{}{}", LINE_SEP, self.next_line_prefix)
    }
}

fn generic_block_text(
        children: &[VElementType],
        block_data: &Option<BlockData>) -> String {

    let mut blocks = Vec::new();
    let mut last_inline_text = String::new();
    let mut had_white_suffix = EdgeState::Trim;
    for child in children.iter() {
        match child {
            VElementType::Text(text, prefix, suffix) => {
                if text == "" {
                    if let EdgeState::Blank = had_white_suffix {
                        had_white_suffix = EdgeState::White;
                    }
                } else {
                    last_inline_text.push_str(had_white_suffix.join_str(prefix));
                    last_inline_text.push_str(text);
                    had_white_suffix = *suffix;
                }
            },
            VElementType::Block(el) => {
                if let Some(data) = &block_data {
                    if last_inline_text.trim() != "" { // TODO: trim?
                        let wrapper = textwrap::Wrapper::new(data.width); // TODO: cache?
                        let wrapped_lines = wrapper.wrap(&last_inline_text);
                        blocks.push(wrapped_lines.join(data.get_sep().as_str()));
                    }
                    let width = Some(data.get_sub_width());
                    let text = if let BlockType::Table = el.block_type {
                        let column_widths = table_column_widths(&el.data.children);
                        let column_widths = recalculate_column_widths(&column_widths, data.width);
                        let rows = table_rows(el, width, &column_widths);
                        rows.iter().filter(|x| !x.is_empty()).join("\n")
                    } else {
                        el.block_text(width)
                    };
                    if !text.is_empty() {
                        let mut block_lines = text.split(LINE_SEP);
                        blocks.push(format!("{}{}", data.first_line_prefix, block_lines.join(data.get_sep().as_str())));
                    }
                    last_inline_text.clear();
                } else {
                    last_inline_text.push_str(had_white_suffix.join_str(&EdgeState::Blank));
                    last_inline_text.push_str(el.block_text(None).as_str())
                }
                had_white_suffix = EdgeState::Trim;
            },
        };
    }
    if let Some(data) = &block_data {
        if last_inline_text != "" {
            // TODO: probably unnecessary
            // let lines = last_inline_text.trim_right_matches(" ").split(LINE_SEP);
            let lines = last_inline_text.split(LINE_SEP);
            let mut wrapped_lines = Vec::new();
            let wrapper = textwrap::Wrapper::new(data.width);
            for line in lines {
                if line == "" {
                    wrapped_lines.push(String::from(""));
                } else {
                    for l in wrapper.wrap(line) {
                        wrapped_lines.push(String::from(&*l));
                    }
                }
            }
            blocks.push(format!("{}{}", data.first_line_prefix, wrapped_lines.join(data.get_sep().as_str())));
        }
        blocks.join(data.child_block_sep)
    } else {
        last_inline_text
    }
}

fn table_column_widths(rows: &[VElementType]) -> Vec<Ratio<Width>> {
    let mut result = Vec::new();
    for child in rows.iter() {
        let child_results = tr_column_widths(&child);
        for child_result in child_results.iter() {
            let min_len = if result.len() < child_result.len() {
                result.len() } else { child_result.len() };
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

fn tr_column_widths(row: &VElementType) -> Vec<Vec<Ratio<Width>>> {
    match row {
        VElementType::Text(_, _, _) => vec!(vec!(Ratio::new(row.width_hint(), 1))),
        VElementType::Block(block) => {
            match block.block_type {
                BlockType::TPart => {
                    let mut result = Vec::new();
                    for child in block.data.children.iter() {
                        result.extend(tr_column_widths(&child));
                    }
                    result
                }, BlockType::TRow => {
                    let mut result = Vec::new();
                    for child in block.data.children.iter() {
                        if child.is_empty() { continue; }
                        let span: Width = match child {
                            VElementType::Block(block) => {
                                match block.get_attr("colspan") {
                                    Some(s) => s.parse::<Width>().unwrap_or(1),
                                    None => 1,
                                }
                            },
                            _ => 1
                        };
                        for _ in 0..span {
                            result.push(Ratio::new(child.width_hint(), span));
                        }
                    }
                    vec!(result)
                }, _ => vec!(vec!(Ratio::new(row.width_hint(), 1))),

            }
        }
    }
}

fn table_rows(table: &Block, max_width: Option<Width>, column_widths: &[Width]) -> Vec<String> {
    let mut rows = Vec::new();
    for maybe_row in table.data.children.iter() {
        if maybe_row.is_empty() { continue; }
        match maybe_row {
            VElementType::Text(text, _, _) => rows.push(text.to_owned()),
            VElementType::Block(block) => {
                match block.block_type {
                    BlockType::TPart => {
                        let inner_rows = table_rows(block, max_width, column_widths);
                        rows.extend(inner_rows);
                    },
                    BlockType::TRow => {
                        rows.push(tr_text(&block, max_width, &column_widths))
                    },
                    _ => rows.push(block.block_text(max_width))
                }
            },
        }
    }
    rows
}

fn tr_text(row: &Block, max_width: Option<Width>, column_widths: &[Width]) -> String {
    if max_width.is_none() {
        row.block_text(None)
    } else {
        let mut cells = Vec::new();
        let mut idx = 0;
        let mut max_height = 0;
        for child in row.data.children.iter() {
            if child.is_empty() { continue; }
            let span = if let VElementType::Block(b) = child {
                match b.get_attr("colspan") {
                    Some(s) => s.parse::<Width>().unwrap_or(1),
                    None => 1,
                }
            } else { 1 };

            if idx + span > column_widths.len() {
                panic!("thing too big {} {} {}", idx, span, column_widths.len());
            }
            let width = column_widths[idx..idx+span].iter().sum();

            let text = match child {
                VElementType::Text(text, _, _) => text.to_owned() ,
                VElementType::Block(b) => b.block_text(Some(width)),
            };

            let height = text.lines().count();
            if max_height < height { max_height = height }
            cells.push((width, text));
            idx += span;
            if idx > column_widths.len() { break; }
        }
        

        let mut lines = vec![String::new(); max_height];
        let mut first = true;

        for (width, content) in cells {
            let mut height = 0;
            for (idx, c_line) in content.lines().enumerate() {
                height = idx+1;
                if !first { lines[idx].push_str(COLUMN_SEP); }
                lines[idx].push_str(c_line);
                let c_line_len = c_line.graphemes(true).count();
                if width > c_line_len {
                    lines[idx].push_str(" ".repeat(width - c_line_len).as_str());
                }
            }
            // here we're just evening out all the cell heights
            for item in lines.iter_mut().take(max_height).skip(height) {
                if !first { item.push_str(COLUMN_SEP); }
                item.push_str(" ".repeat(width).as_str())
            }
            first = false;
            
        }
        lines.iter().map(|l| l.trim_right()).join("\n")
    }
}


fn recalculate_column_widths(widths: &[Ratio<Width>], max_width: Width) -> Vec<Width> {

    let mut result: Vec<Width> = vec![0; widths.len()];
    if !widths.is_empty() {
        let usable_max = Ratio::new(max_width - COLUMN_SEP.len() * (widths.len() - 1), 1);
        let desired_max: Ratio<Width> = widths.iter().sum();

        if !desired_max.is_zero() {
            let ratio = if usable_max < desired_max {
                usable_max / desired_max } else { Ratio::new(1, 1) };
            let mut float_widths = widths.iter().map(|x| ratio * x)
                .enumerate().sorted_by(|a, b| Ord::cmp(&b.1, &a.1))
                .collect::<Vec<(usize, Ratio<usize>)>>();
            let mut balance: Ratio<usize> = Ratio::from_integer(0);
            let mut positive = true;
            let mut start_idx = 0;
            let mut end_idx = widths.len() - 1;
            for _ in 0..widths.len() {
                let idx = if positive { start_idx } else { end_idx };
                if positive { start_idx += 1 } else { end_idx -= 1 }
                let (width_idx, mut width_value) = float_widths[idx];
                let mut new_width_ratio = if positive { width_value.floor() }
                    else { width_value.ceil() };

                float_widths[idx] = (width_idx, new_width_ratio);
                if positive { new_width_ratio += balance }
                    else { width_value += balance }
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
    let start_widths: Vec<Ratio<Width>> = vec![
        Ratio::new(2, 1), Ratio::new(4, 1), Ratio::new(6, 1)];
    let end_widths: Vec<Width> = vec![2, 4, 6];
    assert_eq!(end_widths, recalculate_column_widths(&start_widths, 80));
}

#[test]
fn test_long_recalculate() {
    let start_widths: Vec<Ratio<Width>> = vec![
        Ratio::new(12, 1), Ratio::new(66, 1), Ratio::new(65, 1)];
    let expected_widths: Vec<Width> = vec![7, 35, 34];
    assert_eq!(expected_widths, recalculate_column_widths(&start_widths, 80));
}


#[test]
fn test_long_recalculate2() {
    let start_widths: Vec<Ratio<Width>> = vec![
        Ratio::new(10, 1), Ratio::new(66, 1), Ratio::new(65, 1)];
    let expected_widths: Vec<Width> = vec![6, 35, 35];
    assert_eq!(expected_widths, recalculate_column_widths(&start_widths, 80));
}


fn walk(handle: &Handle, text : &mut String, width: Width) {
    let mut doc_state = HashSet::new();
    for el_type in get_virtual_elements(&handle, StyleData::new(), &mut doc_state) {
        match el_type {
            VElementType::Text(in_text, _, _) => text.push_str(&in_text),
            VElementType::Block(e) => text.push_str(&e.block_text(Some(width))),
        }
    }
}

pub fn convert(input: &str, width: Width) -> String {
    let parser = parse_document(RcDom::default(), Default::default());
    let dril = Tendril::from_str(input).unwrap();
    let dom = parser.one(dril);
    convert_dom(&dom, width)
}

fn convert_dom(dom: &RcDom, width: Width) -> String {
    let mut output = String::new();
    walk(&dom.document, &mut output, width);
    output
}