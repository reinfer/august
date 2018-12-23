#[macro_use] extern crate lazy_static;

use std::collections::HashSet;
use std::default::Default;
use std::string::String;
use std::str::FromStr;

use regex::Regex;
use textwrap;
use num_rational::Ratio;
use num_traits::Zero;
use itertools::Itertools;
use html5ever::{parse_document, rcdom::{NodeData, RcDom, Handle}};
use html5ever::{interface::Attribute, tendril::{Tendril, TendrilSink}};
use unicode_segmentation::UnicodeSegmentation;


lazy_static! {
    static ref WHITESPACE_AFFIX: Regex = Regex::new("\\s+").unwrap();
    static ref WHITESPACE_PREFIX: Regex = Regex::new("^\\s+").unwrap();
    static ref WHITESPACE_SUFFIX: Regex = Regex::new("\\s+$").unwrap();
}

const COLUMN_SEP: &str = "  ";
const LINE_SEP: &str = "\n";

pub type Width = usize;

struct SizeHint {
    width: Width,
}

struct ElementData {
    attrs: Vec<Attribute>,
    children: Vec<ElementType>,
}

macro_rules! formatted_element {
    ($fmt:expr) => {
        |element: &ElementData, _: &mut DocState| {
            let text = element.get_text();
            ElementType::from_text(format!($fmt, text).as_str())
        }
    };
}

fn s_element (element: &ElementData, _: &mut DocState) -> ElementType {
    let text = element.get_text();
    let text_len = text.as_str().graphemes(true).count();
    let suffix = if text_len > 10 {
        let wordcount  = text.split_whitespace().count();
        "^W".repeat(wordcount)
    } else {
        "^H".repeat(text_len)
    };
    ElementType::from_text(format!("{}{}", text, suffix).as_str())
}

fn a_element(element: &ElementData, _: &mut DocState) -> ElementType {
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
    ElementType::from_text(text.as_str())
}

fn abbr_element(element: &ElementData, doc_state: &mut DocState) -> ElementType {
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
    ElementType::from_text(text.as_str())
}

fn img_element(element: &ElementData, _: &mut DocState) -> ElementType {
    if let Some(alt) = element.get_attr("alt") {
        ElementType::from_text(alt.as_str())
    } else {
        ElementType::from_text("")
    }
}

fn br_element(_: &ElementData, _: &mut DocState) -> ElementType {
    ElementType::Text("\n".to_owned(), EdgeState::Trim, EdgeState::Trim)
}

type DocState = HashSet<String>;
type InlineFn = fn(&ElementData, &mut DocState) -> ElementType;

fn get_inline_fn(tag_name: &str) -> Option<InlineFn> {
    match tag_name {
        "span" => Some(formatted_element!("{}")),
        "b" => Some(formatted_element!("*{}*")),
        "strong" => Some(formatted_element!("*{}*")),
        "i" => Some(formatted_element!("/{}/")),
        "em" => Some(formatted_element!("/{}/")),
        "u" => Some(formatted_element!("_{}_")),
        "s" => Some(s_element),
        "a" => Some(a_element),
        "abbr" => Some(abbr_element),
        "img" => Some(img_element),
        "br" => Some(br_element),
        _ => None,
    }
}

struct ItemizedData {
    ordered: bool,
    count: usize,
    count_length: usize,
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

enum BlockType {
    BlockElement,
    HeaderElement,
    BlockquoteElement,
    TableElement,
    TPartElement,
    TrElement,
    UlElement,
    OlElement,
    LiElement,
    HrElement,
}

struct Block {
    tag: BlockType,
    attrs: Vec<Attribute>,
    children: Vec<ElementType>,
    item_data: ItemizedData,
}

impl Block {
    fn from_tag_name(tag_name: &str, attrs: Vec<Attribute>, children: Vec<ElementType>) -> Option<Block> {
        let tag = match tag_name {
            "div" => Some(BlockType::BlockElement),
            "p" => Some(BlockType::BlockElement),
            "td" => Some(BlockType::BlockElement),
            "h1" => Some(BlockType::HeaderElement),
            "h2" => Some(BlockType::HeaderElement),
            "h3" => Some(BlockType::HeaderElement),
            "h4" => Some(BlockType::HeaderElement),
            "h5" => Some(BlockType::HeaderElement),
            "h6" => Some(BlockType::HeaderElement),
            "th" => Some(BlockType::HeaderElement),
            "hr" => Some(BlockType::HrElement),
            "table" => Some(BlockType::TableElement),
            "tbody" => Some(BlockType::TPartElement),
            "thead" => Some(BlockType::TPartElement),
            "tfoot" => Some(BlockType::TPartElement),
            "tr" => Some(BlockType::TrElement),
            "ul" => Some(BlockType::UlElement),
            "ol" => Some(BlockType::OlElement),
            "li" => Some(BlockType::LiElement),
            "blockquote" => Some(BlockType::BlockquoteElement),
            "head" => None,
            "meta" => None,
            "title" => None,
            "script" => None,
            "style" => None,
            "colgroup" => None,
            "col" => None,
            _ => Some(BlockType::BlockElement),
        };
        match tag {
            Some(tag) => Some(Block::from_data(tag, attrs, children)),
            None => None,
        }
    }

    fn from_data(tag: BlockType, attrs: Vec<Attribute>, mut children: Vec<ElementType>) -> Block {
        if let BlockType::OlElement = tag {
            let mut total = 0;

            for child in children.iter() {
                if let ElementType::Block(el) = child {
                    if let BlockType::LiElement = el.tag {
                        total += 1;
                    }
                }
            }
            let count_length = format!("{}", total).len();
            let mut count = 0;
            for child in children.iter_mut() {
                if let ElementType::Block(el) = child {
                    count += 1;
                    if let BlockType::LiElement = el.tag {
                        el.item_data.set_ordering(count, count_length);
                    }
                }
            }
        }
        Block { tag, attrs, children, item_data: ItemizedData::new() }
    }

    fn block_text(&self, width: Option<Width>) -> String {
        if let BlockType::HrElement = self.tag {
            "-".repeat(width.unwrap_or(3))
        } else {
            let data = match width {
                Some(w) => Some(self.get_block_data(w)),
                None => None,
            };
            let result = generic_block_text(&self.children, data);
            if let BlockType::HeaderElement = self.tag {
                result.to_uppercase()
            } else {
                result
            }
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
        match self.tag {
            BlockType::OlElement => "\n",
            BlockType::UlElement => "\n",
            BlockType::BlockquoteElement => "\n>\n",
            _ => "\n\n",
        }
    }

    fn get_first_line_prefix(&self) -> String {
        match self.tag {
            BlockType::LiElement => {
                let id = &self.item_data;
                if id.ordered {
                    format!("{:>width$}. ", count=id.count, width=id.count_length)
                } else {
                    "* ".to_owned()
                }
            },
            BlockType::BlockquoteElement => "> ".to_owned(),
            _ => "".to_owned(),
        }
    }

    fn get_next_line_prefix(&self) -> String {
        match self.tag {
            BlockType::LiElement => {
                let id = &self.item_data;
                if id.ordered {
                    return " ".repeat(id.count_length + 2)
                } else {
                    "  ".to_owned()
                }
            },
            BlockType::BlockquoteElement => "> ".to_owned(),
            _ => "".to_owned(),
        }
    }

    fn get_attr(&self, key: &str) -> Option<String> {
        let mut result: Option<String> = None;
        for attr in self.attrs.iter() {
            if attr.name.local.to_string() == key {
                result = Some(String::from(attr.value.clone()));
                break;
            }
        }
        result
    }
}

#[derive(Debug, Copy, Clone)]
enum EdgeState {
    Trim,
    White,
    Blank,
}

impl EdgeState {
    fn from(text: &str) -> (EdgeState, EdgeState) {
        (EdgeState::from_re_find(WHITESPACE_PREFIX.find(text).is_some()),
         EdgeState::from_re_find(WHITESPACE_SUFFIX.find(text).is_some()))
    }

    fn from_re_find(b: bool) -> EdgeState {
        match b {
            true => EdgeState::White, 
            false => EdgeState::Blank, 
        }
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

enum ElementType {
    Text(String, EdgeState, EdgeState),
    Block(Block),
}

impl ElementType {

    fn is_empty(&self) -> bool {
        match self {
            ElementType::Text(text, _, _) => text == "",
            ElementType::Block(_) => false,
        }
    }

    fn size_hint(&self) -> SizeHint {
        match self {
            ElementType::Text(text, _, _) => {
                SizeHint { width: text.as_str().graphemes(true).count() }
            },
            ElementType::Block(block) => {
                if let BlockType::HrElement = block.tag {
                    SizeHint { width: 3 }
                } else {
                    let mut width = 0;
                    for child in &block.children {
                        let child_size = child.size_hint();
                        if child_size.width > width { width = child_size.width }
                    }
                    SizeHint { width }
                }
            }
        }
    }

    fn from_text(text: &str) -> ElementType {
        let (prefix, suffix) = EdgeState::from(text);
        let sub = WHITESPACE_AFFIX.replace_all(text.trim(), " ");
        ElementType::Text(sub.to_string(), prefix, suffix)
    }
}

fn get_august_element(node: Handle, doc_state: &mut DocState) -> Option<ElementType> {
    match node.data {
        NodeData::Text { ref contents }
            => {
                let b = contents.borrow();
                match b.to_string().as_str() {
                    "" => None,
                    _ => Some(ElementType::from_text(
                        b.clone().to_string().as_str())),
                }
                },
        NodeData::Element { ref name, ref attrs, .. }
            =>  {
                // build vecs
                let mut child_vec = Vec::new();
                for child in node.children.borrow().iter() {
                    if let Some(el) = get_august_element(child.clone(), doc_state) {
                        child_vec.push(el);
                    }
                };
                let mut attrs_vec = Vec::new();
                for attr in attrs.borrow().iter() {
                    attrs_vec.push(attr.clone())
                }

                let tag_name = &name.local[..];
                if let Some(func) = get_inline_fn(tag_name) {
                    let data = ElementData::from(attrs_vec, child_vec);
                    Some(func(&data, doc_state))
                } else {
                    if let Some(block) = Block::from_tag_name(tag_name, attrs_vec, child_vec) {
                        Some(ElementType::Block(block))
                    } else {
                        None
                    }
                }
            },
        NodeData::Document => {
            let mut child_vec = Vec::new();
            for child in node.children.borrow().iter() {
                if let Some(el) = get_august_element(child.clone(), doc_state) {
                    child_vec.push(el);
                }
            };
            Some(ElementType::Block(Block::from_data(
                BlockType::BlockElement, Vec::new(), child_vec)))
        }
        _ => None,
    }
}

impl ElementData {
    fn from(
            attrs: Vec<Attribute>,
            children: Vec<ElementType>) -> ElementData {
        ElementData { attrs, children }
    }

    fn get_text(&self) -> String {
        generic_block_text(&self.children, None)
    }

    fn get_attr(&self, key: &str) -> Option<String> {
        let mut result: Option<String> = None;
        for attr in self.attrs.iter() {
            if attr.name.local.to_string() == key {
                result = Some(String::from(attr.value.clone()));
                break;
            }
        }
        result
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
        children: &Vec<ElementType>,
        block_data: Option<BlockData>) -> String {

    let mut blocks = Vec::new();
    let mut last_inline_text = String::new();
    let mut had_white_suffix = EdgeState::Trim;
    for child in children.iter() {
        match child {
            ElementType::Text(text, prefix, suffix) => {
                if text == "" {
                    if let EdgeState::Blank = had_white_suffix {
                        had_white_suffix = EdgeState::White;
                    }
                } else {
                    last_inline_text.push_str(had_white_suffix.join_str(prefix));
                    last_inline_text.push_str(text);
                    had_white_suffix = suffix.clone();
                }
            },
            ElementType::Block(el) => {
                if let Some(data) = &block_data {
                    if last_inline_text.trim() != "" {
                        let wrapper = textwrap::Wrapper::new(data.width); // TODO: cache?
                        let wrapped_lines = wrapper.wrap(&last_inline_text);
                        blocks.push(wrapped_lines.join(data.get_sep().as_str()));
                    }
                    let width = Some(data.get_sub_width());
                    let text = if let BlockType::TableElement = el.tag {
                        let column_widths = table_column_widths(&el.children);
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

fn table_column_widths(rows: &Vec<ElementType>) -> Vec<Ratio<Width>> {
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

fn tr_column_widths(row: &ElementType) -> Vec<Vec<Ratio<Width>>> {
    match row {
        ElementType::Text(_, _, _) => vec!(vec!(Ratio::new(row.size_hint().width, 1))),
        ElementType::Block(block) => {
            match block.tag {
                BlockType::TPartElement => {
                    let mut result = Vec::new();
                    for child in block.children.iter() {
                        result.extend(tr_column_widths(&child));
                    }
                    result
                }, BlockType::TrElement => {
                    let mut result = Vec::new();
                    for child in block.children.iter() {
                        if child.is_empty() { continue; }
                        let span: Width = match child {
                            ElementType::Block(block) => {
                                match block.get_attr("colspan") {
                                    Some(s) => s.parse::<Width>().unwrap_or(1),
                                    None => 1,
                                }
                            },
                            _ => 1
                        };
                        result.push(Ratio::new(child.size_hint().width, span));
                    }
                    vec!(result)
                }, _ => vec!(vec!(Ratio::new(row.size_hint().width, 1))),

            }
        }
    }
}

fn table_rows(table: &Block, max_width: Option<Width>, column_widths: &Vec<Width>) -> Vec<String> {
    let mut rows = Vec::new();
    for maybe_row in table.children.iter() {
        if maybe_row.is_empty() { continue; }
        match maybe_row {
            ElementType::Text(text, _, _) => rows.push(text.to_owned()),
            ElementType::Block(block) => {
                match block.tag {
                    BlockType::TPartElement => {
                        let inner_rows = table_rows(block, max_width, column_widths);
                        rows.extend(inner_rows);
                    },
                    BlockType::TrElement => {
                        rows.push(tr_text(&block, max_width, &column_widths))
                    },
                    _ => rows.push(block.block_text(max_width))
                }
            },
        }
    }
    rows
}

fn tr_text(row: &Block, max_width: Option<Width>, column_widths: &Vec<Width>) -> String {
    if max_width.is_none() {
        row.block_text(None)
    } else {
        let mut cells = Vec::new();
        let mut idx = 0;
        let mut max_height = 0;
        for child in row.children.iter() {
            if child.is_empty() { continue; }
            let span = if let ElementType::Block(b) = child {
                match b.get_attr("colspan") {
                    Some(s) => s.parse::<Width>().unwrap_or(1),
                    None => 1,
                }
            } else { 1 };

            let width = column_widths[idx..idx+span].iter().sum();

            let text = match child {
                ElementType::Text(text, _, _) => text.to_owned() ,
                ElementType::Block(b) => b.block_text(Some(width)),
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
            for idx in height..max_height {
                if !first { lines[idx].push_str(COLUMN_SEP); }
                lines[idx].push_str(" ".repeat(width).as_str())
            }
            first = false;
            
        }
        lines.iter().map(|l| l.trim_right()).join("\n")
    }
}


fn recalculate_column_widths(
        widths: &Vec<Ratio<Width>>, max_width: Width) -> Vec<Width> {

    let mut result: Vec<Width> = vec![0; widths.len()];
    if widths.len() > 0 {
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


fn walk(handle: Handle, text : &mut String, width: Width) {
    let mut doc_state = HashSet::new();
    if let Some(el_type) = get_august_element(handle, &mut doc_state) {
        match el_type {
            ElementType::Text(in_text, _, _) => text.push_str(&in_text),
            ElementType::Block(e) => text.push_str(&e.block_text(Some(width))),
        }
    }
}

pub fn convert(input: &str, width: Width) -> String {
    let parser = parse_document(RcDom::default(), Default::default());
    let dril = Tendril::from_str(input).unwrap();
    let dom = parser.one(dril);
    convert_dom(dom, width)
}

fn convert_dom(dom: RcDom, width: Width) -> String {
    let mut output = String::new();
    walk(dom.document, &mut output, width);
    output
}