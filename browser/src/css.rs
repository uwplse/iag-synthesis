//! A simple parser for a tiny subset of CSS.
//!
//! To support more CSS syntax, it would probably be easiest to replace this
//! hand-rolled parser with one based on a library or parser generator.

use std::fmt;

// Data structures:

#[derive(Debug)]
pub struct Stylesheet {
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub struct Rule {
    pub selectors: Vec<Selector>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Selector {
    Simple(SimpleSelector),
}

#[derive(Debug)]
pub struct SimpleSelector {
    pub tag: Option<String>,
    pub id: Option<String>,
    pub class: Vec<String>,
}

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    pub value: Value,
}

// pub struct Position<T> {
//     pub left: T,
//     pub right: T,
//     pub top: T,
//     pub bottom: T,
// }
//
// pub struct Properties {
//     pub border_style: props::BorderStyle,
//     pub border_width: Position<props::BorderWidth>,
//     pub margin: Position<props::Margin>,
// }
//
// pub mod props {
//     use css::*;
//
//     pub trait Property: Debug {
//         fn name() -> &'static str;
//         fn initial() -> Self;
//     }
//
//     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
//     pub enum Display {
//         None,
//         Block,
//         Inline,
//         InlineBlock,
//     }
//
//     impl Default for Display {
//         fn default() -> Self { Display::Inline }
//     }
//
//     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
//     pub enum Position {
//         Static,
//         Relative,
//         Absolute,
//         // Sticky,
//         Fixed,
//     }
//
//     impl Default for Position {
//         fn default() -> Self { Position::Static }
//     }
//
//     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
//     pub enum BorderStyle {
//         None,
//         Hidden,
//         Dotted,
//         Dashed,
//         Solid,
//         Double,
//         Groove,
//         Ridge,
//         Inset,
//         Outset,
//     }
//
//     impl Default for BorderStyle {
//         fn default() -> Self { BorderStyle::None }
//     }
//
//     #[derive(Debug, Clone, Copy, PartialEq)]
//     pub enum BorderWidth {
//         Thin,
//         Medium,
//         Thick,
//         Absolute(Length)
//     }
//
//     impl Default for BorderWidth {
//         fn default() -> Self { BorderWidth::Medium }
//     }
//
//     pub enum Margin {
//         Auto,
//         Absolute(Length),
//     }
//
//     impl Default for Margin {
//         fn default() -> Self { Margin::Absolute(Length::default()) }
//     }
// }
//
// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct Length(f32, Unit);
//
// impl Default for Length {
//     fn default() -> Self { Length(f32::default(), Unit::Px) }
// }
//
// pub enum SpecifiedValue<V> {
//     Initial,
//     Inherit,
//     Value(V),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Keyword(String),
    Length(f32, Unit),
    ColorValue(Color),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unit {
    Px,
    // Em,
    // Pt,
    // Cm,
    // Mm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

pub type Specificity = (usize, usize, usize);

impl Selector {
    pub fn specificity(&self) -> Specificity {
        // http://www.w3.org/TR/selectors/#specificity
        let Selector::Simple(ref simple) = *self;
        let a = simple.id.iter().count();
        let b = simple.class.len();
        let c = simple.tag.iter().count();
        (a, b, c)
    }
}

impl Color {
    pub fn alpha(&self) -> f32 {
        self.a as f32 / 255.0
    }

    pub fn channels(&self) -> (f32, f32, f32) {
        let r = self.r as f32 / 255.0;
        let g = self.g as f32 / 255.0;
        let b = self.b as f32 / 255.0;
        (r, g, b)
    }

    pub fn rgb(&self) -> (u8, u8, u8) {
        let r = ((self.r as u16) * (self.a as u16) / 255) as u8;
        let g = ((self.g as u16) * (self.a as u16) / 255) as u8;
        let b = ((self.b as u16) * (self.a as u16) / 255) as u8;
        (r, g, b)
    }

    pub fn over(&self, below: &Self) -> Color {
        let (red_a, green_a, blue_a) = self.channels();
        let (red_b, green_b, blue_b) = below.channels();

        let alpha_a = self.alpha();
        let alpha_b = below.alpha();
        let alpha_c = alpha_a + alpha_b * (1.0 - alpha_a);

        let compose = |channel_a, channel_b| {
            (channel_a * alpha_a + channel_b * alpha_b * (1.0 - alpha_a)) / alpha_c
        };
        let scale = |channel| (255.0 * channel) as u8;

        Color {
            r: scale(compose(red_a, red_b)),
            g: scale(compose(green_a, green_b)),
            b: scale(compose(blue_a, blue_b)),
            a: scale(alpha_c),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Keyword(ref kw) => write!(f, "\"{}\"", kw),
            Value::Length(l, u) => write!(f, "{}{}", l, u),
            Value::ColorValue(c) => write!(f, "{}", c)
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unit::Px => write!(f, "px"),
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rgba({}, {}, {}, {:.1})", self.r, self.g, self.b, self.alpha())
    }
}

/// Parse a whole CSS stylesheet.
pub fn parse(source: String) -> Stylesheet {
    let mut parser = Parser { pos: 0, input: source };
    Stylesheet { rules: parser.parse_rules() }
}

struct Parser {
    pos: usize,
    input: String,
}

impl Parser {
    /// Parse a list of rule sets, separated by optional whitespace.
    fn parse_rules(&mut self) -> Vec<Rule> {
        let mut rules = Vec::new();
        loop {
            self.consume_whitespace();
            if self.eof() { break }
            rules.push(self.parse_rule());
        }
        rules
    }

    /// Parse a rule set: `<selectors> { <declarations> }`.
    fn parse_rule(&mut self) -> Rule {
        Rule {
            selectors: self.parse_selectors(),
            declarations: self.parse_declarations(),
        }
    }

    /// Parse a comma-separated list of selectors.
    fn parse_selectors(&mut self) -> Vec<Selector> {
        let mut selectors = Vec::new();
        loop {
            selectors.push(Selector::Simple(self.parse_simple_selector()));
            self.consume_whitespace();
            match self.next_char() {
                ',' => { self.consume_char(); self.consume_whitespace(); }
                '{' => break,
                c   => panic!("Unexpected character {} in selector list", c)
            }
        }
        // Return selectors with highest specificity first, for use in matching.
        selectors.sort_by(|a,b| b.specificity().cmp(&a.specificity()));
        selectors
    }

    /// Parse one simple selector, e.g.: `type#id.class1.class2.class3`
    fn parse_simple_selector(&mut self) -> SimpleSelector {
        let mut selector = SimpleSelector { tag: None, id: None, class: Vec::new() };
        while !self.eof() {
            match self.next_char() {
                '#' => {
                    self.consume_char();
                    selector.id = Some(self.parse_identifier());
                }
                '.' => {
                    self.consume_char();
                    selector.class.push(self.parse_identifier());
                }
                '*' => {
                    // universal selector
                    self.consume_char();
                }
                c if valid_identifier_char(c) => {
                    selector.tag = Some(self.parse_identifier());
                }
                _ => break
            }
        }
        selector
    }

    /// Parse a list of declarations enclosed in `{ ... }`.
    fn parse_declarations(&mut self) -> Vec<Declaration> {
        assert_eq!(self.consume_char(), '{');
        let mut declarations = Vec::new();
        loop {
            self.consume_whitespace();
            if self.next_char() == '}' {
                self.consume_char();
                break;
            }
            declarations.push(self.parse_declaration());
        }
        declarations
    }

    /// Parse one `<property>: <value>;` declaration.
    fn parse_declaration(&mut self) -> Declaration {
        let property_name = self.parse_identifier();
        self.consume_whitespace();
        assert_eq!(self.consume_char(), ':');
        self.consume_whitespace();
        let value = self.parse_value();
        self.consume_whitespace();
        assert_eq!(self.consume_char(), ';');

        Declaration {
            name: property_name,
            value: value,
        }
    }

    // Methods for parsing values:

    fn parse_value(&mut self) -> Value {
        match self.next_char() {
            '0'...'9' => self.parse_length(),
            '#' => self.parse_color(),
            _ => Value::Keyword(self.parse_identifier())
        }
    }

    fn parse_length(&mut self) -> Value {
        Value::Length(self.parse_float(), self.parse_unit())
    }

    fn parse_float(&mut self) -> f32 {
        let s = self.consume_while(|c| match c {
            '0'...'9' | '.' => true,
            _ => false
        });
        s.parse().unwrap()
    }

    fn parse_unit(&mut self) -> Unit {
        match &*self.parse_identifier().to_ascii_lowercase() {
            "px" => Unit::Px,
            _ => panic!("unrecognized unit")
        }
    }

    fn parse_color(&mut self) -> Value {
        assert_eq!(self.consume_char(), '#');
        Value::ColorValue(Color {
            r: self.parse_hex_pair(),
            g: self.parse_hex_pair(),
            b: self.parse_hex_pair(),
            a: 255
        })
    }

    /// Parse two hexadecimal digits.
    fn parse_hex_pair(&mut self) -> u8 {
        let s = &self.input[self.pos .. self.pos + 2];
        self.pos += 2;
        u8::from_str_radix(s, 16).unwrap()
    }

    /// Parse a property name or keyword.
    fn parse_identifier(&mut self) -> String {
        self.consume_while(valid_identifier_char)
    }

    /// Consume and discard zero or more whitespace characters.
    fn consume_whitespace(&mut self) {
        self.consume_while(char::is_whitespace);
    }

    /// Consume characters until `test` returns false.
    fn consume_while<F>(&mut self, test: F) -> String
            where F: Fn(char) -> bool {
        let mut result = String::new();
        while !self.eof() && test(self.next_char()) {
            result.push(self.consume_char());
        }
        result
    }

    /// Return the current character, and advance self.pos to the next character.
    fn consume_char(&mut self) -> char {
        let mut iter = self.input[self.pos..].char_indices();
        let (_, cur_char) = iter.next().unwrap();
        let (next_pos, _) = iter.next().unwrap_or((1, ' '));
        self.pos += next_pos;
        cur_char
    }

    /// Read the current character without consuming it.
    fn next_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }

    /// Return true if all input is consumed.
    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

fn valid_identifier_char(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '0'...'9' | '-' | '_' => true, // TODO: Include U+00A0 and higher.
        _ => false,
    }
}
