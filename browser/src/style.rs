//! Code for applying CSS styles to the DOM.
//!
//! This is not very interesting at the moment.  It will get much more
//! complicated if I add support for compound selectors.

use crate::dom::{Node, NodeType, ElementData};
use crate::css::{Stylesheet, Rule, Selector, SimpleSelector, Value, Unit, Color, Specificity};
use std::convert::{TryFrom, TryInto};

/// A node with associated style data.
pub struct StyledNode<'a> {
    pub node: &'a Node,
    pub specified: Style,
    pub children: Vec<StyledNode<'a>>,
}

/// Bundled edge offsets.
#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Edge<T> {
    pub left: T,
    pub right: T,
    pub top: T,
    pub bottom: T,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Display {
    Inline,
    Block,
    Float, // A virtual layout mode for `display: block` with `float: left|right`.
    None,
}

impl Display {
    pub fn floated(self, floating: Float) -> Display {
        if self == Display::None || floating == Float::None {
            self
        } else {
            Display::Float
        }
    }
}

impl Default for Display {
    fn default() -> Self { Display::Inline }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Float {
    Left,
    Right,
    None
}

impl Default for Float {
    fn default() -> Self { Float::None }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Clear {
    pub left: bool,
    pub right: bool,
}

impl Clear {
    const NONE: Clear = Clear { left: false, right: false };
    const LEFT: Clear = Clear { left: true, right: false };
    const RIGHT: Clear = Clear { left: false, right: true };
    const BOTH: Clear = Clear { left: true, right: true };
}

impl Default for Clear {
    fn default() -> Self { Clear::NONE }
}

/// A length measured in standard pixels.
pub type Pixels = f32;

/// A potentially automatically calculated length.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Automatic<V> {
    Auto,
    Given(V),
}

impl<V: Default + Copy> Automatic<V> {
    /// Get the given value or the default for its type.
    pub fn value(&self) -> V {
        match self {
            Automatic::Auto => V::default(),
            Automatic::Given(v) => *v,
        }
    }

    /// Give a value to use if the wrapper is automatic.
    pub fn give(&self, v: V) -> Self {
        match self {
            Automatic::Auto => Automatic::Given(v),
            Automatic::Given(_) => *self,
        }
    }

    /// Take the value from the wrapper, using the provided value if automatic.
    pub fn take(&self, v: V) -> V {
        match self {
            Automatic::Auto => v,
            Automatic::Given(v) => *v,
        }
    }

    /// Is the wrapper set to automatic?
    pub fn is_auto(&self) -> bool {
        match self {
            Automatic::Auto => true,
            Automatic::Given(_) => false
        }
    }

    /// Is the wrapper set to a given value?
    pub fn is_given(&self) -> bool {
        match self {
            Automatic::Auto => false,
            Automatic::Given(_) => true
        }
    }
}

impl<V> Default for Automatic<V> {
    fn default() -> Self { Automatic::Auto }
}

impl<V> From<V> for Automatic<V> {
    fn from(v: V) -> Self { Automatic::Given(v) }
}

impl TryFrom<&Value> for Color {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::ColorValue(v) => Ok(*v),
            _ => Err(format!("expected color but found {}", v)),
        }
    }
}

impl TryFrom<&Value> for Automatic<Pixels> {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Length(px, Unit::Px) => Ok(Automatic::Given(*px)),
            Value::Keyword(kw) if kw == "auto" => Ok(Automatic::Auto),
            _ => Err(format!("expected auto/length but found {}", v)),
        }
    }
}

impl TryFrom<&Value> for Pixels {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Length(l, Unit::Px) => Ok(*l),
            _ => Err(format!("expected auto/length but found {}", v)),
        }
    }
}

impl TryFrom<&Value> for Clear {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => {
                match kw.as_str() {
                    "left" => Ok(Clear::LEFT),
                    "right" => Ok(Clear::RIGHT),
                    "both" => Ok(Clear::BOTH),
                    "none" => Ok(Clear::NONE),
                    _ => Err(format!("invalid floating mode \"{}\"", kw)),
                }
            }
            _ => Err(format!("expected floating mode but found {}", v)),
        }
    }
}

impl TryFrom<&Value> for Float {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => {
                match kw.as_str() {
                    "left" => Ok(Float::Left),
                    "right" => Ok(Float::Right),
                    "none" => Ok(Float::None),
                    _ => Err(format!("invalid floating mode \"{}\"", kw)),
                }
            }
            _ => Err(format!("expected floating mode but found {}", v)),
        }
    }
}

impl TryFrom<&Value> for Display {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => {
                match kw.as_str() {
                    "inline" => Ok(Display::Inline),
                    "block" => Ok(Display::Block),
                    "none" => Ok(Display::None),
                    _ => Err(format!("invalid display mode \"{}\"", kw)),
                }
            }
            _ => Err(format!("expected display mode but found {}", v)),
        }
    }
}

/// Computed style values
#[derive(Clone, PartialEq, Debug)]
pub struct Style {
    // layout mode
    pub display: Display,
    pub float: Float,
    pub clear: Clear,

    // box colors
    pub background_color: Color,
    pub border_color: Color,

    // content dimensions (None ~ auto)
    pub width: Automatic<Pixels>,
    pub min_width: Automatic<Pixels>,
    pub max_width: Automatic<Pixels>,
    pub height: Automatic<Pixels>,
    pub min_height: Automatic<Pixels>,
    pub max_height: Automatic<Pixels>,

    // content edge in pixels (None ~ auto)
    //pub content: Edge<Automatic<f32>>,

    // margin edge in pixels (None ~ auto)
    pub margin: Edge<Automatic<Pixels>>,

    // padding edge in pixels
    pub padding: Edge<Pixels>,

    // border edge in pixels
    pub border: Edge<Pixels>,
}

impl Default for Style {
    fn default() -> Self {
        Style {
            display: Display::default(),
            float: Float::default(),
            clear: Clear::default(),

            background_color: Color::default(),
            border_color: Color::default(),

            width: Automatic::Auto,
            min_width: Automatic::Auto,
            max_width: Automatic::Auto,
            height: Automatic::Auto,
            min_height: Automatic::Auto,
            max_height: Automatic::Auto,

            margin: Edge {
                left: Automatic::Given(0.0),
                right: Automatic::Given(0.0),
                top: Automatic::Given(0.0),
                bottom: Automatic::Given(0.0),
            },

            padding: Edge::default(),

            border: Edge::default(),
        }
    }
}

/// Apply a stylesheet to an entire DOM tree, returning a StyledNode tree.
///
/// This finds only the specified values at the moment. Eventually it should be extended to find the
/// computed values too, including inherited values.
pub fn style_tree<'a>(root: &'a Node, stylesheet: &'a Stylesheet) -> StyledNode<'a> {
    StyledNode {
        node: root,
        specified: match root.node_type {
            NodeType::Element(ref elem) => specified_values(elem, stylesheet),
            NodeType::Text(_) => Style::default(),
        },
        children: root.children.iter().map(|child| style_tree(child, stylesheet)).collect(),
    }
}

/// Apply styles to a single element, returning the specified styles.
///
/// To do: Allow multiple UA/author/user stylesheets, and implement the cascade.
fn specified_values(elem: &ElementData, stylesheet: &Stylesheet) -> Style {
    let mut style = Style::default();
    let mut rules = matching_rules(elem, stylesheet);

    // Go through the rules from lowest to highest specificity.
    rules.sort_by(|&(a, _), &(b, _)| a.cmp(&b));
    for (_, rule) in rules {
        for declaration in &rule.declarations {
            let property = declaration.name.as_str();
            let value = &declaration.value;
            match property {
                "display" => { style.display = value.try_into().expect(property); },
                "float" => { style.float = value.try_into().expect(property); },
                "clear" => { style.clear = value.try_into().expect(property); },

                "width" => { style.width = value.try_into().expect(property); },
                "min-width" => { style.min_width = value.try_into().expect(property); },
                "max-width" => { style.max_width = value.try_into().expect(property); },
                "height" => { style.height = value.try_into().expect(property); },
                "min-height" => { style.min_height = value.try_into().expect(property); },
                "max-height" => { style.max_height = value.try_into().expect(property); },

                "background-color" => { style.background_color = value.try_into().expect(property); },
                "border-color" => { style.border_color = value.try_into().expect(property); },

                "margin-left" => { style.margin.left = value.try_into().expect(property); },
                "margin-right" => { style.margin.right = value.try_into().expect(property); },
                "margin-top" => { style.margin.top = value.try_into().expect(property); },
                "margin-bottom" => { style.margin.bottom = value.try_into().expect(property); },
                "margin" => {
                    let specified = value.try_into().expect(property);
                    style.margin.left = specified;
                    style.margin.right = specified;
                    style.margin.top = specified;
                    style.margin.bottom = specified;
                },

                "padding-left" => { style.padding.left = value.try_into().expect(property); },
                "padding-right" => { style.padding.right = value.try_into().expect(property); },
                "padding-top" => { style.padding.top = value.try_into().expect(property); },
                "padding-bottom" => { style.padding.bottom = value.try_into().expect(property); },
                "padding" => {
                    let specified = value.try_into().expect(property);
                    style.padding.left = specified;
                    style.padding.right = specified;
                    style.padding.top = specified;
                    style.padding.bottom = specified;
                },

                "border-left-width" => { style.border.left = value.try_into().expect(property); },
                "border-right-width" => { style.border.right = value.try_into().expect(property); },
                "border-top-width" => { style.border.top = value.try_into().expect(property); },
                "border-bottom-width" => { style.border.bottom = value.try_into().expect(property); },
                "border-width" => {
                    let specified = value.try_into().expect(property);
                    style.border.left = specified;
                    style.border.right = specified;
                    style.border.top = specified;
                    style.border.bottom = specified;
                },

                _ => { /* XXX: Ignore any unsupported styling property! */ }
            }
        }
    }
    style.display = style.display.floated(style.float);
    style
}

/// A single CSS rule and the specificity of its most specific matching selector.
type MatchedRule<'a> = (Specificity, &'a Rule);

/// Find all CSS rules that match the given element.
fn matching_rules<'a>(elem: &ElementData, stylesheet: &'a Stylesheet) -> Vec<MatchedRule<'a>> {
    // For now, we just do a linear scan of all the rules.  For large
    // documents, it would be more efficient to store the rules in hash tables
    // based on tag name, id, class, etc.
    stylesheet.rules.iter().filter_map(|rule| match_rule(elem, rule)).collect()
}

/// If `rule` matches `elem`, return a `MatchedRule`. Otherwise return `None`.
fn match_rule<'a>(elem: &ElementData, rule: &'a Rule) -> Option<MatchedRule<'a>> {
    // Find the first (most specific) matching selector.
    rule.selectors.iter().find(|selector| matches(elem, *selector))
        .map(|selector| (selector.specificity(), rule))
}

/// Selector matching:
fn matches(elem: &ElementData, selector: &Selector) -> bool {
    match *selector {
        Selector::Simple(ref simple_selector) => matches_simple_selector(elem, simple_selector)
    }
}

fn matches_simple_selector(elem: &ElementData, selector: &SimpleSelector) -> bool {
    // Check type selector
    if selector.tag.iter().any(|name| elem.tag != *name) {
        return false
    }

    // Check ID selector
    if selector.id.iter().any(|id| elem.id() != Some(id)) {
        return false;
    }

    // Check class selectors
    let elem_classes = elem.classes();
    if selector.class.iter().any(|class| !elem_classes.contains(&**class)) {
        return false;
    }

    // We didn't find any non-matching selector components.
    true
}
