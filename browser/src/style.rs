//! Code for applying CSS styles to the DOM.
//!
//! This is not very interesting at the moment.  It will get much more
//! complicated if I add support for compound selectors.

use crate::css::{
    self, Declaration, Rule, Selector, SimpleSelector, Specificity, Stylesheet, Unit, Value,
};
use crate::dom::{DocumentNode, DocumentTree, ElementData};
use crate::utility::{
    Automatic::{self, Auto, Given},
    Pixels, Color, Edge,
};
use std::convert::{TryFrom, TryInto};

/// Apply a stylesheet to an entire DOM tree, returning a StyledNode tree.
///
/// This finds only the specified values at the moment. Eventually it should be extended to find the
/// computed values too, including inherited values.
pub fn style_tree<'a>(
    document_tree: &'a DocumentTree,
    stylesheet: &'a Stylesheet,
) -> StyledTree<'a> {
    StyledTree::new(document_tree)
        .cascade(&css::user_agent())
        .cascade(stylesheet)
}

/// The full styled tree, with ownership of the composite styled nodes.
pub struct StyledTree<'a> {
    pub document_tree: &'a DocumentTree,
    pub style_root: StyledNode<'a>,
}

impl<'a> StyledTree<'a> {
    pub fn new(document_tree: &'a DocumentTree) -> StyledTree<'a> {
        let style_root = StyledNode::new(&document_tree.document_root);
        StyledTree { document_tree, style_root }
    }

    pub fn cascade(mut self, stylesheet: &Stylesheet) -> Self {
        self.style_root.cascade(stylesheet);
        self
    }
}

/// A node with associated style data.
pub struct StyledNode<'a> {
    pub node: &'a DocumentNode,
    pub specified: Style,
    pub children: Vec<StyledNode<'a>>,
}

impl<'a> StyledNode<'a> {
    /// Construct the style tree, initializing style properties to their
    /// CSS-defined defaults.
    pub fn new(document_node: &'a DocumentNode) -> StyledNode<'a> {
        StyledNode {
            node: document_node,
            specified: Style::initial(),
            children: document_node.children
                .iter()
                .map(StyledNode::new)
                .collect()
        }
    }

    /// Compute style properties throughout the style tree for a given stylesheet.
    pub fn cascade(&mut self, stylesheet: &Stylesheet) {
        let style = &mut self.specified;
        if let Some(elem) = self.node.as_elem() {
            let mut rules = matching_rules(elem, stylesheet);

            // Go through the rules from lowest to highest specificity.
            rules.sort_by(|&(a, _), &(b, _)| a.cmp(&b));
            for (_, rule) in rules {
                for declaration in &rule.declarations {
                    style.apply_declaration(declaration);
                }
            }
            style.display = style.display.floated(style.float);
        }
    }
}

/// Computed style values
#[derive(Clone, PartialEq, Debug)]
pub struct Style {
    // layout mode
    pub display: DisplayMode,
    pub position: Positioned,
    pub float: Floated,
    pub clear: Clearance,

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DisplayMode {
    Inline,
    Block,
    Float, // A virtual layout mode for `display: block` with `float: left|right`.
    None,
}

impl Default for DisplayMode {
    fn default() -> Self {
        DisplayMode::Inline
    }
}

impl DisplayMode {
    pub fn floated(self, floating: Floated) -> DisplayMode {
        if self == DisplayMode::None || floating == Floated::None {
            self
        } else {
            DisplayMode::Float
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Positioned {
    Static,
    Relative,
    Absolute,
    Fixed,
    Sticky,
}

impl Default for Positioned {
    fn default() -> Self {
        Positioned::Static
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Floated {
    Left,
    Right,
    None,
}

impl Default for Floated {
    fn default() -> Self {
        Floated::None
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Clearance {
    pub left: bool,
    pub right: bool,
}

impl Clearance {
    const NONE: Self = Clearance {
        left: false,
        right: false,
    };
    const LEFT: Self = Clearance {
        left: true,
        right: false,
    };
    const RIGHT: Self = Clearance {
        left: false,
        right: true,
    };
    const BOTH: Self = Clearance {
        left: true,
        right: true,
    };
}

impl Default for Clearance {
    fn default() -> Self {
        Clearance::NONE
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Size {
    // {width,height}
    Length(f32, Unit),
    Percent(f32),
    // border-box
    BorderBox,
    // content-box
    ContentBox,
    // auto
    Auto,
    // fill
    Fill,
    // max-content
    MaxContent,
    // min-content
    MinContent,
    // available
    Available,
    // fit-content
    FitContent,
}

impl Default for Size {
    fn default() -> Self {
        Size::Auto
    }
}

impl TryFrom<Size> for Automatic<Pixels> {
    type Error = Size;

    fn try_from(s: Size) -> Result<Self, Self::Error> {
        match s {
            Size::Auto => Ok(Auto),
            Size::Length(px, Unit::Px) => Ok(Given(px)),
            v => Err(v),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Bound {
    // {min,max}-{width,height}
    Length(f32, Unit),
    Percent(f32),
    // {auto,none} = the otherwise default behavior
    Auto,
    // max-content = intrinsic preferred width/height
    MaxContent,
    // min-content = intrinsic minimum width/height
    MinContent,
    // fill-available = containing block's available width/height
    FillAvailable,
    // fit-content = min(max-content, max(min-content, fill-available)) for min
    // or max-content for max
    FitContent,
}

impl Default for Bound {
    fn default() -> Self {
        Bound::Auto
    }
}

impl std::fmt::Display for Bound {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Bound::Length(px, Unit::Px) => write!(f, "{}px", px),
            Bound::Percent(pct) => write!(f, "{}%", pct),
            Bound::Auto => f.write_str("auto"),
            Bound::MaxContent => f.write_str("max-content"),
            Bound::MinContent => f.write_str("min-content"),
            Bound::FillAvailable => f.write_str("fill-available"),
            Bound::FitContent => f.write_str("fit-content"),
        }
    }
}

impl TryFrom<Bound> for Automatic<Pixels> {
    type Error = String;

    fn try_from(s: Bound) -> Result<Self, Self::Error> {
        match s {
            Bound::Auto => Ok(Auto),
            Bound::Length(px, Unit::Px) => Ok(Given(px)),
            v => Err(format!("recognized but unimplemented keyword `{}`", v)),
        }
    }
}

impl Style {
    /// Create a style record with all properties initialized per CSS (cf.,
    /// the corresponding "initial" semantics in CSS).
    pub fn initial() -> Style {
        Style {
            display: DisplayMode::default(),
            position: Positioned::default(),
            float: Floated::default(),
            clear: Clearance::default(),

            background_color: Color::default(),
            border_color: Color::default(),

            width: Automatic::try_from(Size::default()).unwrap(),
            min_width: Automatic::try_from(Bound::default()).unwrap(),
            max_width: Automatic::try_from(Bound::default()).unwrap(),

            height: Automatic::try_from(Size::default()).unwrap(),
            min_height: Automatic::try_from(Bound::default()).unwrap(),
            max_height: Automatic::try_from(Bound::default()).unwrap(),

            margin: Edge::zero(),
            padding: Edge::default(),
            border: Edge::default(),
        }
    }

    /// Create a style record with each inherited-by-default property copied
    /// from a parent style record, initializing the other (uninherited)
    /// properties anew, as in `initial()`.
    pub fn inherit(_: &Self) -> Style {
        // Apparently none of these CSS properties is inherited.
        Style::initial()
    }

    pub fn apply_declaration(&mut self, declaration: &Declaration) {
        let property = declaration.name.as_ref();
        let value = &declaration.value;
        match property {
            "display" => self.display = value.try_into().expect(property),
            "float" => self.float = value.try_into().expect(property),
            "clear" => self.clear = value.try_into().expect(property),

            "width" => self.width = value.try_into().expect(property),
            "min-width" => self.min_width = value.try_into().expect(property),
            "max-width" => self.max_width = value.try_into().expect(property),
            "height" => self.height = value.try_into().expect(property),
            "min-height" => self.min_height = value.try_into().expect(property),
            "max-height" => self.max_height = value.try_into().expect(property),

            "background-color" => self.background_color = value.try_into().expect(property),
            "border-color" => self.border_color = value.try_into().expect(property),

            "margin-left" => self.margin.left = value.try_into().expect(property),
            "margin-right" => self.margin.right = value.try_into().expect(property),
            "margin-top" => self.margin.top = value.try_into().expect(property),
            "margin-bottom" => self.margin.bottom = value.try_into().expect(property),
            "margin" => self.margin = Edge::new(value.try_into().expect(property)),

            "padding-left" => self.padding.left = value.try_into().expect(property),
            "padding-right" => self.padding.right = value.try_into().expect(property),
            "padding-top" => self.padding.top = value.try_into().expect(property),
            "padding-bottom" => self.padding.bottom = value.try_into().expect(property),
            "padding" => self.padding = Edge::new(value.try_into().expect(property)),

            "border-left-width" => self.border.left = value.try_into().expect(property),
            "border-right-width" => self.border.right = value.try_into().expect(property),
            "border-top-width" => self.border.top = value.try_into().expect(property),
            "border-bottom-width" => self.border.bottom = value.try_into().expect(property),
            "border-width" => self.border = Edge::new(value.try_into().expect(property)),

            _ => (), // XXX: Ignore any unsupported styling property!
        }
    }
}

/// A single CSS rule and the specificity of its most specific matching selector.
type MatchedRule<'a> = (Specificity, &'a Rule);

/// Find all CSS rules that match the given element.
fn matching_rules<'a>(elem: &ElementData, stylesheet: &'a Stylesheet) -> Vec<MatchedRule<'a>> {
    // For now, we just do a linear scan of all the rules.  For large
    // documents, it would be more efficient to store the rules in hash tables
    // based on tag name, id, class, etc.
    stylesheet
        .rules
        .iter()
        .filter_map(|rule| match_rule(elem, rule))
        .collect()
}

/// If `rule` matches `elem`, return a `MatchedRule`. Otherwise return `None`.
fn match_rule<'a>(elem: &ElementData, rule: &'a Rule) -> Option<MatchedRule<'a>> {
    // Find the first (most specific) matching selector.
    rule.selectors
        .iter()
        .find(|selector| matches(elem, *selector))
        .map(|selector| (selector.specificity(), rule))
}

/// Selector matching:
fn matches(elem: &ElementData, selector: &Selector) -> bool {
    match *selector {
        Selector::Simple(ref simple_selector) => matches_simple_selector(elem, simple_selector),
    }
}

fn matches_simple_selector(elem: &ElementData, selector: &SimpleSelector) -> bool {
    // Check type selector
    if selector.tag.iter().any(|name| elem.tag != *name) {
        return false;
    }

    // Check ID selector
    if selector.id.iter().any(|id| elem.id() != Some(id)) {
        return false;
    }

    // Check class selectors
    let elem_classes = elem.classes();
    if selector
        .class
        .iter()
        .any(|class| !elem_classes.contains(&**class))
    {
        return false;
    }

    // We didn't find any non-matching selector components.
    true
}

impl TryFrom<&Value> for DisplayMode {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "inline" => Ok(DisplayMode::Inline),
                "block" => Ok(DisplayMode::Block),
                "none" => Ok(DisplayMode::None),
                _ => Err(format!("invalid display mode `{}`", kw)),
            },
            _ => Err(format!("expected display mode but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Positioned {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "static" => Ok(Positioned::Static),
                "relative" => Ok(Positioned::Relative),
                "absolute" => Ok(Positioned::Absolute),
                "fixed" => Ok(Positioned::Fixed),
                "sticky" => Ok(Positioned::Sticky),
                _ => Err(format!("invalid positioning mode `{}`", kw)),
            },
            _ => Err(format!("expected positioning mode but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Floated {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "left" => Ok(Floated::Left),
                "right" => Ok(Floated::Right),
                "none" => Ok(Floated::None),
                _ => Err(format!("invalid floating mode `{}`", kw)),
            },
            _ => Err(format!("expected floating mode but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Size {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "border-box" => Ok(Size::BorderBox),
                "content-box" => Ok(Size::ContentBox),
                "auto" => Ok(Size::Auto),
                "fill" => Ok(Size::Fill),
                "max-content" => Ok(Size::MaxContent),
                "min-content" => Ok(Size::MinContent),
                "available" => Ok(Size::Available),
                "fit-content" => Ok(Size::FitContent),
                _ => Err(format!("invalid dimension size `{}`", kw)),
            },
            Value::Length(px, Unit::Px) => Ok(Size::Length(*px, Unit::Px)),
            _ => Err(format!("expected dimension size but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Bound {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "auto" | "none" => Ok(Bound::Auto),
                "fill-available" => Ok(Bound::FillAvailable),
                "max-content" => Ok(Bound::MaxContent),
                "min-content" => Ok(Bound::MinContent),
                "fit-content" => Ok(Bound::FitContent),
                _ => Err(format!("invalid dimension bound `{}`", kw)),
            },
            Value::Length(px, Unit::Px) => Ok(Bound::Length(*px, Unit::Px)),
            _ => Err(format!("expected dimension bound but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Clearance {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Keyword(kw) => match kw.as_str() {
                "left" => Ok(Clearance::LEFT),
                "right" => Ok(Clearance::RIGHT),
                "both" => Ok(Clearance::BOTH),
                "none" => Ok(Clearance::NONE),
                _ => Err(format!("invalid floating mode `{}`", kw)),
            },
            _ => Err(format!("expected floating mode but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Automatic<Pixels> {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Length(px, Unit::Px) => Ok(Given(*px)),
            Value::Keyword(kw) if kw == "auto" => Ok(Auto),
            _ => Err(format!("expected auto/length but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Pixels {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Length(l, Unit::Px) => Ok(*l),
            _ => Err(format!("expected auto/length but found `{}`", v)),
        }
    }
}

impl TryFrom<&Value> for Color {
    type Error = String;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::ColorValue(v) => Ok(*v),
            Value::Keyword(kw) => {
                Color::by_css_name(kw).ok_or_else(|| format!("invalid color `{}`", kw))
            }
            _ => Err(format!("expected color but found `{}`", v)),
        }
    }
}
