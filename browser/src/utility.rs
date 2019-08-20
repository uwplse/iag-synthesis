use std::fmt;
use std::ops;

/// Trait for an additive (abelian) group of scalar values.
pub trait Additive: Copy + ops::Add<Self, Output = Self> + ops::Sub<Self, Output = Self> {}

impl<T: Copy + ops::Add<Self, Output = Self> + ops::Sub<Self, Output = Self>> Additive for T {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub fn rgb(red: u8, green: u8, blue: u8) -> Color {
        Color {
            r: red,
            g: green,
            b: blue,
            a: 0,
        }
    }

    pub fn rgba(red: u8, green: u8, blue: u8, alpha: u8) -> Color {
        Color {
            r: red,
            g: green,
            b: blue,
            a: alpha,
        }
    }

    pub fn by_css_name(name: &str) -> Option<Color> {
        let rgb = |r, g, b| Some(Color::rgb(r, g, b));
        let rgba = |r, g, b, a| Some(Color::rgba(r, g, b, a));
        match name {
            // Special colors
            "transparent" => rgba(0, 0, 0, 0),

            // VGA colors
            "black" => rgb(0x00, 0x00, 0x00),
            "silver" => rgb(0xc0, 0xc0, 0xc0),
            "gray" => rgb(0x80, 0x80, 0x80),
            "white" => rgb(0xff, 0xff, 0xff),
            "maroon" => rgb(0x80, 0x00, 0x00),
            "red" => rgb(0xff, 0x00, 0x00),
            "purple" => rgb(0x80, 0x00, 0x80),
            "fuchsia" => rgb(0xff, 0x00, 0xff),
            "green" => rgb(0x00, 0x80, 0x00),
            "lime" => rgb(0x00, 0xff, 0x00),
            "olive" => rgb(0x80, 0x80, 0x00),
            "yellow" => rgb(0xff, 0xff, 0x00),
            "navy" => rgb(0x00, 0x00, 0x80),
            "blue" => rgb(0x00, 0x00, 0xff),
            "teal" => rgb(0x00, 0x80, 0x80),
            "aqua" => rgb(0x00, 0xff, 0xff),

            // SVG colors
            "orange" => rgb(0xff, 0xa5, 0x00),
            "aliceblue" => rgb(0xf0, 0xf8, 0xff),
            "antiquewhite" => rgb(0xfa, 0xeb, 0xd7),
            "aquamarine" => rgb(0x7f, 0xff, 0xd4),
            "azure" => rgb(0xf0, 0xff, 0xff),
            "beige" => rgb(0xf5, 0xf5, 0xdc),
            "bisque" => rgb(0xff, 0xe4, 0xc4),
            "blanchedalmond" => rgb(0xff, 0xeb, 0xcd),
            "blueviolet" => rgb(0x8a, 0x2b, 0xe2),
            "brown" => rgb(0xa5, 0x2a, 0x2a),
            "burlywood" => rgb(0xde, 0xb8, 0x87),
            "cadetblue" => rgb(0x5f, 0x9e, 0xa0),
            "chartreuse" => rgb(0x7f, 0xff, 0x00),
            "chocolate" => rgb(0xd2, 0x69, 0x1e),
            "coral" => rgb(0xff, 0x7f, 0x50),
            "cornflowerblue" => rgb(0x64, 0x95, 0xed),
            "cornsilk" => rgb(0xff, 0xf8, 0xdc),
            "crimson" => rgb(0xdc, 0x14, 0x3c),
            "cyan" => rgb(0x00, 0xff, 0xff),
            "darkblue" => rgb(0x00, 0x00, 0x8b),
            "darkcyan" => rgb(0x00, 0x8b, 0x8b),
            "darkgoldenrod" => rgb(0xb8, 0x86, 0x0b),
            "darkgray" => rgb(0xa9, 0xa9, 0xa9),
            "darkgreen" => rgb(0x00, 0x64, 0x00),
            "darkgrey" => rgb(0xa9, 0xa9, 0xa9),
            "darkkhaki" => rgb(0xbd, 0xb7, 0x6b),
            "darkmagenta" => rgb(0x8b, 0x00, 0x8b),
            "darkolivegreen" => rgb(0x55, 0x6b, 0x2f),
            "darkorange" => rgb(0xff, 0x8c, 0x00),
            "darkorchid" => rgb(0x99, 0x32, 0xcc),
            "darkred" => rgb(0x8b, 0x00, 0x00),
            "darksalmon" => rgb(0xe9, 0x96, 0x7a),
            "darkseagreen" => rgb(0x8f, 0xbc, 0x8f),
            "darkslateblue" => rgb(0x48, 0x3d, 0x8b),
            "darkslategray" => rgb(0x2f, 0x4f, 0x4f),
            "darkslategrey" => rgb(0x2f, 0x4f, 0x4f),
            "darkturquoise" => rgb(0x00, 0xce, 0xd1),
            "darkviolet" => rgb(0x94, 0x00, 0xd3),
            "deeppink" => rgb(0xff, 0x14, 0x93),
            "deepskyblue" => rgb(0x00, 0xbf, 0xff),
            "dimgray" => rgb(0x69, 0x69, 0x69),
            "dimgrey" => rgb(0x69, 0x69, 0x69),
            "dodgerblue" => rgb(0x1e, 0x90, 0xff),
            "firebrick" => rgb(0xb2, 0x22, 0x22),
            "floralwhite" => rgb(0xff, 0xfa, 0xf0),
            "forestgreen" => rgb(0x22, 0x8b, 0x22),
            "gainsboro" => rgb(0xdc, 0xdc, 0xdc),
            "ghostwhite" => rgb(0xf8, 0xf8, 0xff),
            "gold" => rgb(0xff, 0xd7, 0x00),
            "goldenrod" => rgb(0xda, 0xa5, 0x20),
            "greenyellow" => rgb(0xad, 0xff, 0x2f),
            "grey" => rgb(0x80, 0x80, 0x80),
            "honeydew" => rgb(0xf0, 0xff, 0xf0),
            "hotpink" => rgb(0xff, 0x69, 0xb4),
            "indianred" => rgb(0xcd, 0x5c, 0x5c),
            "indigo" => rgb(0x4b, 0x00, 0x82),
            "ivory" => rgb(0xff, 0xff, 0xf0),
            "khaki" => rgb(0xf0, 0xe6, 0x8c),
            "lavender" => rgb(0xe6, 0xe6, 0xfa),
            "lavenderblush" => rgb(0xff, 0xf0, 0xf5),
            "lawngreen" => rgb(0x7c, 0xfc, 0x00),
            "lemonchiffon" => rgb(0xff, 0xfa, 0xcd),
            "lightblue" => rgb(0xad, 0xd8, 0xe6),
            "lightcoral" => rgb(0xf0, 0x80, 0x80),
            "lightcyan" => rgb(0xe0, 0xff, 0xff),
            "lightgoldenrodyellow" => rgb(0xfa, 0xfa, 0xd2),
            "lightgray" => rgb(0xd3, 0xd3, 0xd3),
            "lightgreen" => rgb(0x90, 0xee, 0x90),
            "lightgrey" => rgb(0xd3, 0xd3, 0xd3),
            "lightpink" => rgb(0xff, 0xb6, 0xc1),
            "lightsalmon" => rgb(0xff, 0xa0, 0x7a),
            "lightseagreen" => rgb(0x20, 0xb2, 0xaa),
            "lightskyblue" => rgb(0x87, 0xce, 0xfa),
            "lightslategray" => rgb(0x77, 0x88, 0x99),
            "lightslategrey" => rgb(0x77, 0x88, 0x99),
            "lightsteelblue" => rgb(0xb0, 0xc4, 0xde),
            "lightyellow" => rgb(0xff, 0xff, 0xe0),
            "limegreen" => rgb(0x32, 0xcd, 0x32),
            "linen" => rgb(0xfa, 0xf0, 0xe6),
            "magenta" => rgb(0xff, 0x00, 0xff),
            "mediumaquamarine" => rgb(0x66, 0xcd, 0xaa),
            "mediumblue" => rgb(0x00, 0x00, 0xcd),
            "mediumorchid" => rgb(0xba, 0x55, 0xd3),
            "mediumpurple" => rgb(0x93, 0x70, 0xdb),
            "mediumseagreen" => rgb(0x3c, 0xb3, 0x71),
            "mediumslateblue" => rgb(0x7b, 0x68, 0xee),
            "mediumspringgreen" => rgb(0x00, 0xfa, 0x9a),
            "mediumturquoise" => rgb(0x48, 0xd1, 0xcc),
            "mediumvioletred" => rgb(0xc7, 0x15, 0x85),
            "midnightblue" => rgb(0x19, 0x19, 0x70),
            "mintcream" => rgb(0xf5, 0xff, 0xfa),
            "mistyrose" => rgb(0xff, 0xe4, 0xe1),
            "moccasin" => rgb(0xff, 0xe4, 0xb5),
            "navajowhite" => rgb(0xff, 0xde, 0xad),
            "oldlace" => rgb(0xfd, 0xf5, 0xe6),
            "olivedrab" => rgb(0x6b, 0x8e, 0x23),
            "orangered" => rgb(0xff, 0x45, 0x00),
            "orchid" => rgb(0xda, 0x70, 0xd6),
            "palegoldenrod" => rgb(0xee, 0xe8, 0xaa),
            "palegreen" => rgb(0x98, 0xfb, 0x98),
            "paleturquoise" => rgb(0xaf, 0xee, 0xee),
            "palevioletred" => rgb(0xdb, 0x70, 0x93),
            "papayawhip" => rgb(0xff, 0xef, 0xd5),
            "peachpuff" => rgb(0xff, 0xda, 0xb9),
            "peru" => rgb(0xcd, 0x85, 0x3f),
            "pink" => rgb(0xff, 0xc0, 0xcb),
            "plum" => rgb(0xdd, 0xa0, 0xdd),
            "powderblue" => rgb(0xb0, 0xe0, 0xe6),
            "rosybrown" => rgb(0xbc, 0x8f, 0x8f),
            "royalblue" => rgb(0x41, 0x69, 0xe1),
            "saddlebrown" => rgb(0x8b, 0x45, 0x13),
            "salmon" => rgb(0xfa, 0x80, 0x72),
            "sandybrown" => rgb(0xf4, 0xa4, 0x60),
            "seagreen" => rgb(0x2e, 0x8b, 0x57),
            "seashell" => rgb(0xff, 0xf5, 0xee),
            "sienna" => rgb(0xa0, 0x52, 0x2d),
            "skyblue" => rgb(0x87, 0xce, 0xeb),
            "slateblue" => rgb(0x6a, 0x5a, 0xcd),
            "slategray" => rgb(0x70, 0x80, 0x90),
            "slategrey" => rgb(0x70, 0x80, 0x90),
            "snow" => rgb(0xff, 0xfa, 0xfa),
            "springgreen" => rgb(0x00, 0xff, 0x7f),
            "steelblue" => rgb(0x46, 0x82, 0xb4),
            "tan" => rgb(0xd2, 0xb4, 0x8c),
            "thistle" => rgb(0xd8, 0xbf, 0xd8),
            "tomato" => rgb(0xff, 0x63, 0x47),
            "turquoise" => rgb(0x40, 0xe0, 0xd0),
            "violet" => rgb(0xee, 0x82, 0xee),
            "wheat" => rgb(0xf5, 0xde, 0xb3),
            "whitesmoke" => rgb(0xf5, 0xf5, 0xf5),
            "yellowgreen" => rgb(0x9a, 0xcd, 0x32),

            _ => None,
        }
    }

    pub fn alpha(&self) -> f32 {
        self.a as f32 / 255.0
    }

    pub fn channels(&self) -> (f32, f32, f32) {
        let r = self.r as f32 / 255.0;
        let g = self.g as f32 / 255.0;
        let b = self.b as f32 / 255.0;
        (r, g, b)
    }

    pub fn to_rgb(&self) -> (u8, u8, u8) {
        let r = ((self.r as u16) * (self.a as u16) / 255) as u8;
        let g = ((self.g as u16) * (self.a as u16) / 255) as u8;
        let b = ((self.b as u16) * (self.a as u16) / 255) as u8;
        (r, g, b)
    }

    pub fn over(&self, below: &Self) -> Self {
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

impl Default for Color {
    fn default() -> Self {
        Color {
            r: 0,
            g: 0,
            b: 0,
            a: 0,
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "rgba({}, {}, {}, {:.1})",
            self.r,
            self.g,
            self.b,
            self.alpha()
        )
    }
}

/// A length or co-ordinate measured in literal pixels.
pub type Pixels = f32;

pub const MIN_PIXELS: Pixels = std::f32::MIN;
pub const MAX_PIXELS: Pixels = std::f32::MAX;

/// A rectangular co-ordinate point.
#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
}

/// A rectangular perimeter edge with distinct breadth on each side.
#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Edge<T> {
    pub left: T,
    pub right: T,
    pub top: T,
    pub bottom: T,
}

impl<T: Copy> Edge<T> {
    /// Create a new `Edge` with uniform breadth on all sides.
    pub fn new(breadth: T) -> Self {
        Edge {
            left: breadth,
            right: breadth,
            top: breadth,
            bottom: breadth,
        }
    }
}

impl<T: Additive> Edge<T> {
    pub fn transform<U, F: Fn(T) -> U>(&self, tr: F) -> Edge<U> {
        Edge {
            left: tr(self.left),
            right: tr(self.right),
            top: tr(self.top),
            bottom: tr(self.bottom),
        }
    }
}

impl Edge<Automatic<Pixels>> {
    pub fn auto() -> Self {
        Edge {
            left: Auto,
            right: Auto,
            top: Auto,
            bottom: Auto,
        }
    }

    pub fn zero() -> Self {
        let zero = Given(0.0);
        Edge {
            left: zero,
            right: zero,
            top: zero,
            bottom: zero,
        }
    }
}

/// Rectangle co-ordinates and dimensions.
#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct Rect<T> {
    pub x: T,
    pub y: T,
    pub width: T,
    pub height: T,
}

impl<T: Additive> Rect<T> {
    pub fn from_diagonal(origin: Point<T>, bound: Point<T>) -> Self {
        Rect {
            x: origin.x,
            y: origin.y,
            width: bound.x - origin.x,
            height: bound.y - origin.y
        }
    }

    pub fn transform<U, F: Fn(T) -> U>(&self, f: F) -> Rect<U> {
        Rect {
            x: f(self.x),
            y: f(self.y),
            width: f(self.width),
            height: f(self.height),
        }
    }

    pub fn extend_by(&self, edge: &Edge<T>) -> Self {
        Rect {
            x: self.x - edge.left,
            y: self.y - edge.top,
            width: self.width + edge.left + edge.right,
            height: self.height + edge.top + edge.bottom,
        }
    }

    pub fn frame_by(&self, edge: &Edge<T>) -> Edge<Rect<T>> {
        let pt = self.bound();
        Edge {
            left: Rect {
                width: edge.left,
                ..*self
            },
            right: Rect {
                width: edge.right,
                x: pt.x - edge.right,
                ..*self
            },
            top: Rect {
                height: edge.top,
                ..*self
            },
            bottom: Rect {
                height: edge.bottom,
                y: pt.y - edge.bottom,
                ..*self
            },
        }
    }

    pub fn origin(&self) -> Point<T> {
        Point {
            x: self.x,
            y: self.y,
        }
    }

    pub fn bound(&self) -> Point<T> {
        Point {
            x: self.x + self.width,
            y: self.y + self.height,
        }
    }

    pub fn to_diagonal(&self) -> (Point<T>, Point<T>) {
        (self.origin(), self.bound())
    }
}

impl Rect<f32> {
    pub fn from_dimensions(width: f32, height: f32) -> Self {
        Rect { x: 0.0, y: 0.0, width, height }
    }

    pub fn clip(&self, pt: Point<f32>) -> Point<f32> {
        Point {
            x: pt.x.max(self.x).min(self.width),
            y: pt.y.max(self.y).min(self.height),
        }
    }

    pub fn clip_rect(&self, rect: &Self) -> Self {
        let (origin, bound) = rect.to_diagonal();
        Rect::from_diagonal(self.clip(origin), self.clip(bound))
    }
}

/// A potentially automatically calculated length.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Automatic<V> {
    Auto,
    Given(V),
}
use Automatic::{Auto, Given};

impl<V: Default + Copy> Automatic<V> {
    /// Get the given value or the default for its type.
    pub fn value(&self) -> V {
        match self {
            Auto => V::default(),
            Given(v) => *v,
        }
    }

    /// Give a value to use if the wrapper is automatic.
    pub fn give(&self, v: V) -> Self {
        match self {
            Auto => Given(v),
            Given(_) => *self,
        }
    }

    /// Take the value from the wrapper, using the provided value if automatic.
    pub fn take(&self, v: V) -> V {
        match self {
            Auto => v,
            Given(v) => *v,
        }
    }

    /// Is the wrapper set to automatic?
    pub fn is_auto(&self) -> bool {
        match self {
            Auto => true,
            Given(_) => false,
        }
    }

    /// Is the wrapper set to a given value?
    pub fn is_given(&self) -> bool {
        match self {
            Auto => false,
            Given(_) => true,
        }
    }
}

impl<V> Default for Automatic<V> {
    fn default() -> Self {
        Auto
    }
}

impl<V> From<V> for Automatic<V> {
    fn from(v: V) -> Self {
        Given(v)
    }
}

#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub struct MarginAccumulator {
    pub positive: Pixels,
    pub negative: Pixels,
    pub collapse: bool,
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct FloatLevel {
    bottom: Pixels, // block_end
    left: Pixels,   // inline_start
    right: Pixels,  // inline_end
}

impl FloatLevel {
    fn left(content: &Rect<Pixels>) -> Self {
        FloatLevel {
            bottom: content.y + content.height,
            left: content.x + content.width,
            right: MAX_PIXELS,
        }
    }

    fn right(content: &Rect<Pixels>) -> Self {
        FloatLevel {
            bottom: content.y + content.height,
            left: 0.0,
            right: content.x,
        }
    }

    fn merge(&mut self, other: &FloatLevel) {
        // Must be same level.
        assert!(self.bottom == other.bottom);
        self.left = self.left.max(other.left);
        self.right = self.right.min(other.right);
    }

    fn order(&self, other: &FloatLevel) -> std::cmp::Ordering {
        // We assume that every `Pixels` value is a rational number.
        self.bottom.partial_cmp(&other.bottom).unwrap()
    }
}

#[derive(Clone, Default, PartialEq, Debug)]
pub struct FloatCursor {
    block: Pixels,
    inline: Vec<FloatLevel>,
}

impl FloatCursor {
    pub fn new() -> FloatCursor {
        FloatCursor {
            block: 0.0,
            inline: Vec::new(),
        }
    }

    fn advance(&self, block: Pixels, level: FloatLevel) -> FloatCursor {
        let mut inline: Vec<FloatLevel> = self
            .inline
            .iter()
            .skip_while(|l| l.bottom < block)
            .cloned()
            .collect();
        match inline.binary_search_by(|l| l.order(&level)) {
            Ok(i) => {
                inline[i].merge(&level);
            }
            Err(i) => {
                inline.insert(i, level);
            }
        }
        FloatCursor { block, inline }
    }

    pub fn insert_left(&self, content: &Rect<Pixels>) -> FloatCursor {
        if content.height > 0.0 {
            self.advance(content.y, FloatLevel::left(&content))
        } else {
            self.clone()
        }
    }

    pub fn insert_right(&self, content: &Rect<Pixels>) -> FloatCursor {
        if content.height > 0.0 {
            self.advance(content.y, FloatLevel::right(&content))
        } else {
            self.clone()
        }
    }

    pub fn place_left(&self, container: &Rect<Pixels>, width: Pixels) -> (Pixels, Pixels) {
        let mut y = self.block.max(container.y);
        let x = container.x;
        for level in &self.inline {
            // Is the proposed left-floating anchor above this layer of
            // floated boxes? If so, it doesn't tell us anything.
            if level.bottom < y {
                continue;
            }
            // Would this layer of floated boxes force the proposed
            // left-floating anchor to shift excessively, thus overflowing
            // it's container? If so, then we must do better.
            if level.left - x > container.width - width {
                y = level.bottom; // descend below this layer
                continue;
            }
            let x = level.left.max(x);
            // Does this layer of floated boxes have enough interior
            // width available to use the proposed left-floating anchor?
            // If so, then we must do better.
            if level.right - x < width {
                y = level.bottom; // descend below this layer
                continue;
            }
            return (x, y);
        }

        // At this point, we've descended beyond the existing layers of
        // floated boxes.
        (x, y)
    }

    pub fn place_right(&self, container: &Rect<Pixels>, width: Pixels) -> (Pixels, Pixels) {
        let mut y = self.block.max(container.y);
        let x = container.x + container.width;
        for level in &self.inline {
            // Is the proposed right-floating anchor above this layer of
            // floated boxes? If so, it doesn't tell us anything.
            if level.bottom < y {
                continue;
            }
            // Would this layer of floated boxes force the proposed
            // right-floating anchor to shift excessively, thus overflowing
            // it's container? If so, then we must do better.
            if x - level.right > container.width - width {
                y = level.bottom; // descend below this layer
                continue;
            }
            let x = level.right.min(x);
            // Does this layer of floated boxes have enough interior
            // width available to use the proposed right-floating anchor?
            // If so, then we must do better.
            if x - level.left < width {
                y = level.bottom; // descend below this layer
                continue;
            }
            return (x - width, y);
        }

        // At this point, we've descended beyond the existing layers of
        // floated boxes.
        (x - width, y)
    }

    pub fn clear_left(&self, block: Pixels) -> Pixels {
        self.inline.last().map_or(self.block, |l| l.bottom).max(block) - block
    }

    pub fn clear_right(&self, block: Pixels) -> Pixels {
        self.inline.last().map_or(self.block, |l| l.bottom).max(block) - block
    }
}

/*
#[derive(Clone, Debug)]
struct Pen {
    char_count: u32, // number of characters/atoms, including the current line
    line_count: u32, // number of lines, including the current line
    current: LineData, // active, continued line box
    previous: Vec<(u32, LineData)>, // inactive, completed line boxes
    floating_impact: FloatImpact, // e.g., left/right width used
    floating_context: FloatContext, // potentially updated
}
*/

/*
pub enum Inline {
    TextRun, InlineFlow, BlockRoot, FloatedBox, Replaced,
}

pub enum Block {
    InlineRoot, BlockFlow, BlockRoot, Floated, Replaced
}
*/
