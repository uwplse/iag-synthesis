#![allow(unused_variables)]
#![allow(dead_code)]

///! Basic CSS block layout.
///
/// N.B.: The version of this file kept under version control is meant as a
/// "safe" fallback/baseline, omitting more recent improvements to the CSS
/// attribute grammar. Please don't check in each new auto-generated version,
/// especially while still debugging.

use crate::dom::DocumentNode;
use crate::style::{StyledTree, StyledNode, Style, DisplayMode};
use crate::paint::DisplayList;
use crate::utility::{Pixels, Edge, Rect, FloatCursor};
use crate::lazy::Lazy;
use std::fmt;
use itertools::Itertools;

const CASSIUS_LAYOUT_NAME: &str = "doc-2";

/// Construct the layout tree around to the style tree, returning it with all
/// layout constraints solved.
pub fn layout_tree<'a>(style_tree: &'a StyledTree<'a>, parameters: Parameters) -> LayoutTree<'a> {
    let mut layout_tree = LayoutTree::new(style_tree, parameters);
    layout_tree.layout();
    layout_tree
}

/// Fold the layout tree into a display list to render.
pub fn display_list(layout_tree: &LayoutTree) -> DisplayList {
    layout_tree.render()
}

/// Output parameters.
#[derive(Clone, Copy, Debug)]
pub struct Parameters {
    pub viewport_width: usize,
    pub viewport_height: usize,
    pub scrollbar_width: usize,
    pub font_size: usize,
}

/// The full layout tree, with ownership of the composite layout nodes.
pub struct LayoutTree<'a> {
    style_tree: &'a StyledTree<'a>,
    parameters: Parameters,
    layout_root: LayoutNode<'a>
}

impl<'a> LayoutTree<'a> {
    fn new(style_tree: &'a StyledTree<'a>, parameters: Parameters) -> Self {
        LayoutTree {
            style_tree,
            parameters,
            layout_root: LayoutNode::new(&style_tree.style_root)
        }
    }
}

impl<'a> fmt::Display for LayoutTree<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let head = format!(
            "({} :matched true :w {} :h {} :fs {} :scrollw {})",
            CASSIUS_LAYOUT_NAME,
            self.parameters.viewport_width, self.parameters.viewport_height,
            self.parameters.font_size, self.parameters.scrollbar_width
        );
        let body = format!(
            "([VIEW :w {}] {})",
            self.parameters.viewport_width,
            self.layout_root
        );
        write!(f, "(define-layout {} {})", head, body)
    }
}

/// Packaged layout data for each box.
#[derive(Clone, Default, Debug)]
pub struct Layout {
    /// Position and size of the container box (from the containing block).
    container: Rect<Pixels>,
    /// Position and size of the content box relative to the document origin.
    content_box: Rect<Pixels>,
    /// Position and size of the padding box relative to the document origin.
    padding_box: Rect<Pixels>,
    /// Position and size of the border box relative to the document origin.
    border_box: Rect<Pixels>,
    /// Position and size of the margin box relative to the document origin.
    margin_box: Rect<Pixels>,
    /// Edges of the padding box.
    padding: Edge<Pixels>,
    /// Edges of the border box.
    border: Edge<Pixels>,
    /// Edges of the margin box.
    margin: Edge<Pixels>,
    /// Excess (or missing) horizontal space.
    underflow: Pixels,
    /// Cumulative positioning state for all predecessor floats.
    float_cursor: Lazy<FloatCursor>,
}

/// A node in the layout tree.
pub struct LayoutNode<'a> {
    document_node: Option<&'a DocumentNode>,
    /// Specified values from styling.
    style: &'a Style,
    /// Layout state for this node.
    layout: Layout,
    /// Fundamental layout mode (e.g., block, inline, float, absolute, &c.).
    class: LayoutClass,
    /// Zero or more descendant (child) boxes.
    children: Vec<LayoutNode<'a>>,
}

impl<'a> LayoutNode<'a> {
    fn new(style_node: &'a StyledNode) -> Self {
        let style = &style_node.specified;
        let class = LayoutClass::from(style_node.specified.display);
        let mut children = Vec::new();

        // Group children by class into internally homogeneous segments, in
        // order to wrap any with incompatible class inside an anonymous box.
        let segmentation =
            style_node.children.iter()
                .map(LayoutNode::new)
                .group_by(|child| child.class);
        for (segment_class, segment) in segmentation.into_iter() {
            if let Some(wrapper_class) = class.has_wrapper_for(segment_class) {
                children.push(LayoutNode::anon(wrapper_class, style, segment.collect()));
            } else {
                children.extend(segment);
            }
        }
        LayoutNode {
            document_node: Some(&style_node.node),
            style, layout: Layout::default(), class, children
        }
    }

    /// Create an anonymous layout node wrapping a segment of nodes.
    fn anon(wrapper_class: LayoutClass, parent_style: &'a Style, wrapped_nodes: Vec<Self>) -> Self {
        LayoutNode {
            document_node: None,
            style: Box::leak(Box::new(Style::inherit(parent_style))),
            layout: Layout::default(),
            class: wrapper_class,
            children: wrapped_nodes
        }
    }

    fn is_none(&self) -> bool {
        self.class == LayoutClass::None
    }

    fn is_anon(&self) -> bool {
        self.document_node.is_none()
    }
}

impl<'a> fmt::Display for LayoutNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let geometry = format!(
            ":x {} :y {} :w {} :h {}",
            self.layout.content_box.x,
            self.layout.content_box.y,
            self.layout.content_box.width,
            self.layout.content_box.height
        );
        let elem = self.document_node.map(|doc_node| doc_node.index);
        let text = self.document_node.and_then(DocumentNode::as_text);
        let header = match self.class {
            _ if self.is_anon() =>
                String::from("[ANON]"),
            LayoutClass::None =>
                String::from("[NONE]"),
            LayoutClass::Text =>
                format!("[TEXT {} :text \"{}\"]", geometry, text.unwrap()),
            LayoutClass::Line =>
                String::from("[LINE]"),
            LayoutClass::Inline =>
                format!("[INLINE :elt {}]", elem.unwrap()),
            LayoutClass::Float | LayoutClass::Block =>
                format!("[BLOCK {} :elt {}]", geometry, elem.unwrap()),
        };

        let (open, close) = if self.is_none() { ("#|(", ")|#") } else { ("(", ")") };
        f.write_str(open)?;
        f.write_str(&header)?;
        for child in self.children.iter() {
            write!(f, " {}", child)?;
        }
        f.write_str(close)?;
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LayoutClass {
    None,
    Text,
    Line,
    Inline,
    Block,
    Float,
}

impl LayoutClass {
    fn has_wrapper_for(self, child: Self) -> Option<Self> {
        use LayoutClass::{Block, Inline, Line, Text};
        match (self, child) {
            (Block, Inline) => Some(Block),
            (Block, Text) => Some(Inline),
            (Inline, Text) => Some(Line),
            (_, _) => None
        }
    }
}

impl From<DisplayMode> for LayoutClass {
    fn from(display: DisplayMode) -> Self {
        match display {
            DisplayMode::None => LayoutClass::None,
            DisplayMode::Inline => LayoutClass::Inline,
            DisplayMode::Block => LayoutClass::Block,
            DisplayMode::Float => LayoutClass::Float,
        }
    }
}

impl<'a> LayoutTree<'a> {
    fn layout(&mut self) {
        let width = self.parameters.viewport_width as Pixels;
        let height = self.parameters.viewport_height as Pixels;
        self.layout_root.layout.container.width = width;
        //self.layout_root.layout.container.height = height;
        self.layout_root.layout();
    }

    fn render(&self) -> DisplayList {
        let mut list = DisplayList::new();
        self.layout_root.render(&mut list);
        list
    }
}

impl<'a> LayoutNode<'a> {
    /// Lay out a box and its descendants.
    fn layout(&mut self) {
        match self.class {
            LayoutClass::Block if !self.is_anon() => self.layout_block(),
            LayoutClass::Float => self.layout_block(),
            _ => {},
        }
    }

    /// Lay out a block-level element and its descendants.
    fn layout_block(&mut self) {
        // Child width can depend on parent width, so we need to calculate this box's width before
        // laying out its children.
        self.calculate_block_width();

        // Finish calculating the block's edge sizes, and position it within its containing block.
        self.layout.margin.top = self.style.margin.top.value(); // auto ==> 0
        self.layout.margin.bottom = self.style.margin.bottom.value(); // auto ==> 0

        self.layout.border.top = self.style.border.top;
        self.layout.border.bottom = self.style.border.bottom;

        self.layout.padding.top = self.style.padding.top;
        self.layout.padding.bottom = self.style.padding.bottom;

        // Position the box flush left (w.r.t. margin/border/padding) to the container.
        self.layout.content_box.x = self.layout.container.x +
                             self.layout.margin.left + self.layout.border.left + self.layout.padding.left;

        // Position the box below all the previous boxes in the container.
        self.layout.content_box.y = self.layout.container.y + self.layout.container.height +
                             self.layout.margin.top + self.layout.border.top + self.layout.padding.top;

        // Recursively lay out the children of this box.
        let mut height = 0.0; // fold accumulator
        for child in &mut self.children {
            // Give the child box the boundaries of its container.
            child.layout.container.x = self.layout.content_box.x;
            child.layout.container.y = self.layout.content_box.y;
            child.layout.container.height = height;
            child.layout.container.width = self.layout.content_box.width;
            // Lay out the child box.
            child.layout();
            // Increment the height so each child is laid out below the previous one.
            height += child.layout.margin_box.height;
        }

        // Parent height can depend on child height, so `calculate_height` must be called after the
        // children are laid out.
        self.layout.content_box.height = if self.style.height.is_auto() {
            height
        } else {
            self.style.height.value()
        };

        self.layout.padding_box = self.layout.content_box.extend_by(&self.layout.padding);
        self.layout.border_box = self.layout.padding_box.extend_by(&self.layout.border);
        self.layout.margin_box = self.layout.border_box.extend_by(&self.layout.margin);
    }

    /// Calculate the width of a block-level non-replaced element in normal flow.
    ///
    /// http://www.w3.org/TR/CSS2/visudet.html#blockwidth
    ///
    /// Sets the horizontal margin/padding/border dimensions, and the `width`.
    fn calculate_block_width(&mut self) {
        // Adjust used values to balance this difference, by increasing the total width by exactly
        // `underflow` pixels.
        self.layout.underflow = self.layout.container.width - [
            self.style.margin.left.value(), self.style.margin.right.value(),
            self.style.border.left, self.style.border.right,
            self.style.padding.left, self.style.padding.right,
            self.style.width.value(),
        ].iter().sum::<f32>();

        self.layout.padding.left = self.style.padding.left;
        self.layout.padding.right = self.style.padding.right;

        self.layout.border.left = self.style.border.left;
        self.layout.border.right = self.style.border.right;

        self.layout.content_box.width = if self.style.width.is_auto() {
            self.layout.underflow.max(0.0)
        } else {
            self.style.width.value()
        };

        self.layout.margin.left = if self.style.margin.left.is_auto() {
            if self.style.width.is_auto() || self.layout.underflow < 0.0 {
                0.0
            } else if self.style.margin.right.is_auto() {
                self.layout.underflow / 2.0
            } else {
                self.layout.underflow
            }
        } else {
            self.style.margin.left.value()
        };

        self.layout.margin.right = if self.style.width.is_auto() && self.layout.underflow < 0.0 {
            self.style.margin.right.value() + self.layout.underflow
        } else if self.style.margin.right.is_auto() {
            if self.style.width.is_auto() {
                0.0
            } else if self.style.margin.left.is_auto() {
                self.layout.underflow / 2.0
            } else {
                self.layout.underflow
            }
        } else if !self.style.margin.left.is_auto() || !self.style.width.is_auto() {
            self.style.margin.right.value() + self.layout.underflow
        } else {
            self.style.margin.right.value()
        };
    }

    fn render(&self, list: &mut DisplayList) {
        let block = self.layout.border_box;
        let frame = self.layout.border_box.frame_by(&self.layout.border);
        list.display_block(self.style.background_color, block);
        list.display_frame(self.style.border_color, frame);
        for child in self.children.iter().rev() {
            child.render(list);
        }
    }
}
