#![allow(unused_variables)]
#![allow(dead_code)]

///! Basic CSS block layout.
///
/// N.B.: The version of this file kept under version control is meant as a
/// "safe" fallback/baseline, omitting more recent improvements to the CSS
/// attribute grammar. Please don't check in each new auto-generated version,
/// especially while still debugging.

use crate::dom::DocumentNode;
use crate::style::{StyledTree, StyledNode, Style, DisplayType, Floated, Positioned, Overflow};
use crate::paint::DisplayList;
use crate::utility::{Pixels, Edge, Rect, FloatCursor, MarginAccumulator};
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
    /// Upper accumulator for vertical margin.
    upper_margin: MarginAccumulator,
    /// Lower accumulator for vertical margin.
    lower_margin: MarginAccumulator,
    /// Logical size in the block (vertical) axis, excluding overflow+margin.
    block_size: Pixels,
    /// Logical size in the inline (horizontal) axis, excluding overflow+margin.
    inline_size: Pixels,
    /// Actual size in the block (vertical) axis, including overflow+margin.
    block_extent: Pixels,
    /// Actual size in the inline (horizontal) axis, including overflow+margin.
    inline_extent: Pixels,
    ///  Carried margin in the block axis
    carried_margin: Pixels,
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
        LayoutNode::from_style_node(style_node).pop().unwrap()
    }

    /// Construct a new layout node at the block level.
    ///
    /// With this constructor, the caller signals that the resulting layout
    /// node is immediately inside a block container. After reaching an inline
    /// container, recursive construction switches to `new_at_inline_level` to
    /// permit
    ///
    /// A style node with a display type of "none" is omitted.
    fn from_style_node(style_node: &'a StyledNode) -> Vec<Self> {
        let style = &style_node.specified;
        let class = match LayoutClass::of_style_node(style_node) {
            None => { return Vec::new(); },
            Some(class) => class
        };
        let generate = |child_nodes| LayoutNode {
            document_node: Some(style_node.node),
            style,
            class,
            children: child_nodes,
            layout: Layout::default()
        };

        let mut child_iter =
            style_node.children
                .iter()
                .flat_map(LayoutNode::from_style_node)
                .peekable();

        // An inline container distributes itself over contiguous runs of
        // inline-level boxes, to effectively break around any transitively
        // contained in-flow block-level boxes. If needed, an empty split
        // of the inline container is added to cap outer block-level boxes.

        let mut contents = Vec::new();
        if class.is_block_container() {
            while child_iter.peek().is_some() { // (BlockLevel* InlineLevel*)*
                // First, greedily consume block-level children.
                contents.extend(
                    child_iter.peeking_take_while(LayoutNode::is_block_level)
                );
                // Check for termination eagerly to avoid empty anonymous
                // wrappers.
                if child_iter.peek().is_none() {
                    break;
                }
                // Once exhausted, greedily consume inline-level children for
                // anonymous wrapping (including intervening floated boxes).
                contents.push(LayoutNode::into_inline_root(
                    style,
                    child_iter.peeking_take_while(LayoutNode::is_inline_level)
                ));
            }

            if class.is_floated() {
                contents = vec![LayoutNode::into_block_root(style, contents)];
            }

            vec![generate(contents)]
        } else /* class.is_inline_container() */ {
            loop { // InlineLevel* (BlockFlow+ InlineLevel*)*
                // First, greedily consume inline-level children.
                contents.push(generate(
                    child_iter.peeking_take_while(LayoutNode::is_inline_level).collect()
                ));
                // Check for termination only when we have inline boxes capping
                // both ends.
                if child_iter.peek().is_none() {
                    break;
                }
                // Once exhausted, consume in-flow block-level children for
                // anonymous wrapping (excluding intervening floated boxes).
                contents.push(LayoutNode::into_block_container(
                    style,
                    child_iter.peeking_take_while(LayoutNode::is_block_flow)
                ));
            }

            contents
        }
    }

    fn into_inline_root<I: IntoIterator<Item=Self>>(parent_style: &'a Style, iterable: I) -> Self {
        let wrapped_children = iterable.into_iter().collect_vec();
        assert!(wrapped_children.iter().all(LayoutNode::is_inline_level));

        LayoutNode::anon(LayoutClass::InlineRoot, parent_style, wrapped_children)
    }

    fn into_block_root<I: IntoIterator<Item=Self>>(parent_style: &'a Style, iterable: I) -> Self {
        let wrapped_children = iterable.into_iter().collect_vec();
        assert!(wrapped_children.iter().all(LayoutNode::is_block_level));

        LayoutNode::anon(LayoutClass::BlockRoot, parent_style, wrapped_children)
    }

    fn into_block_container<I: IntoIterator<Item=Self>>(parent_style: &'a Style, iterable: I) -> Self {
        let wrapped_children = iterable.into_iter().collect_vec();
        assert!(wrapped_children.iter().all(LayoutNode::is_block_level));

        LayoutNode::anon(LayoutClass::Block, parent_style, wrapped_children)
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

    fn is_block_level(&self) -> bool { self.class.is_block_level() }
    fn is_block_container(&self) -> bool { self.class.is_block_container() }
    fn is_block_root(&self) -> bool { self.class.is_block_root() }
    fn is_block_flow(&self) -> bool { self.is_block_level() && !self.is_floated() }

    fn is_inline_level(&self) -> bool { self.class.is_inline_level() }
    fn is_inline_container(&self) -> bool { self.class.is_inline_container() }
    fn is_inline_root(&self) -> bool { self.class.is_inline_root() }
    fn is_inline_flow(&self) -> bool { self.is_inline_level() && !self.is_floated() }

    fn is_floated(&self) -> bool { self.class.is_floated() }
    fn is_floated_left(&self) -> bool { self.is_floated() && self.style.float == Floated::Left }
    fn is_floated_right(&self) -> bool { self.is_floated() && self.style.float == Floated::Right }

    fn is_text_run(&self) -> bool { self.class.is_text_run() }

    fn is_positioned(&self) -> bool { self.style.position.is_positioned() }

    fn is_relative(&self) -> bool { self.style.position == Positioned::Relative }

    fn has_offsets(&self) -> bool { self.style.left.is_some() && self.style.right.is_some() && self.style.top.is_some() && self.style.bottom.is_some() }

    fn is_in_flow(&self) -> bool {
        use LayoutClass::*;
        use Positioned::*;

        match self.class {
            Text | Line | Inline | InlineRoot | InlineBlock | Block => true,
            Floated => false,
            BlockRoot => match self.style.position {
                Static | Relative => true,
                Absolute | Fixed | Sticky => false,
            },
        }
    }

    fn is_out_of_flow(&self) -> bool { !self.is_in_flow() }

    fn is_anon(&self) -> bool { self.document_node.is_none() }
}

impl<'a> fmt::Display for LayoutNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LayoutClass::*;

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
            Text =>
                format!("[TEXT {} :text \"{}\"]", geometry, text.unwrap()),
            Line =>
                String::from("[LINE]"),
            Inline | InlineRoot =>
                format!("[INLINE :elt {}]", elem.unwrap()),
            InlineBlock =>
                format!("[INLINE {} :elt {}]", geometry, elem.unwrap()),
            BlockRoot | Block | Floated =>
                format!("[BLOCK {} :elt {}]", geometry, elem.unwrap()),
        };

        f.write_str("(")?;
        f.write_str(&header)?;
        for child in self.children.iter() {
            write!(f, " {}", child)?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LayoutClass {
    Text,
    Line, // inline-level inline container
    Inline, // inline-level inline container
    InlineRoot, // block-level inline container
    InlineBlock, // inline-level block container
    Block, // block-level block container
    BlockRoot, // block-level block container
    Floated, // floated block container
}

impl LayoutClass {
    fn of_style_node(style_node: &StyledNode) -> Option<Self> {
        if style_node.as_text().is_some() {
            Some(LayoutClass::Text)
        } else if style_node.node.tag() == Some("html") {
            Some(LayoutClass::BlockRoot)
        } else {
            let style = &style_node.specified;
            if style.overflow != Overflow::Visible {
                Some(LayoutClass::BlockRoot)
            } else {
                match style.position {
                    Positioned::Absolute | Positioned::Fixed =>
                        Some(LayoutClass::BlockRoot),
                    Positioned::Sticky =>
                        unimplemented!("sticky positioning unsupported"),
                    Positioned::Relative | Positioned::Static => match style.float {
                        Floated::Left | Floated::Right =>
                            Some(LayoutClass::Floated),
                        Floated::None => match style.display {
                            DisplayType::Block => Some(LayoutClass::Block),
                            DisplayType::Inline => Some(LayoutClass::Inline),
                            DisplayType::InlineBlock => Some(LayoutClass::InlineBlock),
                            DisplayType::None => None,
                        },
                    },
                }
            }
        }
    }

    /// Is this class of node a block-level box?
    fn is_block_level(&self) -> bool {
        match self {
            LayoutClass::Block => true, // block box
            LayoutClass::BlockRoot => true, // block root box
            LayoutClass::InlineRoot => true, // inline root box
            LayoutClass::Floated => true, // floated box with block anchor
            _ => false
        }
    }

    /// Is this class of node serve an inline-level box?
    fn is_inline_level(&self) -> bool {
        match self {
            LayoutClass::Text => true, // text run (with position metadata)
            LayoutClass::Line => true, // line box (needed?)
            LayoutClass::Inline => true, // inline box
            LayoutClass::InlineBlock => true, // inline-level block container box
            LayoutClass::Floated => true, // floated box with inline anchor
            _ => false
        }
    }

    /// Is this class of node a block container box?
    ///
    /// Note that a floated box implicitly wraps its children in a `BlockRoot`.
    fn is_block_container(&self) -> bool {
        match self {
            LayoutClass::Block => true, // block box
            LayoutClass::BlockRoot => true, // block root box
            LayoutClass::Floated => true, // floated box with block anchor
            LayoutClass::InlineBlock => true, // inline-level block container box
            _ => false
        }
    }

    /// Is this class of node an inline container box?
    fn is_inline_container(&self) -> bool {
        match self {
            LayoutClass::Line => true, // line box (necessary?)
            LayoutClass::Inline => true, // inline box
            LayoutClass::InlineRoot => true, // inline root box
            _ => false
        }
    }

    /// Is this class of node a block root box?
    ///
    /// N.B.: A floated box implicitly wraps its children under a `BlockRoot`.
    fn is_block_root(&self) -> bool {
        match self {
            LayoutClass::BlockRoot => true, // block root box
            LayoutClass::Floated => true, // floated box with block anchor
            LayoutClass::InlineBlock => true, // inline-level block container box
            _ => false
        }
    }

    /// Is this class of node an inline root box?
    ///
    /// N.B.: An inline root box is always an anonymous block-level box.
    fn is_inline_root(&self) -> bool {
        match self {
            LayoutClass::InlineRoot => true, // inline box
            _ => false
        }
    }

    // Is this class of node a floated box?
    fn is_floated(&self) -> bool {
        match self {
            LayoutClass::Floated => true,
            _ => false
        }
    }

    // Is this class of node a text run?
    fn is_text_run(&self) -> bool {
        match self {
            LayoutClass::Text => true,
            _ => false
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
            LayoutClass::BlockRoot => self.layout_block(),
            LayoutClass::Block if !self.is_anon() => self.layout_block(),
            LayoutClass::Floated => self.layout_float(),
            _ => { },
        }
    }

    /// Lay out a block-level element and its descendants.
    fn layout_block(&mut self) {
        // Child width can depend on parent width, so we need to calculate this box's width before
        // laying out its children.
        self.calculate_block_width();

        self.layout.border.top = self.style.border.top;
        self.layout.border.bottom = self.style.border.bottom;

        self.layout.padding.top = self.style.padding.top;
        self.layout.padding.bottom = self.style.padding.bottom;

        self.layout.upper_margin.collapse = self.layout.padding.top == 0.0 && self.layout.border.top == 0.0;
        self.layout.lower_margin.collapse = self.layout.padding.bottom == 0.0 && self.layout.border.bottom == 0.0 && self.style.height.is_auto();

        // Finish calculating the block's edge sizes, and position it within its containing block.
        self.layout.margin.top = (self.style.margin.top.value() - self.layout.carried_margin).max(0.0); // auto ==> 0
        self.layout.margin.bottom = self.style.margin.bottom.value(); // auto ==> 0

        let mut clearance: f32 = 0.0;
        if self.style.clear.left {
            clearance = clearance.max(
                self.layout.float_cursor.clear_left(clearance)
            );
        }
        if self.style.clear.right {
            clearance = clearance.max(
                self.layout.float_cursor.clear_right(clearance)
            );
        }

        let padding = &self.layout.padding;
        let border = &self.layout.border;
        let margin = &self.layout.margin;

        // Position the box flush left (w.r.t. margin/border/padding) to the container.
        self.layout.content_box.x = self.layout.container.x;
        self.layout.content_box.x += padding.left + border.left + margin.left;

        // Position the box below all the previous boxes in the container.
        self.layout.content_box.y = self.layout.container.y + self.layout.container.height;
        self.layout.content_box.y += padding.top + border.top + margin.top;
        self.layout.content_box.y = self.layout.content_box.y.max(clearance);

        if self.is_positioned() {
            if let Some(x) = self.style.left {
                self.layout.content_box.x = x;
            }
            if let Some(y) = self.style.top {
                self.layout.content_box.y = y;
            }
        }

        if self.is_relative() {
            if let Some(dx) = self.style.left {
                self.layout.content_box.x += dx;
            }
            if let Some(dy) = self.style.top {
                self.layout.content_box.y += dy;
            }
        }

        // Recursively lay out the children of this box.
        let mut margin_carrier = self.layout.margin.top.max(self.layout.carried_margin);
        let mut block_cursor = 0.0; // block position for the next child box
        for child in &mut self.children {
            // Give the child box the boundaries of its container.
            child.layout.container.x = self.layout.content_box.x;
            child.layout.container.y = self.layout.content_box.y;
            child.layout.container.height = block_cursor;
            child.layout.container.width = self.layout.content_box.width;
            child.layout.float_cursor = self.layout.float_cursor.clone();
            // Carry in collapsible vertical margin.
            child.layout.carried_margin = margin_carrier;
            // Lay out the child box.
            child.layout();
            // Increment the cursor so each child is laid out below the previous one.
            if !child.is_positioned() {
                block_cursor += child.layout.block_size;
            }
            // Carry out collapsible vertical margin.
            margin_carrier = child.layout.carried_margin;

            if !child.is_positioned() {
                self.layout.block_extent = self.layout.block_extent.max(child.layout.block_extent);
            }
            self.layout.float_cursor = child.layout.float_cursor.clone();
        }
        self.layout.margin.bottom = (self.style.margin.bottom.value() - margin_carrier).max(0.0);
        if let Some(child) = self.children.first() {
            if child.is_in_flow() {
                self.layout.container.y += child.layout.margin.top;
                block_cursor -= child.layout.margin.top;
            }
        }

        // Parent height can depend on child height, so `calculate_height` must be called after the
        // children are laid out.
        self.layout.content_box.height = if self.style.height.is_auto() {
            if self.is_block_root() {
                block_cursor.max(self.layout.block_extent - self.layout.content_box.y)
            } else {
                block_cursor
            }
        } else {
            self.style.height.value()
        };

        self.layout.padding_box = self.layout.content_box.extend_by(&self.layout.padding);
        self.layout.border_box = self.layout.padding_box.extend_by(&self.layout.border);
        self.layout.margin_box = self.layout.border_box.extend_by(&self.layout.margin);

        // XXX: Use border box or margin box?
        self.layout.block_size = if self.is_in_flow() {
            if self.layout.content_box.height == 0.0 {
                self.layout.margin.top.max(self.layout.margin.bottom)
            } else {
                self.layout.margin_box.height.max(0.0)
            }
        } else {
            0.0
        };
        self.layout.block_extent = self.layout.block_extent.max(
            self.layout.margin_box.y + self.layout.margin_box.height
        );
        self.layout.carried_margin = if self.is_in_flow() {
                margin_carrier.max(
                if self.layout.content_box.height == 0.0 {
                    self.layout.margin.bottom.max(self.layout.margin.top)
                } else {
                    self.layout.margin.bottom
                })
            } else {
                0.0
            };

        self.layout.float_cursor = match self.style.float {
            Floated::Left => Lazy::new(self.layout.float_cursor.insert_left(&self.layout.margin_box)),
            Floated::Right => Lazy::new(self.layout.float_cursor.insert_right(&self.layout.margin_box)),
            Floated::None => self.layout.float_cursor.clone(),
        };
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

    /// Lay out a floating element and its descendants.
    fn layout_float(&mut self) {
        // Child width can depend on parent width, so we need to calculate this box's width before
        // laying out its children.
        self.calculate_float_width();

        // Finish calculating the block's edge sizes, and position it within its containing block.
        self.layout.padding.top = self.style.padding.top;
        self.layout.padding.bottom = self.style.padding.bottom;

        self.layout.border.top = self.style.border.top;
        self.layout.border.bottom = self.style.border.bottom;

        self.layout.margin.top = self.style.margin.top.value(); // auto ==> 0
        self.layout.margin.bottom = self.style.margin.bottom.value(); // auto ==> 0

        let mut container = self.layout.container.clone();
        container.y += container.height;
        if self.style.clear.left {
            container.y += self.layout.float_cursor.clear_left(container.y);
        }
        if self.style.clear.right {
            container.y += self.layout.float_cursor.clear_right(container.y);
        }

        let padding = &self.layout.padding;
        let border = &self.layout.border;
        let margin = &self.layout.margin;

        let width =
            self.layout.content_box.width
            + padding.left + border.left + margin.left
            + padding.right + border.right + margin.right;
        let (inline, block) = if self.is_floated_left() {
            self.layout.float_cursor.place_left(&container, width)
        } else /* self.is_floated_right() */ {
            self.layout.float_cursor.place_right(&container, width)
        };

        let content_box = &mut self.layout.content_box;
        content_box.x = inline + padding.left + border.left + margin.left;
        content_box.y = block + padding.top + border.top + margin.top;

        // Recursively lay out the children of this box.
        let mut block_cursor = 0.0; // block position for the next child box
        for child in &mut self.children {
            // Give the child box the boundaries of its container.
            child.layout.container.x = self.layout.content_box.x;
            child.layout.container.y = self.layout.content_box.y;
            child.layout.container.height = block_cursor;
            child.layout.container.width = self.layout.content_box.width;
            child.layout.float_cursor = self.layout.float_cursor.clone();
            // Lay out the child box.
            child.layout();
            // Increment the cursor so each child is laid out below the previous one.
            block_cursor += child.layout.block_size;

            self.layout.block_extent = self.layout.block_extent.max(child.layout.block_extent);
            self.layout.float_cursor = child.layout.float_cursor.clone();
        }

        // Parent height can depend on child height, so `calculate_height` must be called after the
        // children are laid out.
        self.layout.content_box.height = if self.style.height.is_auto() {
            if self.is_block_root() {
                block_cursor.max(self.layout.block_extent)
            } else {
                block_cursor
            }
        } else {
            self.style.height.value()
        };

        self.layout.padding_box = self.layout.content_box.extend_by(&self.layout.padding);
        self.layout.border_box = self.layout.padding_box.extend_by(&self.layout.border);
        self.layout.margin_box = self.layout.border_box.extend_by(&self.layout.margin);

        // XXX: Use border box or margin box?
        self.layout.block_size = 0.0;
        self.layout.block_extent = self.layout.block_extent.max(
            if self.layout.border_box.height > 0.0 {
                self.layout.border_box.y + self.layout.border_box.height
            } else {
                0.0
            }
        );

        self.layout.float_cursor = match self.style.float {
            Floated::Left => Lazy::new(self.layout.float_cursor.insert_left(&self.layout.margin_box)),
            Floated::Right => Lazy::new(self.layout.float_cursor.insert_right(&self.layout.margin_box)),
            Floated::None => self.layout.float_cursor.clone(),
        };
    }

    /// Calculate the width of a floating element.
    ///
    /// http://www.w3.org/TR/CSS2/visudet.html#blockwidth
    ///
    /// Sets the horizontal margin/padding/border dimensions, and the `width`.
    fn calculate_float_width(&mut self) {
        // Adjust used values to balance this difference, by increasing the total width by exactly
        // `underflow` pixels.
        self.layout.underflow = 0.0;

        self.layout.padding.left = self.style.padding.left;
        self.layout.padding.right = self.style.padding.right;

        self.layout.border.left = self.style.border.left;
        self.layout.border.right = self.style.border.right;

        self.layout.margin.left = self.style.margin.left.value(); // auto ==> 0
        self.layout.margin.right = self.style.margin.right.value(); // auto ==> 0

        let available_width =
            self.layout.container.width
            - self.layout.padding.left
            - self.layout.padding.right
            - self.layout.border.left
            - self.layout.border.right
            - self.layout.margin.left
            - self.layout.margin.right;
        self.layout.content_box.width = if self.style.width.is_auto() {
            // min(max(preferred_minimum_width, available_width), preferred_width)
            available_width
        } else {
            self.style.width.value()
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
