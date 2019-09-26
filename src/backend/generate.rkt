#lang rosette

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../grammar/expression.rkt"
         "intermediate.rkt")

(provide generate-program)

; -----------------------
; Standard program header
; -----------------------

(define directives
  (list '(hash-bang allow (unused_parens))
        '(hash-bang allow (unused_variables))
        '(hash-bang allow (dead_code))
        '(blank)))

(define imports
  (list '(use crate dom DocumentNode)
        '(use crate style (StyledTree StyledNode Style DisplayType Floated Positioned))
        '(use crate paint DisplayList)
        '(use crate utility (Pixels Edge Rect FloatCursor))
        '(use crate utility (MarginAccumulator FloatCursor))
        '(use crate lazy Lazy)
        '(use itertools Itertools)))

(define struct-Layout
  (list '(blank)
        '(hash derive (Clone Default PartialEq Debug))
        '(struct Layout ()
           (record (: container (gen Rect (Pixels)))
                   (: content_box (gen Rect (Pixels)))
                   (: padding_box (gen Rect (Pixels)))
                   (: border_box (gen Rect (Pixels)))
                   (: margin_box (gen Rect (Pixels)))
                   (: flow_height Pixels)
                   (: margin_clear bool)
                   (: margin_weird bool)
                   (: margin_above MarginAccumulator)
                   (: margin_below MarginAccumulator)
                   (: padding (gen Edge (Pixels)))
                   (: border (gen Edge (Pixels)))
                   (: margin (gen Edge (Pixels)))
                   (: float_cursor (gen Lazy (FloatCursor)))
                   (: underflow Pixels)))))

(define struct-LayoutBox
  (list '(blank)
        '(struct LayoutBox ((life a))
           (record (: element (gen Option ((ref a DocumentNode))))
                   (: class LayoutClass)
                   (: layout Layout)
                   (: style (ref a Style))
                   (: children (gen Vec ((gen LayoutNode ((life a))))))))))

(define impl-LayoutBox-new
  (list '(blank)
        '(impl ((life a)) LayoutBox
               (fn new () ((: box_type BoxType) (: style (ref a Style))) Self
                   (do (return (struct LayoutBox
                                 ((: element None)
                                  (: container (call (:: Rect default) ()))
                                  (: content_box (call (:: Rect default) ()))
                                  (: padding_box (call (:: Rect default) ()))
                                  (: border_box (call (:: Rect default) ()))
                                  (: margin_box (call (:: Rect default) ()))
                                  (: computedHeight 0.0)
                                  (: margin_acc (call (:: MarginAccumulator default) ()))
                                  (: floatLstIn (call (:: FloatList empty) ()))
                                  (: floatLstOut (call (:: FloatList empty) ()))
                                  (: padding (call (:: Edge default) ()))
                                  (: border (call (:: Edge default) ()))
                                  (: margin (call (:: Edge default) ()))
                                  (: underflow 0.0)
                                  (: style style)
                                  (: anonymous #t)
                                  (: class box_type)
                                  (: children (call (:: Vec new) ()))))))))))

(define impl-Display-for-LayoutBox
  (list '(blank)
        '(impl ((life a)) (for (:: fmt Display) LayoutBox)
               (fn fmt () ((: self (ref Self)) (: f (ref (mut (:: fmt Formatter))))) (:: fmt Result)
                          (do (if (== (select self class) (:: BoxType None))
                                  (do (? (call write! (f "#|")))))
                              (? (call write! (f "(")))
                              (match (select self element)
                                (=> (constructor None (unit))
                                    (do (? (call write! (f "[ANON]")))))
                                (=> (constructor Some (tuple id))
                                    (do (let kind
                                             (match (select self class)
                                                (=> (constructor (:: BoxType None) (unit)) "NONE")
                                                (=> (constructor (:: BoxType Block) (unit)) "BLOCK")
                                                (=> (constructor (:: BoxType Float) (unit)) "BLOCK")
                                                (=> (constructor (:: BoxType Inline) (unit)) "INLINE")))
                                        (let x (select (select self content_box) x))
                                        (let y (select (select self content_box) y))
                                        (let w (select (select self content_box) width))
                                        (let h (select (select self content_box) height))
                                        (? (call write! (f "[{} :x {} :y {} :w {} :h {} :elt {}]" kind x y w h id))))))
                              (for child (call (select (select self children) iter) ())
                                (do (? (call write! (f " {}" child)))))
                              (? (call write! (f ")")))
                              (if (== (select self class) (:: BoxType None))
                                  (do (? (call write! (f "|#")))))
                              (return (call (:: Result Ok) ((unit)))))))))

(define fn-layout_tree
  (list '(blank)
        '(pub (fn layout_tree ((life a)) ((: node (ref a (gen StyledNode ((life a)))))
                                          (: width usize)
                                          (: height usize))
                  (gen LayoutBox ((life a)))
                  (do (let-mut root_box (call build_layout_tree (node)))
                      (:= (select (select root_box container) width) (as width Pixels))
                    (call (select root_box layout) ())
                    (call println! ("(define-layout (doc-2 :matched true :w {} :h {} :fs 16 :scrollw 0) ([VIEW :w {}] {}))"
                                    width height width root_box))
                    (return root_box))))))

(define fn-build_layout_tree
  (list '(blank)
        '(fn build_layout_tree ((life a)) ((: style_node (ref a (gen StyledNode ((life a))))))
             (gen LayoutBox ((life a)))
             (do (let box_type (match (select (select style_node specified) display)
                                 (=> (constructor (:: Display Inline) (unit)) (:: BoxType Inline))
                                 (=> (constructor (:: Display Block) (unit)) (:: BoxType Block))
                                 (=> (constructor (:: Display Float) (unit)) (:: BoxType Float))
                                 (=> (constructor (:: Display None) (unit)) (:: BoxType None))))
                 (let style (ref (select style_node specified)))
                 (let-mut root (call (:: LayoutBox new) (box_type style)))
                 (:= (select root element) (call Some ((select (select style_node node) number))))
                 (:= (select root anonymous) #f)
                 (let children (call (select (call (select (select style_node children) iter) ()) map) (build_layout_tree)))
                 (for (tuple box_type children) (call (select (ref children) group_by) ((lambda (child) (select child class))))
                      (do (if (!= (select root class) box_type)
                              (do (let-mut wrapper (call (:: LayoutBox new) ((select root class) style)))
                                  (call (select (select wrapper children) extend) (children))
                                  (call (select (select root children) push) (wrapper)))
                              (do (call (select (select root children) extend) (children))))))
                 (return root)))))

(define fn-display_list
  (list '(blank)
        '(pub (fn display_list ((life a)) ((: layout_root (ref (gen LayoutBox ((life a)))))) DisplayList
              (do (let-mut list (call (:: Vec new) ()))
                  (call (select layout_root render) ((ref (mut list))))
                  (return list))))))

(define impl-LayoutBox-render
  (list '(blank)
        '(impl ((life a)) LayoutBox
               (fn render () ((: self (ref Self)) (: list (ref (mut DisplayList)))) (unit)
                   (do (call (select list push)
                             ((struct (:: DisplayCommand SolidColor)
                                ((: color (select (select self style) background_color))
                                 (: x (select (select self border_box) x))
                                 (: y (select (select self border_box) y))
                                 (: width (select (select self border_box) width))
                                 (: height (select (select self border_box) height))))))
                     (call (select list push)
                           ((struct (:: DisplayCommand SolidColor)
                              ((: color (select (select self style) border_color))
                               (: x (select (select self border_box) x))
                               (: y (select (select self border_box) y))
                               (: width (select (select self border) left))
                               (: height (select (select self border_box) height))))))
                     (call (select list push)
                           ((struct (:: DisplayCommand SolidColor)
                              ((: color (select (select self style) border_color))
                               (: x (- (+ (select (select self border_box) x)
                                          (select (select self border_box) width))
                                       (select (select self border) right)))
                               (: y (select (select self border_box) y))
                               (: width (select (select self border) right))
                               (: height (select (select self border_box) height))))))
                     (call (select list push)
                           ((struct (:: DisplayCommand SolidColor)
                              ((: color (select (select self style) border_color))
                               (: x (select (select self border_box) x))
                               (: y (select (select self border_box) y))
                               (: width (select (select self border_box) width))
                               (: height (select (select self border) top))))))
                     (call (select list push)
                           ((struct (:: DisplayCommand SolidColor)
                              ((: color (select (select self style) border_color))
                               (: x (select (select self border_box) x))
                               (: y (- (+ (select (select self border_box) y)
                                          (select (select self border_box) height))
                                       (select (select self border) bottom)))
                               (: width (select (select self border_box) width))
                               (: height (select (select self border) bottom))))))


                     (for child (call (select (call (select (select self children) iter) ()) rev) ())
                          (do (call (select child render) (list)))))))))

(define header
  (append directives
          imports
          struct-Layout
          struct-LayoutBox
          impl-LayoutBox-new
          impl-Display-for-LayoutBox
          fn-layout_tree
          fn-build_layout_tree
          fn-display_list
          impl-LayoutBox-render))

(define boilerplate
"
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(dead_code)]

///! Basic CSS block layout.
///
/// N.B.: The version of this file kept under version control is meant as a
/// \"safe\" fallback/baseline, omitting more recent improvements to the CSS
/// attribute grammar. Please don't check in each new auto-generated version,
/// especially while still debugging.

use crate::dom::DocumentNode;
use crate::style::{StyledTree, StyledNode, Style, DisplayType, Floated, Positioned, Overflow};
use crate::paint::DisplayList;
use crate::utility::{Pixels, Edge, Rect, FloatCursor, MarginAccumulator};
use crate::lazy::Lazy;
use std::fmt;
use itertools::Itertools;

const CASSIUS_LAYOUT_NAME: &str = \"doc-2\";

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
            \"({} :matched true :w {} :h {} :fs {} :scrollw {})\",
            CASSIUS_LAYOUT_NAME,
            self.parameters.viewport_width, self.parameters.viewport_height,
            self.parameters.font_size, self.parameters.scrollbar_width
        );
        let body = format!(
            \"([VIEW :w {}] {})\",
            self.parameters.viewport_width,
            self.layout_root
        );
        write!(f, \"(define-layout {} {})\", head, body)
    }
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
    /// A style node with a display type of \"none\" is omitted.
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
            \":x {} :y {} :w {} :h {}\",
            self.layout.content_box.x,
            self.layout.content_box.y,
            self.layout.content_box.width,
            self.layout.content_box.height
        );
        let elem = self.document_node.map(|doc_node| doc_node.index);
        let text = self.document_node.and_then(DocumentNode::as_text);
        let header = match self.class {
            _ if self.is_anon() =>
                String::from(\"[ANON]\"),
            Text =>
                format!(\"[TEXT {} :text \\\"{}\\\"]\", geometry, text.unwrap()),
            Line =>
                String::from(\"[LINE]\"),
            Inline | InlineRoot =>
                format!(\"[INLINE :elt {}]\", elem.unwrap()),
            InlineBlock =>
                format!(\"[INLINE {} :elt {}]\", geometry, elem.unwrap()),
            BlockRoot | Block | Floated =>
                format!(\"[BLOCK {} :elt {}]\", geometry, elem.unwrap()),
        };

        f.write_str(\"(\")?;
        f.write_str(&header)?;
        for child in self.children.iter() {
            write!(f, \" {}\", child)?;
        }
        f.write_str(\")\")?;
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
        } else if style_node.node.tag() == Some(\"html\") {
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
                        unimplemented!(\"sticky positioning unsupported\"),
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
    /// Block-axis margin above this box.
    margin_above: MarginAccumulator,
    /// Block-axis margin below this box.
    margin_below: MarginAccumulator,
    /// Logical size in the block (vertical) axis, excluding overflow+margin.
    block_size: Pixels,
    /// Logical size in the inline (horizontal) axis, excluding overflow+margin.
    inline_size: Pixels,
    /// Actual size in the block (vertical) axis, including overflow+margin.
    block_extent: Pixels,
    /// Actual size in the inline (horizontal) axis, including overflow+margin.
    inline_extent: Pixels,
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
")

; ---------------------------------
; Generation of tree data structure
; ---------------------------------

(define/match (generate-child-field child)
  [((ag:child/one name (ag:interface sort _ _)))
   `(: ,name (gen Box (,sort)))]
  [((ag:child/seq name (ag:interface sort _ _)))
   `(: ,name (gen Vec (,sort)))])

(define/match (generate-label-field label)
  [((ag:label name type))
   `(: ,name ,type)])

(define (generate-class-field interface)
  (define sort (symbol-append (ag:interface-name interface) 'Class))
  `(: class ,sort))

(define (generate-class-variant class)
  (define name (ag:class-name class))
  (define fields (map generate-child-field (ag:class-children* class)))
  `(variant ,name (record . ,fields)))

(define (generate-interface-enumeration interface)
  (define sort (symbol-append (ag:interface-name interface) 'Class))
  (define classes (ag:interface-classes interface))

  `(enum ,sort () . ,(map generate-class-variant classes)))

(define (generate-interface-structure interface)
  (define sort (ag:interface-name interface))
  (define fields
    (cons (generate-class-field interface)
          (map generate-label-field (ag:interface-labels interface))))

  `(struct ,sort () (record . ,fields)))

(define (generate-class-enumeration interface)
  (define sort (ag:interface-name interface))
  (define type-name (symbol-append sort 'Class))

  (define variants
    (for/list ([class (ag:interface-classes interface)])
      (define tag (ag:class-name class))
      `(variant ,tag (unit))))

  (list '(blank)
        '(hash derive (Clone Copy PartialEq Eq Debug))
        `(enum ,type-name . ,variants)))

(define (generate-structure G)
  (define interfaces (ag:grammar-interfaces G))
  (define classes (ag:grammar-classes G))

  (append (map generate-interface-structure interfaces)
          (map generate-interface-enumeration interfaces)))

; ---------------------------------
; Generation of tree traversal code
; ---------------------------------

(define *class* (make-parameter #f))
(define *traversal* (make-parameter #f))

(define (normalize-fieldname field)
  (cond
    [(equal? field 'layout.float_cursor_in)
     '|layout.float_cursor|]
    [(equal? field 'layout.float_cursor_out)
     '|layout.float_cursor|]
    [else
     field]))

(define (contract-fieldname field)
  (string->symbol (string-replace (symbol->string field) "." "_")))

(define (generate-term term)
  (define/match (recur term)
    [((ex:const v)) v]
    [((ex:field/get (cons 'self field)))
     `(select self ,(normalize-fieldname field))]
    [((ex:field/get (cons child field)))
     `(select ,child ,(normalize-fieldname field))]
    [((ex:field/cur (cons child field)))
     (define cursor (symbol-append child '_i))
     `(select ,cursor ,(normalize-fieldname field))]
    [((ex:field/acc (cons object field)))
     (contract-fieldname (normalize-fieldname field))]
    [((ex:field/sup (cons object field)))
     (contract-fieldname (normalize-fieldname field))]
    ; [((ex:field/peek (cons child field) default))
    ;  (define peek-child `(call (select ,(symbol-append child 'iter) peek) ()))
    ;  `(call (select ,first-child map_or_else)
    ;         ((lambda () ,(recur default))
    ;          (lambda (node) (select node ,(normalize-fieldname field)))))]
    [((ex:field/first (cons child field) default))
     (define first-child `(call (select ,child first) ()))
     `(call (select ,first-child map_or_else)
            ((lambda () ,(recur default))
             (lambda (node) (select node ,(normalize-fieldname field)))))]
    [((ex:field/last (cons child field) default))
     (define last-child `(call (select ,child last) ()))
     `(call (select ,last-child map_or_else)
            ((lambda () ,(recur default))
             (lambda (node) (select node ,(normalize-fieldname field)))))]
    [((ex:branch condition consequent alternate))
     `(if ,(recur condition)
          ,(recur consequent)
          ,(recur alternate))]
    [((ex:logic operator operands))
     `(,operator . ,(map recur operands))]
    [((ex:order comparison left right))
     `(,comparison ,(recur left) ,(recur right))]
    [((ex:arith operator operands))
     `(,operator . ,(map recur operands))]
    [((ex:call function arguments))
     `(call ,function
            ,(map recur arguments))]
    [((ex:invoke receiver method arguments))
     `(call (select ,(recur receiver) ,method)
            ,(map recur arguments))])
  (recur term))

(define (attribute->formula attr)
  (ag:rule-formula (ag:class-ref*/rule (*class*) attr)))

(define (command->statement* command)
  (match command
    [(ag:iter child body)
     (define command-list (flatten body))
     (define loop-body
       (append (map command->statement/iteration command-list)
               (map command->statement/increment command-list)))

     (define prologue (map command->statement/prologue command-list))
     (define loop (list (ir:foreach child (ag:iter-rev? command) loop-body)))
     (define epilogue (map command->statement/epilogue command-list))

     (append prologue loop epilogue)]
    [(ag:eval attr)
     (define expr (attribute->formula attr))
     (list (ir:assign (ex:field/get attr) expr))]
    [(ag:recur child)
     (list (ir:invoke (ex:field/get child (*traversal*))))]
    [(ag:skip)
     (list (ir:skip))]))

(define (command->statement/prologue command)
  (match command
    [(ag:eval attr)
     (match (attribute->formula attr)
       [(ag:fold init _) (ir:declare (ex:field/acc attr) init)]
       [(ag:scan init _) (ir:declare (ex:field/acc attr) init)]
       [_ (ir:skip)])]
    [(ag:recur _) (ir:skip)]
    [(ag:skip) (ir:skip)]))

(define (command->statement/iteration command)
  (match command
    [(ag:eval attr)
     (match (attribute->formula attr)
       [(ag:fold _ next) (ir:assign (ex:field/get attr) next)]
       [(ag:scan _ next) (ir:assign (ex:field/cur attr) next)]
       [expr (ir:assign (ex:field/cur attr) expr)])]
    [(ag:recur child)
     (ir:invoke (ex:field/cur (cons child (*traversal*))))]
    [(ag:skip) (ir:skip)]))

(define (command->statement/increment command)
  (match command
    [(ag:eval attr)
     (match (attribute->formula attr)
       [(ag:fold _ _) (ir:assign (ex:field/acc attr) (ex:field/get attr))]
       [(ag:scan _ _) (ir:assign (ex:field/acc attr) (ex:field/cur attr))]
       [_ (ir:skip)])]
    [(ag:recur _) (ir:skip)]
    [(ag:skip) (ir:skip)]))

(define (command->statement/epilogue command)
  (match command
    [(ag:eval attr)
     (match (attribute->formula attr)
       [(ag:fold _ _) (ir:assign (ex:field/get attr) (ex:field/acc attr))]
       [(ag:scan _ _) (ir:skip)]
       [_ (ir:skip)])]
    [(ag:recur _) (ir:skip)]
    [(ag:skip) (ir:skip)]))

(define (generate-statement statement)
  (match statement
    [(ir:foreach child rev? statement-list)
     (define iterator
       (let ([base `(call (select (select self ,child) iter_mut) ())])
         (if rev? `(call (select ,base rev) ()) base)))
     (define cursor (symbol-append child '_i))
     (define body (map generate-statement statement-list))

     `(for ,cursor ,iterator (do . ,body))]
    [(ir:declare lhs rhs)
     `(let-mut ,(generate-term lhs) ,(generate-term rhs))]
    [(ir:assign lhs rhs)
     `(:= ,(generate-term lhs) ,(generate-term rhs))]
    [(ir:invoke method)
     `(call ,(generate-term method) ())]
    [(ir:skip)
     `(skip)]))

(define (generate-visitor visitor)
  (parameterize ([*class* (ag:visitor-class visitor)])
    (define interface (ag:class-interface (*class*)))
    (define commands (ag:visitor-commands visitor))

    (define sort (symbol-append (ag:interface-name interface) 'Class))
    (define kind (ag:class-name (*class*)))
    (define variant `(:: ,sort ,kind))
    (define fields (map ag:child-name (ag:class-children* (*class*))))
    (define pattern `(constructor ,variant (record . ,fields)))

    (define statement-list (append-map command->statement* (flatten commands)))
    (define body (map generate-statement statement-list))

    `(=> (constructor (:: LayoutClass ,kind) (unit)) (do . ,body))))

(define (generate-traversal G traversal)
  (parameterize ([*traversal* (ag:traversal-name traversal)])

    (for/list ([interface (ag:grammar-interfaces G)]
               #:when (eq? (ag:interface-name interface) 'LayoutNode))
      (define sort (ag:interface-name interface))
      (define cases
        (map generate-visitor
             (ag:traversal-ref/interface traversal interface)))
      (define default (list '(=> _ (skip))))

      `(impl ((life a)) ,sort
             (fn ,(*traversal*) () ((: self (ref (mut Self)))) (unit)
                 (do (match (select self class)
                       .
                       ,(append cases default))))))))

(define (generate-program G S)
  (append ;header
          ;(add-between (generate-structure G) `(blank))
          (list `(raw ,boilerplate)
                '(blank))
          (add-between (generate-traversal G S) '(blank))))
