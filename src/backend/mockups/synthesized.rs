use crate::context::LayoutContext;
use crate::flow::{Flow, GetBaseFlow};
use crate::incremental::RelayoutMode;
use crate::traversal::{preorder, postorder};

pub fn reflow(tree: &mut dyn Flow, layout_context: &LayoutContext, relayout_mode: RelayoutMode) -> () {
    postorder(tree, &|node| {
        node.bubble_inline_sizes();
    });
    preorder(tree, &|node| {
        node.assign_inline_sizes(layout_context);
    });
    postorder(tree, &|node| {
        node.assign_block_size(layout_context);
    });
    preorder(tree, &|node| {
        node.compute_stacking_relative_position(layout_context);
    });
}
