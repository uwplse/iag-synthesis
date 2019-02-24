interface BaseFlow {
    // The position of the upper left corner of the border box of this flow,
    // relative to the containing block.
    var containingX : Au;
    var containingY : Au;

    // Width and height of border box
    var flowHeight : Au;
    var flowWidth : Au;
    var flowX : Au;
    var flowY : Au;

    // Absolute positioning of border box of this flow
    var absX : Au;
    var absY : Au;

    var bottom : Au;
    var right : Au;

    // var intrinsPrefWidth : Au;
    // var intrinsMinWidth : Au;

    var availableWidth : Au;

    // Width and height of margin box of the flow
    var totalWidth: Au;
    var totalHeight: Au;

    //* var render : int;
    var makeList: FTLDisplayList;

    var display_list: FTLDisplayList;
    //var node_list: FTLDisplayList;

    input screenwidth: Au;
}

interface InlineBox {
    var endOfLine : bool;
    var availableTextWidth : Au;
    var baseline : AuList;
    var baselineFinal : AuList;
    var lineHeight : Au;
    var linePosY : Au;
    var posX : Au;
    var right : Au;
    var posY : Au;

    input mustEndLine : bool;
    input inlineHeight : Au;
    input inlineAscent : Au;
    input inlineWidth : Au;
    input fragSpecific : SpecificFragmentInfo;
    input fragStyle : Arc<ComputedValues>;
    input fragNode : OpaqueNode;
}

trait blockWidth{
    actions{

        selfIntrinsWidth := spec_or_zero(boxStyleWidth, availableWidth);
        selfIntrinsHeight := spec_or_zero(boxStyleHeight, Au(0));

        pt := specified(paddingTop, availableWidth);
        pb := specified(paddingBottom, availableWidth);
        pl := specified(paddingLeft, availableWidth);
        pr := specified(paddingRight, availableWidth);

        bt := borderTop;
        bb := borderBottom;
        bl := borderLeft;
        br := borderRight;

        mt := is_auto(marginTop) ? Au(0) : spec_or_zero(marginTop, availableWidth);
        mb := is_auto(marginBottom) ? Au(0) : spec_or_zero(marginBottom, availableWidth);
        ml := is_auto(marginLeft) ?
                (is_auto(boxStyleWidth) ?
                  Au(0) :
                  (is_auto(marginRight) ?
                    (availableWidth - pr - pl - bl - br - selfIntrinsWidth) / Au(2) :
                    (availableWidth - pr - pl - bl - br - selfIntrinsWidth - spec_or_zero(marginRight, availableWidth)))) :
                spec_or_zero(marginLeft, availableWidth);

        mr := (!is_auto(marginRight) && (is_auto(boxStyleWidth) || is_auto(marginLeft))) ?
                spec_or_zero(marginRight, availableWidth) :
                (is_auto(boxStyleWidth) ?
                  Au(0) :
                  (is_auto(marginLeft) ?
                    (availableWidth - pr - pl - br - bl - selfIntrinsWidth) / Au(2) :
                    (availableWidth - pr - pl - br - bl - selfIntrinsWidth - spec_or_zero(marginLeft, availableWidth))));


        computedWidth := is_root ?
                           screenwidth:
                           (is_auto(boxStyleWidth) ?
                             // max(intrinsMinWidth, availableWidth) - sumMBP:
                             availableWidth - mbpHoriz:
                             selfIntrinsWidth);

        mbpVert  := mt + mb + pt + pb + bt + bb;
        mbpHoriz := ml + mr + pl + pr + bl + br;

    }
}

trait defaultBaseValues {
    actions {
        flowHeight := Au(0);
        flowWidth := Au(0);
        flowX := Au(0);
        flowY := Au(0);

        totalHeight := Au(0);
        totalWidth := Au(0);
    }
}

class BlockFlow (blockWidth) : BaseFlow {
    children {
        flowChildren : [BaseFlow];
    }
    attributes {

        var childsHeight : Au;
        var childsWidth : Au;

        input is_root : bool;
        input fragment: &Fragment;
        input boxStyleHeight : LengthOrPercentageOrAuto;
        input boxStyleWidth : LengthOrPercentageOrAuto;

        /// The size of Block Flow's box.
        /// The size includes padding and border, but not margin.
        // var boxWidth : Au;
        // var boxHeight : Au;

        var computedWidth : Au;

        var childsHeight : Au;
        var childsWidth : Au;

        var mbpVert : Au;
        var mbpHoriz : Au;
        var selfIntrinsWidth : Au;
        var selfIntrinsHeight : Au;


        // Margin, padding and border attributes.

        // Full names are the values read from the style file, whereas the
        // two-letter names are the computed values in Au

        input marginTop : LengthOrPercentageOrAuto;
        input marginBottom : LengthOrPercentageOrAuto;
        input marginLeft : LengthOrPercentageOrAuto;
        input marginRight : LengthOrPercentageOrAuto;

        var mt : Au;
        var mb : Au;
        var ml : Au;
        var mr : Au;

        input paddingTop : LengthOrPercentage;
        input paddingBottom : LengthOrPercentage;
        input paddingLeft : LengthOrPercentage;
        input paddingRight : LengthOrPercentage;

        var pt : Au;
        var pb : Au;
        var pl : Au;
        var pr : Au;

        input borderTop : Au;
        input borderBottom : Au;
        input borderLeft : Au;
        input borderRight : Au;

        var bt : Au;
        var bb : Au;
        var bl : Au;
        var br : Au;
    }
    actions {
        loop flowChildren {
            childsHeight := fold Au(0) .. ($-.childsHeight + flowChildren$i.totalHeight);
            childsWidth := fold Au(0) .. max($-.childsWidth, flowChildren$i.totalWidth);

            //intrinsMinWidth := fold selfIntrinsWidth + sumMBP
            //                   .. max(self$-.intrinsMinWidth, flowChildren$i.intrinsMinWidth + sumMBP);
            // intrinsPrefWidth := fold selfIntrinsWidth + sumMBP ..
            //  ((selfIntrinsWidth == Au(0)) ? (max($-.intrinsPrefWidth, sumMBP + flowChildren$i.intrinsPrefWidth))
            //                               : ($-.intrinsPrefWidth));

            flowChildren.bottom := fold pt + bt .. (flowChildren$-.bottom + flowChildren$i.totalHeight);
            flowChildren.right := fold Au(0) .. (flowChildren$i.totalWidth + pl + bl);

            flowChildren.containingX := fold Au(0) .. flowChildren$i.right - flowChildren$i.totalWidth;
            flowChildren.containingY := fold Au(0) .. flowChildren$i.bottom - flowChildren$i.totalHeight;

            flowChildren.absX := fold Au(0) .. flowChildren$i.containingX + absX + ml;
            flowChildren.absY := fold Au(0) .. flowChildren$i.containingY + absY + mt;

            flowChildren.availableWidth := fold Au(0) .. computedWidth;

            display_list := fold makeList .. merge_lists($-.display_list, flowChildren$i.display_list);

            //* makeLists := fold 0 .. merge_lists(display_list, flowChildren$i.display_list)
            //                      + flowChildren$i.render;
        }

        flowWidth :=  is_root ? screenwidth : computedWidth + pl + pr + bl + br;
        flowHeight := (selfIntrinsHeight == Au(0)) ? childsHeight + pb + pt + bb + bt
                                                   : selfIntrinsHeight + pb + pt + bb + bt;
        flowX := containingX + ml;
        flowY := containingY + mt;

        totalWidth := flowWidth + ml + mr;
        totalHeight := flowHeight + mt + mb;

        //boxWidth := flowWidth;
        //boxHeight := flowHeight;

        //* display_list := new_display_list();

        // Adds items to display list layering from bottom up. In this case background
        // comes before border.

        makeList := add_border(add_background(new_display_list(), fragment, absX + ml,
                                               absY + mt, flowWidth, flowHeight),
                                fragment, absX + ml, absY + mt, flowWidth, flowHeight, bt, br, bb, bl);

        //* render := addBackground(display_list, fragment, absX + ml, absY + mt, flowWidth, flowHeight)
        //         + addBorder(display_list, fragment, absX + ml, absY + mt, flowWidth, flowHeight,
        //                     bt, br, bb, bl);
    }
}

class InlineFlow: BaseFlow {
    children {
        text : [InlineBox];
    }

    attributes {
        var baselineLast: AuList;
    }

    actions {
        flowWidth := availableWidth;
        flowX := containingX;
        flowY := containingY;
        // intrinsPrefWidth := Au(0);
        // intrinsMinWidth := Au(0);
        totalHeight := flowHeight;
        totalWidth := flowWidth;

        loop text by split_to_width(text$-.availableTextWidth, text$-.endOfLine) {
            text.endOfLine := fold true .. (text$i.inlinewidth > text$-.availableTextWidth) || text$i.mustEndLine;

            text.availableTextWidth := fold availableWidth ..
                                            (text$i.endOfLine) ?
                                              (availableWidth) :
                                              (text$-.availableTextWidth - text$i.inlinewidth);

            text.baseline := fold EmptyList() .. text$-.endOfLine ? 
                                                append(text$-.baseline,text$i.inlineAscent) : 
                                                modifyLast(text$-.baseline, max(getLast(text$-.baseline), text$i.inlineAscent)); 
        }

        baselineLast := text$$.baseline;

        loop text {
            text.baselineFinal := fold baselineLast .. text$-.endOfLine ? text$-.baselineFinal : butFirst(text$-.baselineFinal);
        }

        loop text {
            text.right := fold Au(0) .. text$-.endOfLine ?
                                          text$i.inlinewidth :
                                          text$-.right + text$i.inlinewidth;

            text.posX := fold Au(0) .. absX + text$i.right - text$i.inlinewidth;

            text.lineHeight := fold Au(0) .. text$-.endOfLine ?
                                               text$i.inlineHeight :
                                               max(text$-.lineHeight, text$i.inlineHeight);

            text.linePosY := fold Au(0) .. text$-.endOfLine ?
                                             (text$-.linePosY + text$-.lineHeight) :
                                             text$-.linePosY;

            text.posY := fold Au(0) .. absY + text$i.linePosY + getFirst(text$i.baselineFinal) - text$i.inlineAscent;

            flowHeight := fold Au(0) .. text$i.lineHeight + $-.flowHeight;

            display_list := fold new_display_list() .. add_text_fragment($-.display_list,
                                                                         text$i.fragSpecific,
                                                                         text$i.fragStyle,
                                                                         text$i.fragNode,
                                                                         text$i.posX,
                                                                         text$i.posY,
                                                                         text$i.availableTextWidth,
                                                                         text$i.lineHeight);
        }
    }
}

class TableColGroupFlow (defaultBaseValues) : BaseFlow {}


//class TableWrapperFlow {}
//class TableFlow {}
//class TableRowGroupFlow {}
//class TableRowFlow {}
//class TableCaptionFlow {}
//class TableCellFlow {}
