interface BaseFlow {
    // The position of the upper left corner of the border box of this flow,
    // relative to the containing block.
    var containingX : int;
    var containingY : int;

    // Width and height of border box
    var flowHeight : int;
    var flowWidth : int;
    var flowX : int;
    var flowY : int;

    // Absolute positioning of border box of this flow
    var absX : int;
    var absY : int;

    var bottom : int;
    var right : int;

    // var intrinsPrefWidth : int;
    // var intrinsMinWidth : int;

    var availableWidth : int;

    // Width and height of margin box of the flow
    var totalWidth: int;
    var totalHeight: int;

    //* var render : int;
    var makeList : int; // yeah, this is faked

    var display_list: int; // yeah, this is faked
    //var node_list: int; // yeah, this is faked

    input screenwidth: int;
}

interface InlineBox {
    var endOfLine : bool;
    var availableTextWidth : int;
    var baseline : int;
    var baselineFinal : int;
    var lineHeight : int;
    var linePosY : int;
    var posX : int;
    var right : int;
    var posY : int;

    input mustEndLine : bool;
    input inlineHeight : int;
    input inlineAscent : int;
    input inlineWidth : int;
    input fragSpecific : SpecificFragmentInfo;
    input fragStyle : Arc<ComputedValues>;
    input fragNode : OpaqueNode;
}

trait blockWidth{
    actions{

        selfIntrinsWidth := spec_or_zero(boxStyleWidth, availableWidth);
        selfIntrinsHeight := spec_or_zero(boxStyleHeight, 0);

        pt := specified(paddingTop, availableWidth);
        pb := specified(paddingBottom, availableWidth);
        pl := specified(paddingLeft, availableWidth);
        pr := specified(paddingRight, availableWidth);

        bt := borderTop;
        bb := borderBottom;
        bl := borderLeft;
        br := borderRight;

        mt := is_auto(marginTop) ? 0 : spec_or_zero(marginTop, availableWidth);
        mb := is_auto(marginBottom) ? 0 : spec_or_zero(marginBottom, availableWidth);
        ml := is_auto(marginLeft) ?
                (is_auto(boxStyleWidth) ?
                  0 :
                  (is_auto(marginRight) ?
                    (availableWidth - pr - pl - bl - br - selfIntrinsWidth) / 2 :
                    (availableWidth - pr - pl - bl - br - selfIntrinsWidth - spec_or_zero(marginRight, availableWidth)))) :
                spec_or_zero(marginLeft, availableWidth);

        mr := (!is_auto(marginRight) && (is_auto(boxStyleWidth) || is_auto(marginLeft))) ?
                spec_or_zero(marginRight, availableWidth) :
                (is_auto(boxStyleWidth) ?
                  0 :
                  (is_auto(marginLeft) ?
                    (availableWidth - pr - pl - br - bl - selfIntrinsWidth) / 2 :
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
        flowHeight := 0;
        flowWidth := 0;
        flowX := 0;
        flowY := 0;

        totalHeight := 0;
        totalWidth := 0;
    }
}

class BlockFlow (blockWidth) : BaseFlow {
    children {
        flowChildren : [BaseFlow];
    }
    attributes {

        var childsHeight : int;
        var childsWidth : int;

        input is_root : bool;
        input fragment: &Fragment;
        input boxStyleHeight : LengthOrPercentageOrintto;
        input boxStyleWidth : LengthOrPercentageOrintto;

        /// The size of Block Flow's box.
        /// The size includes padding and border, but not margin.
        // var boxWidth : int;
        // var boxHeight : int;

        var computedWidth : int;

        var childsHeight : int;
        var childsWidth : int;

        var mbpVert : int;
        var mbpHoriz : int;
        var selfIntrinsWidth : int;
        var selfIntrinsHeight : int;


        // Margin, padding and border attributes.

        // Full names are the values read from the style file, whereas the
        // two-letter names are the computed values in int

        input marginTop : LengthOrPercentageOrintto;
        input marginBottom : LengthOrPercentageOrintto;
        input marginLeft : LengthOrPercentageOrintto;
        input marginRight : LengthOrPercentageOrintto;

        var mt : int;
        var mb : int;
        var ml : int;
        var mr : int;

        input paddingTop : LengthOrPercentage;
        input paddingBottom : LengthOrPercentage;
        input paddingLeft : LengthOrPercentage;
        input paddingRight : LengthOrPercentage;

        var pt : int;
        var pb : int;
        var pl : int;
        var pr : int;

        input borderTop : int;
        input borderBottom : int;
        input borderLeft : int;
        input borderRight : int;

        var bt : int;
        var bb : int;
        var bl : int;
        var br : int;
    }
    actions {
        loop flowChildren {
            childsHeight := fold 0 .. ($-.childsHeight + flowChildren$i.totalHeight);
            childsWidth := fold 0 .. ($-.childsWidth + flowChildren$i.totalWidth);

            //intrinsMinWidth := fold selfIntrinsWidth + sumMBP
            //                   .. max(self$-.intrinsMinWidth, flowChildren$i.intrinsMinWidth + sumMBP);
            // intrinsPrefWidth := fold selfIntrinsWidth + sumMBP ..
            //  ((selfIntrinsWidth == 0) ? (max($-.intrinsPrefWidth, sumMBP + flowChildren$i.intrinsPrefWidth))
            //                               : ($-.intrinsPrefWidth));

            flowChildren.bottom := fold pt + bt .. (flowChildren$-.bottom + flowChildren$i.totalHeight);
            flowChildren.right := fold 0 .. (flowChildren$i.totalWidth + pl + bl);

            flowChildren.containingX := fold 0 .. flowChildren$i.right - flowChildren$i.totalWidth;
            flowChildren.containingY := fold 0 .. flowChildren$i.bottom - flowChildren$i.totalHeight;

            flowChildren.absX := fold 0 .. flowChildren$i.containingX + absX + ml;
            flowChildren.absY := fold 0 .. flowChildren$i.containingY + absY + mt;

            flowChildren.availableWidth := fold 0 .. computedWidth;

            display_list := fold makeList .. $-.display_list + flowChildren$i.display_list;

            //* makeLists := fold 0 .. display_list + flowChildren$i.display_list
            //                      + flowChildren$i.render;
        }

        flowWidth :=  is_root ? screenwidth : computedWidth + pl + pr + bl + br;
        flowHeight := (selfIntrinsHeight == 0) ? childsHeight + pb + pt + bb + bt
                                                   : selfIntrinsHeight + pb + pt + bb + bt;
        flowX := containingX + ml;
        flowY := containingY + mt;

        totalWidth := flowWidth + ml + mr;
        totalHeight := flowHeight + mt + mb;

        //boxWidth := flowWidth;
        //boxHeight := flowHeight;

        //* display_list := 0;

        // Adds items to display list layering from bottom up. In this case background
        // comes before border.

        makeList := 0 + fragment + absX + ml + absY + mt + flowWidth, flowHeight +
                    fragment + absX + ml + absY + mt + flowWidth + flowHeight +
                    bt + br + bb + bl;

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
        var baselineLast: int;
    }

    actions {
        flowWidth := availableWidth;
        flowX := containingX;
        flowY := containingY;
        // intrinsPrefWidth := 0;
        // intrinsMinWidth := 0;
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
            text.right := fold 0 .. text$-.endOfLine ?
                                          text$i.inlinewidth :
                                          text$-.right + text$i.inlinewidth;

            text.posX := fold 0 .. absX + text$i.right - text$i.inlinewidth;

            text.lineHeight := fold 0 .. text$-.endOfLine ?
                                               text$i.inlineHeight :
                                               max(text$-.lineHeight, text$i.inlineHeight);

            text.linePosY := fold 0 .. text$-.endOfLine ?
                                             (text$-.linePosY + text$-.lineHeight) :
                                             text$-.linePosY;

            text.posY := fold 0 .. absY + text$i.linePosY + getFirst(text$i.baselineFinal) - text$i.inlineAscent;

            flowHeight := fold 0 .. text$i.lineHeight + $-.flowHeight;

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
