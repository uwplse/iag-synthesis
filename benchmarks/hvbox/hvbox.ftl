interface Top { }

interface HVBox {
    output width : int;
    output height : int;
    output width_out : int; // outer width
    output height_out : int; // outer height
    output right : int;
    output bottom : int;
    input visible : bool;
}

class Root : Top {
    children {
        root : HVBox;
    }
    evaluation {
        root.right := root.width;
        root.bottom := root.height;
        root.width_out := root.width;
        root.height_out := root.height;
    }
}

class HBox : HVBox {
    children {
        childs : [HVBox];
    }
    evaluation {
        loop childs {
            childs.width_out := fold 0 .. childs$-.width_out + childs.width;
            childs.height_out := fold 0 .. childs$-.height_out > childs.height ? childs$-.height_out : childs.height;
            childs.right := fold (right - width) .. childs$-.right + childs.width;
            childs.bottom := bottom;
        }

        width := visible ? childs$$.width_out : 0;
        height := visible ? childs$$.height_out : 0;
    }
}

class VBox : HVBox {
    children {
        childs : [HVBox];
    }
    evaluation {
        loop childs {
            childs.width_out := fold 0 .. childs$-.width_out > childs.width ? childs$-.width_out : childs.width;
            childs.height_out := fold 0 .. childs$-.height_out + childs.height;
            childs.bottom := fold 0 .. childs$-.bottom + childs.height;
            childs.right := right;
        }

        width := visible ? childs$$.width_out : 0;
        height := visible ? childs$$.height_out : 0;
    }
}

class Leaf : HVBox {
    attributes {
        input width_in : int;
        input height_in : int;
    }
    evaluation {
        width := width_in;
        height := height_in;
    }
}
