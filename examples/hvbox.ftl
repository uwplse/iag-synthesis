interface Top {
    input a : int;
}

interface HVBox {
    var width : int;
    var height : int;
    var right : int;
    var bottom : int;
    input visible : bool;
}

class Root : Top {
    children {
        root : HVBox;
    }
    actions {
        root.right := root.width;
        root.bottom := root.height;
    }
}

class HBox : HVBox {
    children {
        childs : [HVBox];
    }
    attributes {
        var childsWidth : int;
        var childsHeight : int;
    }
    actions {
        loop childs {
            childsWidth := fold 0 .. $-.childsWidth + childs$i.width;
            childsHeight := fold 0 .. $-.childsHeight > childs$i.height ? $-.childsHeight : childs$i.height;
            childs.right := fold (right - width) .. childs$-.right + childs$i.width;
            childs.bottom := bottom;
        }

        width := visible ? childsWidth : 0;
        height := visible ? childsHeight : 0;
    }
}

class VBox : HVBox {
    children {
        childs : [HVBox];
    }
    attributes {
        var childsWidth : int;
        var childsHeight : int;
    }
    actions {
        loop childs {
            childsWidth := fold 0 .. $-.childsWidth > childs$i.width ? $-.childsWidth : childs$i.width;
            childsHeight := fold 0 .. $-.childsHeight + childs$i.height;
            childs.bottom := fold 0 .. childs$-.bottom + childs$i.height;
            childs.right := right;
        }

        width := visible ? childsWidth : 0;
        height := visible ? childsHeight : 0;
    }
}

class Leaf : HVBox {
    attributes {
        input width_in : int;
        input height_in : int;
    }
    actions {
        width := width_in;
        height := height_in;
    }
}
