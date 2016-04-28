interface Root { }

interface Point {
    var bx : int;
    var by : int;
    var x : int;
    var y : int;
}

trait InheritBase {
    children {
        p : [Point];
    }
    attributes {
        var w : int;
        var h : int;
    }
    actions {
        loop p {
            p.bx := x;
            p.by := y;
            w := fold -x .. p$i.x;
            h := fold -y .. p$i.y;
        }
    }
}

class Origin(InheritBase) : Root {
    attributes {
        input x : int;
        input y : int;
    }
}

class Relative(InheritBase) : Point {
    attributes {
        input dx : int;
        input dy : int;
    }
    actions {
        x := bx + dx;
        y := by + dy;
    }
}

class Fixed(InheritBase) : Point {
    attributes {
        var dx : int;
        var dy : int;
        input fx : int;
	input fy : int;
    }
    actions {
        dx := x - bx;
        dy := y - by;
        x := fx;
        y := fy;
    }
}

class Endpoint : Point {
    actions {
        x := bx;
        y := by;
    }
}