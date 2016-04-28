interface SumI {
}

interface TermI {
    var index : int;
    var value : int;
    var total : int;
}

class Sum : SumI {
    attributes {
        var sum : int;
    }
    children {
        terms : [TermI];
    }
    actions {
        loop terms {
	    terms.index := fold -1 .. terms$-.index + 1;
	    terms.total := fold 0 .. terms$-.total + terms$i.value;
	}
	self.sum := terms$$.total;
    }
}

class Term : TermI {
    attributes {
        input factor : int;
        input constant : int;
    }
    actions {
        self.value := self.factor * self.index + self.constant;
    }
}
