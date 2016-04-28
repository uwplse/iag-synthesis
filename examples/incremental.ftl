interface Node {
  x : int;
  y : int;
  z : int;
}

class Branch : Node {
  children {
    left : Node;
    right : Node;
  }
  input {
    x : int;
    y : int;
  }
  phantom {
    w : int;
  }
  output {
    z : int;
  }
  initial {
    w := f(x, y);
    z := g(w) + w;
  }
  update {
    w <= delta[x] || delta[y];
    z <= delta[w];
    parent <= delta[*];
    left <= delta[x];
    right <= delta[y];
  }
}
