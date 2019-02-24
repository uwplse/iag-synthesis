interface Node {
  input open : int;
  output show : int;

  output r : int;
  output parentTotR : int;
  output alpha : int;
  output sectorAng : int;
  output maxR : int;
  
  input bgcolor : int;
  output render : int;

  output rootCenterX : int;
  output rootCenterY : int;
  
  output show_out : int;

  output numOpenChildren : int;

  //output subtreeWeight : int;
}

interface IRoot { }

class Root : IRoot {
  children {
    child : Node;
  }
  
  attributes {
    input radius : int;
    input centerRadius : int;
    input centerAlpha : int;
    
    input w : int;
    input h : int;
  }
  
  evaluation {
    child.alpha := 45;
    child.maxR := radius;
    child.parentTotR := 0;
    child.sectorAng := 360;
    
    child.show := 1;
    child.show_out := 1;
        
    child.rootCenterX := centerRadius * centerAlpha;
    child.rootCenterY := centerRadius * centerAlpha;    
  }
}

class Radial : Node {
  children {
    child : [ Node ];
  }
  evaluation {
  
    r := (maxR - parentTotR)/3 < 10 ? 10 : (maxR - parentTotR)/4;
      
    loop child {
      //subtreeWeight := fold 1 .. child$-.subtreeWeight + child.subtreeWeight;
  
      child.parentTotR := parentTotR + r;
  
      child.rootCenterX := rootCenterX;
      child.rootCenterY := rootCenterY;

      child.show := show * child.open;
    
    
      child.maxR := maxR;
      
      child.sectorAng :=  child$$.numOpenChildren > 0 ? child.show * sectorAng / child$$.numOpenChildren : 0;
      
      // FIXME: Can this be simplified?
      child.alpha := 
        fold 
           (child$$.numOpenChildren > 0 ?
            alpha - (sectorAng / 2) - (sectorAng/(child$$.numOpenChildren*2)) 
            : 0)
        .. 
        (child$$.numOpenChildren > 0 ?
          child$-.alpha + child.show * (sectorAng/child$$.numOpenChildren)
          : 0);

      child.show_out := fold 0 .. child$-.show_out + child.show;
    }
    self.numOpenChildren := child$$.show_out;
    self.render := rootCenterX + rootCenterY + show * (parentTotR + r) + alpha + sectorAng + (show * 4 * r) / 5 + bgcolor;
  } 
}

class Leaf : Node {
  evaluation {
    self.r := (maxR - parentTotR)/3 < 10 ? 10 : (maxR - parentTotR)/4;
    self.numOpenChildren := 0;
    self.render := rootCenterX + rootCenterY + show * (parentTotR + r) + alpha + sectorAng + (show * 4 * r) / 5 + bgcolor;
  } 
}
