extern crate crossbeam;

use std::cell::UnsafeCell;
use std::time::Instant;

use crossbeam::thread::*;

type int = i32;
type float = f32;


pub struct IRoot {
    class: IRootKind
}

pub enum IRootKind {
    IsRoot(Root)
}
use IRootKind::*;

pub struct Root {
    radius: int,
    centerRadius: int,
    centerAlpha: int,
    w: int,
    h: int,
    child: Box<Node>
}


pub struct Node {
    open: int,
    show: int,
    r: int,
    parentTotR: int,
    alpha: int,
    sectorAng: int,
    maxR: int,
    bgcolor: int,
    render: int,
    rootCenterX: int,
    rootCenterY: int,
    show_out: int,
    numOpenChildren: int,
    class: NodeKind
}

pub enum NodeKind {
    IsRadial(Radial),
    IsLeaf(Leaf)
}
use NodeKind::*;

pub struct Radial {
    numOpenGrandChildren: int,
    child: Vec<Node>
}

pub struct Leaf {}
impl IRoot {
    pub fn zip(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.child.rootCenterY = (class.centerRadius * class.centerAlpha);
                class.child.show = 1;
                class.child.maxR = class.radius;
                class.child.parentTotR = 0;
                class.child.sectorAng = 360;
                class.child.zip();
            }
        }
    }
}

impl Node {
    pub fn zip(&mut self) -> () {
        match &mut self.class {
            IsRadial(ref mut class) => {
                let mut show_out_acc = 0;
                for child_cur in class.child.iter_mut() {
                    child_cur.show = (self.show * child_cur.open);
                    child_cur.zip();
                    child_cur.show_out = (show_out_acc + child_cur.show);
                    show_out_acc = child_cur.show_out;
                }
                class.numOpenGrandChildren = class.child[(class.child.len() - 1)].numOpenChildren;
                self.numOpenChildren = class.child[(class.child.len() - 1)].show_out;
            },
            IsLeaf(ref mut class) => {
                self.numOpenChildren = 0;
            }
        }
    }
}

impl IRoot {
    pub fn pre(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.child.show_out = 1;
                class.child.rootCenterX = (class.centerRadius * class.centerAlpha);
                class.child.alpha = 45;
                class.child.pre();
            }
        }
    }
}

impl Node {
    pub fn pre(&mut self) -> () {
        match &mut self.class {
            IsRadial(ref mut class) => {
                self.r = if (((self.maxR - self.parentTotR) / 3) < 10) { 10 } else { ((self.maxR - self.parentTotR) / 4) };
                self.render = (self.rootCenterX + (self.rootCenterY + ((self.show * (self.parentTotR + self.r)) + (self.alpha + (self.sectorAng + (((self.show * (4 * self.r)) / 5) + self.bgcolor))))));
                let mut alpha_acc = if (class.numOpenGrandChildren > 0) { (self.alpha - ((self.sectorAng / 2) - (self.sectorAng / (class.numOpenGrandChildren * 2)))) } else { 0 };
                for child_cur in class.child.iter_mut() {
                    child_cur.maxR = self.maxR;
                    child_cur.parentTotR = (self.parentTotR + self.r);
                    child_cur.rootCenterX = self.rootCenterX;
                    child_cur.rootCenterY = self.rootCenterY;
                    child_cur.sectorAng = if (class.numOpenGrandChildren > 0) { (child_cur.show * (self.sectorAng / class.numOpenGrandChildren)) } else { 0 };
                    child_cur.alpha = if (class.numOpenGrandChildren > 0) { (alpha_acc + (child_cur.show * (self.sectorAng / class.numOpenGrandChildren))) } else { 0 };
                    alpha_acc = child_cur.alpha;
                }
                for child_cur in class.child.iter_mut() {
                    child_cur.pre();
                }
            },
            IsLeaf(ref mut class) => {
                self.r = if (((self.maxR - self.parentTotR) / 3) < 10) { 10 } else { ((self.maxR - self.parentTotR) / 4) };
                self.render = (self.rootCenterX + (self.rootCenterY + ((self.show * (self.parentTotR + self.r)) + (self.alpha + (self.sectorAng + (((self.show * (4 * self.r)) / 5) + self.bgcolor))))));
            }
        }
    }
}

pub fn evaluate(tree: IRoot) -> () {
    let cell = UnsafeCell::new(tree);
    let tree = unsafe { cell.get().as_mut().unwrap() };
    tree.zip();
    tree.pre();
}
