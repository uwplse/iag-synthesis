extern crate crossbeam;

use std::cell::UnsafeCell;
use std::time::Instant;

use crossbeam::thread::*;

type int = i32;
type float = f32;


pub struct Top {
    class: TopKind
}

pub enum TopKind {
    IsRoot(Root)
}
use TopKind::*;

pub struct Root {
    root: Box<HVBox>
}


pub struct HVBox {
    width: int,
    height: int,
    width_out: int,
    height_out: int,
    right: int,
    bottom: int,
    visible: bool,
    class: HVBoxKind
}

pub enum HVBoxKind {
    IsHBox(HBox),
    IsVBox(VBox),
    IsLeaf(Leaf)
}
use HVBoxKind::*;

pub struct HBox {
    childs: Vec<HVBox>
}

pub struct VBox {
    childs: Vec<HVBox>
}

pub struct Leaf {
    width_in: int,
    height_in: int
}
impl Top {
    pub fn post(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.root.post();
            }
        }
    }
}

impl HVBox {
    pub fn post(&mut self) -> () {
        match &mut self.class {
            IsHBox(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post();
                }
                let mut width_out_acc = 0;
                let mut height_out_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.width_out = (width_out_acc + childs_cur.width);
                    childs_cur.height_out = if (height_out_acc > childs_cur.height) { height_out_acc } else { childs_cur.height };
                    width_out_acc = childs_cur.width_out;
                    height_out_acc = childs_cur.height_out;
                }
                self.width = if self.visible { class.childs[(class.childs.len() - 1)].width_out } else { 0 };
                self.height = if self.visible { class.childs[(class.childs.len() - 1)].height_out } else { 0 };
            },
            IsVBox(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post();
                }
                let mut width_out_acc = 0;
                let mut height_out_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.width_out = if (width_out_acc > childs_cur.width) { width_out_acc } else { childs_cur.width };
                    childs_cur.height_out = (height_out_acc + childs_cur.height);
                    width_out_acc = childs_cur.width_out;
                    height_out_acc = childs_cur.height_out;
                }
                self.height = if self.visible { class.childs[(class.childs.len() - 1)].height_out } else { 0 };
                self.width = if self.visible { class.childs[(class.childs.len() - 1)].width_out } else { 0 };
            },
            IsLeaf(ref mut class) => {
                self.width = class.width_in;
                self.height = class.height_in;
            }
        }
    }
}

impl Top {
    pub fn pre(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.root.right = class.root.width;
                class.root.height_out = class.root.height;
                class.root.bottom = class.root.height;
                class.root.width_out = class.root.width;
                class.root.pre();
            }
        }
    }
}

impl HVBox {
    pub fn pre(&mut self) -> () {
        match &mut self.class {
            IsHBox(ref mut class) => {
                let mut right_acc = (self.right - self.width);
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.bottom = self.bottom;
                    childs_cur.right = (right_acc + childs_cur.width);
                    right_acc = childs_cur.right;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre();
                }
            },
            IsVBox(ref mut class) => {
                let mut bottom_acc = self.bottom;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.right = self.right;
                    childs_cur.bottom = (bottom_acc + childs_cur.height);
                    bottom_acc = childs_cur.bottom;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre();
                }
            },
            IsLeaf(ref mut class) => {}
        }
    }
}

pub fn evaluate(tree: Top) -> () {
    let cell = UnsafeCell::new(tree);
    let tree = unsafe { cell.get().as_mut().unwrap() };
    tree.post();
    tree.pre();
}

