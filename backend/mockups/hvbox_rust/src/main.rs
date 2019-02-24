extern crate rayon;
extern crate rand;
extern crate time;

use std::boxed::*;
use std::cmp::*;
use rayon::prelude::*;
// TODO: Library for rendering output?

const CUTOFF: u32 = 3;

const TREE_SPREAD: u32 = 4;
const TREE_HEIGHT: u32 = 14;

const MAX_WIDTH: u32 = 100;
const MAX_HEIGHT: u32 = 100;

type int = i32;
type float = f32;

trait Class<I>: Send {
    fn interface(&mut self) -> &mut I;

    fn traverse1(&mut self, depth: u32) -> ();

    fn traverse2(&mut self, depth: u32) -> ();
}

struct Top { }

struct Root {
    public: Top,
    root: Box<Class<HVBox>>
}

unsafe impl Send for Root { }

unsafe impl Sync for Root { }

impl Class<Top> for Root {
    fn interface(&mut self) -> &mut Top {
        &mut self.public
    }

    fn traverse1(&mut self, depth: u32) -> () {
        self.root.traverse1(depth + 1);
    }

    fn traverse2(&mut self, depth: u32) -> () {
        {
            let fields = self.root.interface();
            fields.right = fields.width;
            fields.bottom = fields.height;
            fields.width_out = fields.width;
            fields.height_out = fields.height;
        }
        self.root.traverse2(depth + 1);
    }
}

struct HVBox {
    width: u32,
    width_out: u32,
    height: u32,
    height_out: u32,
    right: u32,
    bottom: u32,
    visible: bool
}

struct HBox {
    public: HVBox,
    children: Vec<Box<Class<HVBox>>>
}

unsafe impl Send for HBox { }

unsafe impl Sync for HBox { }

struct VBox {
    public: HVBox,
    children: Vec<Box<Class<HVBox>>>
}

unsafe impl Send for VBox { }

unsafe impl Sync for VBox { }

struct Leaf {
    public: HVBox,
    width_in: u32,
    height_in: u32
}

unsafe impl Send for Leaf { }

unsafe impl Sync for Leaf { }

impl Class<HVBox> for HBox {
    fn interface(&mut self) -> &mut HVBox {
        &mut self.public
    }

    fn traverse1(&mut self, depth: u32) -> () {
        if depth < CUTOFF {
            self.children
                .par_iter_mut()
                .for_each(|child| child.traverse1(depth + 1));
        } else {
            for child in self.children.iter_mut() {
                child.traverse1(depth + 1);
            }
        }

        let mut width_out: u32 = 0;
        let mut height_out: u32 = 0;
        for child in self.children.iter_mut() {
            let fields = child.interface();
            fields.width_out += width_out;
            fields.height_out = if height_out > fields.height {
                height_out
            } else {
                fields.height
            };
            width_out = fields.width_out;
            height_out = fields.height_out;
        }
        if self.public.visible {
            self.public.width = width_out;
            self.public.height = height_out;
        } else {
            self.public.width = 0;
            self.public.height = 0;
        }
    }

    fn traverse2(&mut self, depth: u32) -> () {
        let mut right = self.public.right - self.public.width;
        for child in self.children.iter_mut() {
            let fields = child.interface();
            fields.right = right + fields.width;
            fields.bottom = self.public.bottom;
            right = fields.right;
        }

        if depth < CUTOFF {
            self.children
                .par_iter_mut()
                .for_each(|child| child.traverse2(depth + 1));
        } else {
            for child in self.children.iter_mut() {
                child.traverse2(depth + 1);
            }
        }
    }
}

impl Class<HVBox> for VBox {
    fn interface(&mut self) -> &mut HVBox {
        &mut self.public
    }

    fn traverse1(&mut self, depth: u32) -> () {
        if depth < CUTOFF {
            self.children
                .par_iter_mut()
                .for_each(|child| child.traverse1(depth + 1));
        } else {
            for child in self.children.iter_mut() {
                child.traverse1(depth + 1);
            }
        }

        let mut width_out: u32 = 0;
        let mut height_out: u32 = 0;
        for child in self.children.iter_mut() {
            let fields = child.interface();
            fields.width_out = max(width_out, fields.width);
            fields.height_out = height_out + fields.height_out;
            width_out = fields.width_out;
            height_out = fields.height_out;
        }
        if self.public.visible {
            self.public.width = width_out;
            self.public.height = height_out;
        } else {
            self.public.width = 0;
            self.public.height = 0;
        }
    }

    fn traverse2(&mut self, depth: u32) -> () {
        let mut bottom = self.public.bottom;
        for child in self.children.iter_mut() {
            let fields = child.interface();
            fields.right = self.public.right;
            fields.bottom = bottom + fields.height;
            bottom = fields.bottom;
        }

        if depth < CUTOFF {
            self.children
                .par_iter_mut()
                .for_each(|child| child.traverse2(depth + 1));
        } else {
            for child in self.children.iter_mut() {
                child.traverse2(depth + 1);
            }
        }
    }
}

impl Class<HVBox> for Leaf {
    fn interface(&mut self) -> &mut HVBox {
        &mut self.public
    }

    fn traverse1(&mut self, depth: u32) -> () {
        self.public.width = self.width_in;
        self.public.height = self.height_in;
    }

    fn traverse2(&mut self, depth: u32) -> () { }
}

// Tree generation

impl Root {
    fn generate(width: u32, height: u32) -> Root {
        Root { public: Top {}, root: Box::new(<HBox>::generate(width, height - 1)) }
    }
}

impl HBox {
    fn generate(width: u32, height: u32) -> HBox {
        let make_leaf = |_| Box::new(Leaf::generate()) as Box<Class<HVBox>>;
        let make_vbox = |_| Box::new(VBox::generate(width, height - 1)) as Box<Class<HVBox>>;
        let children = if height == 1 {
            (0..width).map(make_leaf).collect()
        } else if TREE_HEIGHT - height < CUTOFF {
            (0..width).into_par_iter().map(make_vbox).collect()
        } else {
            (0..width).map(make_vbox).collect()
        };

        HBox {
            public: HVBox {
                width: 0, width_out: 0, height: 0, height_out: 0,
                right: 0, bottom: 0, visible: true
            },
            children: children
        }
    }
}

impl VBox {
    fn generate(width: u32, height: u32) -> VBox {
        let make_leaf = |_| Box::new(Leaf::generate()) as Box<Class<HVBox>>;
        let make_hbox = |_| Box::new(HBox::generate(width, height - 1)) as Box<Class<HVBox>>;
        let children = if height == 1 {
            (0..width).map(make_leaf).collect()
        } else if TREE_HEIGHT - height < CUTOFF {
            (0..width).into_par_iter().map(make_hbox).collect()
        } else {
            (0..width).map(make_hbox).collect()
        };

        VBox {
            public: HVBox {
                width: 0, width_out: 0, height: 0, height_out: 0,
                right: 0, bottom: 0, visible: true
            },
            children: children
        }
    }
}

impl Leaf {
    fn generate() -> Leaf {
        Leaf {
            public: HVBox {
                width: 0, width_out: 0, height: 0, height_out: 0,
                right: 0, bottom: 0, visible: true
            },
            width_in: 1 + rand::random::<u32>() % MAX_WIDTH,
            height_in: 1 + rand::random::<u32>() % MAX_HEIGHT
        }
    }
}

type Tree = Root;

fn generate(width: u32, height: u32) -> Tree {
    Root::generate(width, height)
}

fn parallel<L, R>(tree: &mut Tree, f: L, g: R) -> () where
    L: FnOnce(&mut Tree) + Send,
    R: FnOnce(&mut Tree) + Send {
    unsafe {
        let left = Box::from_raw(tree as *mut Tree);
        let right = Box::from_raw(tree as *mut Tree);
        rayon::join(
            move || f(&mut *Box::into_raw(left)),
            move || g(&mut *Box::into_raw(right))
        );
    }
}

fn sequence<L, R>(tree: &mut Tree, f: L, g: R) -> () where
    L: FnOnce(&mut Tree) + Send,
    R: FnOnce(&mut Tree) + Send {
    f(tree);
    g(tree);
}

fn layout(t: &mut Tree) -> () {
    t.traverse1(0);
    t.traverse2(0);
}

fn main() -> () {
    rayon::initialize(rayon::Configuration::default().num_threads(6));

    println!("Generating tree...");
    let mut tree = generate(TREE_SPREAD, TREE_HEIGHT);

    println!("Beginning layout...");
    let time = time::precise_time_ns();
    layout(&mut tree);
    let time = time::precise_time_ns() - time;
    println!("Time: {}ms\n", (time as f64) / 1000000f64);
}
