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

struct IRoot {
    width: int,
    height: int,
    minTurnout: int,
    maxTurnout: int,
    showFraud: int,
    showProjected: float,
    votesUR: int,
    fixWidth: int,
    showJavascript: int,
    totalMag: int
}

struct Root {
    public: Top,
    child: Box<Class<Node>>
}

unsafe impl Send for Root { }

unsafe impl Sync for Root { }

impl Class<IRoot> for Root {
    fn interface(&mut self) -> &mut IRootAG { &mut self.public }

    fn traverse1(&mut self, depth: u32) -> () { }

    fn traverse2(&mut self, depth: u32) -> () { }
}

struct Node {
    totalMag: int,
    minTurnout: int,
    maxTurnout: int,
    votesUR: int,
    showFraud: int,
    showProjected: float,
    fixWidth: int,
    render: int,
    showJavascript: int,
    w: int,
    h: int,
    x: int,
    y: int,
    rx: int,
    by: int
}

struct CountryContainer {
    public: Node,
    children: Vec<Box<Class<Node>>>
}

unsafe impl Send for CountryContainer { }

unsafe impl Sync for CountryContainer { }

struct Region {
    public: Node,
    children: Vec<Box<Class<Node>>>
}

unsafe impl Send for Region { }

unsafe impl Sync for Region { }

struct District {
    public: Node,
    children: Vec<Box<Class<Node>>>
}

unsafe impl Send for District { }

unsafe impl Sync for District { }

struct VSquare {
    public: Node,
    children: Vec<Box<Class<Node>>>
}

unsafe impl Send for VSquare { }

unsafe impl Sync for VSquare { }

struct HSquare {
    public: Node,
    children: Vec<Box<Class<Node>>>
}

unsafe impl Send for HSquare { }

unsafe impl Sync for HSquare { }

struct PollingPlace {
    public: Node,
        totalVotes: int,
        totalVotesUR: int,
        urVotes: int,
        urVotesProjected: int,
        turnout: int,
        defColor: int,
        urColor: int,
        fraudColor: int,
        inJavascript: int,
        calcRegularColor: int,
        calcFraudColor: int.
        calcProjectedColor: int,
        calcVotesColor: int,
        magnitude: int
}

unsafe impl Send for PollingPlace { }

unsafe impl Sync for PollingPlace { }

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
