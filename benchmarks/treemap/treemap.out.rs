extern crate crossbeam;

use std::cell::UnsafeCell;
use std::time::Instant;

use crossbeam::thread::*;

type int = i32;
type float = f32;


pub struct IRoot {
    width: int,
    height: int,
    minTurnout: int,
    maxTurnout: int,
    showFraud: int,
    showProjected: int,
    votesUR: int,
    fixWidth: int,
    showJavascript: int,
    totalMag: int,
    class: IRootKind
}

pub enum IRootKind {
    IsRoot(Root)
}
use IRootKind::*;

pub struct Root {
    child: Box<Node>
}


pub struct Node {
    totalMag: int,
    minTurnout: int,
    maxTurnout: int,
    votesUR: int,
    showFraud: int,
    showProjected: int,
    fixWidth: int,
    render: int,
    showJavascript: int,
    w: int,
    h: int,
    x: int,
    rx: int,
    y: int,
    by: int,
    class: NodeKind
}

pub enum NodeKind {
    IsCountryContainer(CountryContainer),
    IsRegion(Region),
    IsDistrict(District),
    IsVSquare(VSquare),
    IsHSquare(HSquare),
    IsPollingPlace(PollingPlace)
}
use NodeKind::*;

pub struct CountryContainer {
    childs: Vec<Node>
}

pub struct Region {
    childs: Vec<Node>
}

pub struct District {
    childs: Vec<Node>
}

pub struct VSquare {
    childs: Vec<Node>
}

pub struct HSquare {
    childs: Vec<Node>
}

pub struct PollingPlace {
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
    calcFraudColor: int,
    calcProjectedColor: int,
    calcVotesColor: int,
    magnitude: int
}
impl IRoot {
    pub fn pre0(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.child.maxTurnout = self.maxTurnout;
                class.child.showFraud = self.showFraud;
                class.child.minTurnout = self.minTurnout;
                class.child.showJavascript = self.showJavascript;
                class.child.showProjected = self.showProjected;
                class.child.pre0();
            }
        }
    }
}

impl Node {
    pub fn pre0(&mut self) -> () {
        match &mut self.class {
            IsCountryContainer(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.showJavascript = self.showJavascript;
                    childs_cur.showProjected = self.showProjected;
                    childs_cur.maxTurnout = self.maxTurnout;
                    childs_cur.minTurnout = self.minTurnout;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre0();
                }
            },
            IsRegion(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.maxTurnout = self.maxTurnout;
                    childs_cur.minTurnout = self.minTurnout;
                    childs_cur.showJavascript = self.showJavascript;
                    childs_cur.showProjected = self.showProjected;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre0();
                }
            },
            IsDistrict(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.maxTurnout = self.maxTurnout;
                    childs_cur.showProjected = self.showProjected;
                    childs_cur.minTurnout = self.minTurnout;
                    childs_cur.showJavascript = self.showJavascript;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre0();
                }
            },
            IsVSquare(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.minTurnout = self.minTurnout;
                    childs_cur.maxTurnout = self.maxTurnout;
                    childs_cur.showJavascript = self.showJavascript;
                    childs_cur.showProjected = self.showProjected;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre0();
                }
            },
            IsHSquare(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.showProjected = self.showProjected;
                    childs_cur.minTurnout = self.minTurnout;
                    childs_cur.maxTurnout = self.maxTurnout;
                    childs_cur.showJavascript = self.showJavascript;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre0();
                }
            },
            IsPollingPlace(ref mut class) => {
                class.magnitude = if ((class.turnout > self.minTurnout) && (class.turnout <= self.maxTurnout)) { class.totalVotes } else { 0 };
                self.totalMag = if (class.inJavascript != 0) { class.magnitude } else { (self.showJavascript * class.magnitude) };
                self.votesUR = if ((class.turnout > self.minTurnout) && (class.turnout <= self.maxTurnout)) { if (class.turnout > 0) { ((class.totalVotesUR * (1 - self.showProjected)) + ((class.totalVotes * class.urVotesProjected) * self.showProjected)) } else { class.totalVotesUR } } else { 0 };
                class.calcProjectedColor = (class.defColor + (class.urColor + class.urVotesProjected));
                class.calcRegularColor = (class.defColor + (class.urColor + class.urVotes));
            }
        }
    }
}

impl IRoot {
    pub fn post0(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.child.post0();
                class.child.by = self.height;
                class.child.rx = self.width;
                self.totalMag = class.child.totalMag;
                class.child.fixWidth = self.fixWidth;
            }
        }
    }
}

impl Node {
    pub fn post0(&mut self) -> () {
        match &mut self.class {
            IsCountryContainer(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post0();
                }
                let mut votesUR_acc = 0;
                let mut totalMag_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    self.votesUR = (votesUR_acc + childs_cur.votesUR);
                    self.totalMag = (totalMag_acc + childs_cur.totalMag);
                    votesUR_acc = self.votesUR;
                    totalMag_acc = self.totalMag;
                }
                self.votesUR = votesUR_acc;
                self.totalMag = totalMag_acc;
            },
            IsRegion(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post0();
                }
                self.render = 0;
                let mut totalMag_acc = 0;
                let mut votesUR_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    self.totalMag = (totalMag_acc + childs_cur.totalMag);
                    self.votesUR = (votesUR_acc + childs_cur.votesUR);
                    totalMag_acc = self.totalMag;
                    votesUR_acc = self.votesUR;
                }
                self.totalMag = totalMag_acc;
                self.votesUR = votesUR_acc;
            },
            IsDistrict(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post0();
                }
                let mut votesUR_acc = 0;
                let mut totalMag_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    self.votesUR = (votesUR_acc + childs_cur.votesUR);
                    self.totalMag = (totalMag_acc + childs_cur.totalMag);
                    votesUR_acc = self.votesUR;
                    totalMag_acc = self.totalMag;
                }
                self.votesUR = votesUR_acc;
                self.totalMag = totalMag_acc;
            },
            IsVSquare(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post0();
                }
                let mut totalMag_acc = 0;
                let mut votesUR_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    self.totalMag = (totalMag_acc + childs_cur.totalMag);
                    self.votesUR = (votesUR_acc + childs_cur.votesUR);
                    totalMag_acc = self.totalMag;
                    votesUR_acc = self.votesUR;
                }
                self.totalMag = totalMag_acc;
                self.votesUR = votesUR_acc;
            },
            IsHSquare(ref mut class) => {
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.post0();
                }
                let mut votesUR_acc = 0;
                let mut totalMag_acc = 0;
                for childs_cur in class.childs.iter_mut() {
                    self.votesUR = (votesUR_acc + childs_cur.votesUR);
                    self.totalMag = (totalMag_acc + childs_cur.totalMag);
                    votesUR_acc = self.votesUR;
                    totalMag_acc = self.totalMag;
                }
                self.votesUR = votesUR_acc;
                self.totalMag = totalMag_acc;
            },
            IsPollingPlace(ref mut class) => {}
        }
    }
}

impl IRoot {
    pub fn pre1(&mut self) -> () {
        match &mut self.class {
            IsRoot(ref mut class) => {
                class.child.w = if (self.fixWidth != 0) { self.width } else { (self.width * (self.totalMag / 63895164)) };
                class.child.h = (self.height * (self.totalMag / 63895164));
                self.votesUR = (class.child.votesUR / self.totalMag);
                class.child.pre1();
            }
        }
    }
}

impl Node {
    pub fn pre1(&mut self) -> () {
        match &mut self.class {
            IsCountryContainer(ref mut class) => {
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                self.render = if (self.fixWidth != 0) { (self.x + (self.y + (self.w + self.h))) } else { 0 };
                let mut rx_acc = self.x;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.h = self.h;
                    childs_cur.showFraud = self.showFraud;
                    childs_cur.w = ((childs_cur.totalMag / self.totalMag) * self.w);
                    childs_cur.rx = (rx_acc + childs_cur.w);
                    childs_cur.by = (self.y + self.h);
                    childs_cur.fixWidth = self.fixWidth;
                    rx_acc = childs_cur.rx;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre1();
                }
            },
            IsRegion(ref mut class) => {
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                let mut by_acc = self.y;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.fixWidth = self.fixWidth;
                    childs_cur.h = ((childs_cur.totalMag / self.totalMag) * self.h);
                    childs_cur.rx = (self.x + self.w);
                    childs_cur.showFraud = self.showFraud;
                    childs_cur.by = (by_acc + childs_cur.h);
                    childs_cur.w = self.w;
                    by_acc = childs_cur.by;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre1();
                }
            },
            IsDistrict(ref mut class) => {
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                self.render = if (self.fixWidth != 0) { (self.x + (self.y + (self.w + self.h))) } else { 0 };
                let mut rx_acc = self.x;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.h = self.h;
                    childs_cur.showFraud = self.showFraud;
                    childs_cur.fixWidth = self.fixWidth;
                    childs_cur.by = (self.y + self.h);
                    childs_cur.w = ((childs_cur.totalMag / self.totalMag) * self.w);
                    childs_cur.rx = (rx_acc + childs_cur.w);
                    rx_acc = childs_cur.rx;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre1();
                }
            },
            IsVSquare(ref mut class) => {
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                self.render = 0;
                let mut by_acc = self.y;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.showFraud = self.showFraud;
                    childs_cur.rx = (self.x + self.w);
                    childs_cur.fixWidth = self.fixWidth;
                    childs_cur.h = ((childs_cur.totalMag / self.totalMag) * self.h);
                    childs_cur.by = (by_acc + childs_cur.h);
                    childs_cur.w = self.w;
                    by_acc = childs_cur.by;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre1();
                }
            },
            IsHSquare(ref mut class) => {
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                self.render = 0;
                let mut rx_acc = self.x;
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.showFraud = self.showFraud;
                    childs_cur.h = self.h;
                    childs_cur.fixWidth = self.fixWidth;
                    childs_cur.w = ((childs_cur.totalMag / self.totalMag) * self.w);
                    childs_cur.rx = (rx_acc + childs_cur.w);
                    childs_cur.by = (self.y + self.h);
                    rx_acc = childs_cur.rx;
                }
                for childs_cur in class.childs.iter_mut() {
                    childs_cur.pre1();
                }
            },
            IsPollingPlace(ref mut class) => {
                class.calcVotesColor = if (class.turnout > 0) { (class.calcRegularColor + (class.calcProjectedColor + self.showProjected)) } else { class.calcRegularColor };
                class.calcFraudColor = if (class.turnout > 0) { class.fraudColor } else { class.calcVotesColor };
                self.y = (self.by - self.h);
                self.x = (self.rx - self.w);
                self.render = (self.x + (self.y + (self.w + (self.h + (class.calcVotesColor + (class.calcFraudColor + self.showFraud))))));
            }
        }
    }
}

pub fn evaluate(tree: IRoot) -> () {
    let cell = UnsafeCell::new(tree);
    let tree = unsafe { cell.get().as_mut().unwrap() };
    tree.pre0();
    tree.post0();
    tree.pre1();
}
