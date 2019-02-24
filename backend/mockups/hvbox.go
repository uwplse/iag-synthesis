package main

import (
  "fmt"
  "sync"
  "time"
  "math/rand"
)
//import "github.com/fogleman/gg"

const CUTOFF = 3

const TREE_SPREAD = 4
const TREE_HEIGHT = 14

const MAX_WIDTH = 100
const MAX_HEIGHT = 100

type Top struct {}

type TopNode interface {
    public() *Top
    traverse_1(int)
    traverse_2(int)
}

type Root struct {
    fields Top
    root HVBoxNode
}

func (self *Root) public() *Top {
    return &self.fields
}

type HVBox struct {
    width, width_out, height, height_out int
    right, bottom int
    visible bool
}

type HVBoxNode interface {
    public() *HVBox
    traverse_1(int)
    traverse_2(int)
}

type HBox struct {
    fields HVBox
    children []HVBoxNode
}

func (self *HBox) public() *HVBox {
    return &self.fields
}

type VBox struct {
    fields HVBox
    children []HVBoxNode
}

func (self *VBox) public() *HVBox {
    return &self.fields
}

type Leaf struct {
    fields HVBox
    width_in, height_in int
}

func (self *Leaf) public() *HVBox {
    return &self.fields
}

type Tree TopNode

// Visitors for the first, post-order traversal pass

func (self *Root) traverse_1(d int) {
    self.root.traverse_1(d)
}

func (self *HBox) traverse_1(d int) {
    if d < CUTOFF {
        var wg sync.WaitGroup
        for i := range self.children {
            wg.Add(1)
            child := self.children[i]
            go func () { child.traverse_1(d + 1); wg.Done() }()
        }
        wg.Wait()
    } else {
        for _, child := range self.children {
            child.traverse_1(d + 1)
        }
    }
    width_out := 0
    height_out := 0
    for _, child := range self.children {
        fields := child.public()
        fields.width_out = width_out + fields.width_out
        if height_out > fields.height {
            fields.height_out = height_out
        } else {
            fields.height_out = fields.height
        }
        width_out = fields.width_out
        height_out = fields.height_out
    }
    if self.fields.visible {
        self.fields.width = width_out
        self.fields.height = height_out
    } else {
        self.fields.width = 0
        self.fields.height = 0
    }
}

func (self *VBox) traverse_1(d int) {
    if d < CUTOFF {
        var wg sync.WaitGroup
        for i := range self.children {
            wg.Add(1)
            child := self.children[i]
            go func () { child.traverse_1(d + 1); wg.Done() }()
        }
        wg.Wait()
    } else {
        for _, child := range self.children {
            child.traverse_1(d + 1)
        }
    }
    width_out := 0
    height_out := 0
    for _, child := range self.children {
        fields := child.public()
        if width_out > fields.width {
            fields.width_out = width_out
        } else {
            fields.width_out = fields.width
        }
        fields.height_out = height_out + fields.height_out
        width_out = fields.width_out
        height_out = fields.height_out
    }
    if self.fields.visible {
        self.fields.width = width_out
        self.fields.height = height_out
    } else {
        self.fields.width = 0
        self.fields.height = 0
    }
}

func (self *Leaf) traverse_1(d int) {
    self.fields.width = self.width_in
    self.fields.height = self.height_in
}

// Visitors for the second, pre-order traversal pass

func (self *Root) traverse_2(d int) {
    fields := self.root.public()
    fields.right = fields.width
    fields.bottom = fields.height
    fields.width_out = fields.width
    fields.height_out = fields.height
    self.root.traverse_2(d)
}

func (self *HBox) traverse_2(d int) {
    right := self.fields.right - self.fields.width
    for _, child := range self.children {
        fields := child.public()
        fields.right = right + fields.width
        fields.bottom = self.fields.bottom
        right = fields.right
    }
    if d < CUTOFF {
        var wg sync.WaitGroup
        for i := range self.children {
            wg.Add(1)
            child := self.children[i]
            go func () { child.traverse_2(d + 1); wg.Done() }()
        }
        wg.Wait()
    } else {
        for _, child := range self.children {
            child.traverse_2(d + 1)
        }
    }
}

func (self *VBox) traverse_2(d int) {
    bottom := self.fields.bottom
    for _, child := range self.children {
        fields := child.public()
        fields.right = self.fields.right
        fields.bottom = bottom + fields.height
        bottom = fields.bottom
    }
    if d < CUTOFF {
        var wg sync.WaitGroup
        for i := range self.children {
            wg.Add(1)
            child := self.children[i]
            go func () { child.traverse_2(d + 1); wg.Done() }()
        }
        wg.Wait()
    } else {
        for _, child := range self.children {
            child.traverse_2(d + 1)
        }
    }
}

func (self *Leaf) traverse_2(d int) { }

// Tree generation

func generate_top() Top {
    return Top {}
}

func generate_root(w, h int) TopNode {
    return &Root { generate_top(), generate_hbox(w, h - 1) }
}

func generate_hvbox() HVBox {
    return HVBox { visible: true }
}

func generate_hbox(w, h int) HVBoxNode {
    children := make([]HVBoxNode, w)
    ch := make(chan bool)
    go func () {
        for i := range children {
            if h == 1 {
                children[i] = generate_leaf()
            } else {
                children[i] = generate_vbox(w, h - 1)
            }
        }
        ch <- true
    }()
    <-ch
    return &HBox { generate_hvbox(), children }
}

func generate_vbox(w, h int) HVBoxNode {
    children := make([]HVBoxNode, w)
    ch := make(chan bool)
    go func () {
        for i := range children {
            if h == 1 {
                children[i] = generate_leaf()
            } else {
                children[i] = generate_vbox(w, h - 1)
            }
        }
        ch <- true
    }()
    <-ch
    return &VBox { generate_hvbox(), children }
}

func generate_leaf() HVBoxNode {
    return &Leaf {
        generate_hvbox(),
        rand.Intn(MAX_WIDTH),
        rand.Intn(MAX_HEIGHT),
    }
}

func generate(w, h int) Tree {
    t := generate_root(w, h)
    return t
}

func layout(t Tree) {
    t.traverse_1(0)
    t.traverse_2(0)
}

func main() {
    fmt.Println("Generating tree...")
    t := generate(TREE_SPREAD, TREE_HEIGHT)
    fmt.Println("Beginning layout...")
    now := time.Now()
    layout(t)
    fmt.Printf("Time: %v\n", time.Since(now))
}
