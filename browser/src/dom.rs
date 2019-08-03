//! Basic DOM data structures.

use std::collections::{HashMap,HashSet};

pub type NodeId = i32;
pub type AttrMap = HashMap<String, String>;

#[derive(Debug)]
pub struct Node {
    // data common to all nodes:
    pub number: NodeId,
    pub children: Vec<Node>,

    // data specific to each node type:
    pub node_type: NodeType,
}

#[derive(Debug)]
pub enum NodeType {
    Element(ElementData),
    Text(String),
}

#[derive(Debug)]
pub struct ElementData {
    pub tag: String,
    pub attributes: AttrMap,
}

// Constructor functions for convenience:

pub fn text(no: NodeId, data: String) -> Node {
    Node {
        number: no,
        children: vec![],
        node_type: NodeType::Text(data)
    }
}

pub fn elem(no: NodeId, name: String, attrs: AttrMap, children: Vec<Node>) -> Node {
    Node {
        number: no,
        children: children,
        node_type: NodeType::Element(ElementData {
            tag: name,
            attributes: attrs,
        })
    }
}

// Element methods

impl ElementData {
    pub fn id(&self) -> Option<&String> {
        self.attributes.get("id")
    }

    pub fn classes(&self) -> HashSet<&str> {
        match self.attributes.get("class") {
            Some(classlist) => classlist.split(' ').collect(),
            None => HashSet::new()
        }
    }
}
