//! Basic DOM data structures.

use std::collections::{HashMap, HashSet};

pub struct DocumentTree {
    pub document_root: DocumentNode,
}

/// A node of the document tree (a baby DOM tree).
pub struct DocumentNode {
    pub index: NodeIndex, // N.B., meaningless for text!
    pub children: Vec<DocumentNode>,
    pub node_type: NodeType,
}

/// Preorder-assigned node indices.
pub type NodeIndex = u32;

#[derive(Clone, Debug)]
pub struct AttributeMap(HashMap<String, String>);

#[derive(Clone, Debug)]
pub enum NodeType {
    Element(ElementData),
    Text(String),
}

#[derive(Clone, Debug)]
pub struct ElementData {
    pub tag: String,
    pub attributes: AttributeMap,
}

impl DocumentTree {
    pub fn new(root_nodes: Vec<DocumentNode>) -> Self {
        let mut document_root = if root_nodes.len() == 1 {
            root_nodes.into_iter().next().unwrap()
        } else {
            DocumentNode::new_elem("html".to_string(), AttributeMap::default(), root_nodes)
        };
        document_root.number_preorder();
        DocumentTree { document_root }
    }
}

impl DocumentNode {
    /// Construct a text node.
    ///
    /// Note that node indices are assigned only when a complete `DocumentTree`
    /// is created from a root node.
    pub fn new_text(data: String) -> Self {
        DocumentNode {
            index: 0,
            children: vec![],
            node_type: NodeType::Text(data),
        }
    }

    /// Construct an element node, with children already constructed.
    ///
    /// Note that node indices are assigned only when a complete `DocumentTree`
    /// is created from a root node.
    pub fn new_elem(tag_name: String, attr_map: AttributeMap, children: Vec<Self>) -> Self {
        DocumentNode {
            index: 0,
            children: children,
            node_type: NodeType::Element(ElementData {
                tag: tag_name,
                attributes: attr_map,
            }),
        }
    }

    pub fn as_elem(&self) -> Option<&ElementData> {
        match self.node_type {
            NodeType::Element(ref elem) => Some(elem),
            NodeType::Text(_) => None,
        }
    }

    pub fn as_text(&self) -> Option<&str> {
        match self.node_type {
            NodeType::Element(_) => None,
            NodeType::Text(ref s) => Some(s),
        }
    }

    pub fn is_elem(&self) -> bool {
        self.as_elem().is_some()
    }

    pub fn is_text(&self) -> bool {
        self.as_text().is_some()
    }

    pub fn tag(&self) -> Option<&str> {
        self.as_elem().map(|elem| elem.tag.as_ref())
    }

    /// Number each node in the document tree such that assigned node indices
    /// ascend with preorder traversal, also returning the maximal node index.
    ///
    /// Note that this method considers the first document node on which it's
    /// invoked as the tree root, assigning it node index 0.
    fn number_preorder(&mut self) -> NodeIndex {
        let mut i = self.index;
        for child in self.children.iter_mut().filter(|node| node.is_elem()) {
            child.index = i + 1;
            i = child.number_preorder();
        }
        i
    }
}

impl ElementData {
    /// Lookup this document element's ID.
    pub fn id(&self) -> Option<&str> {
        self.attributes.lookup("id")
    }

    /// Lookup this document element's class set.
    pub fn classes(&self) -> HashSet<&str> {
        match self.attributes.lookup("class") {
            Some(class_list) => class_list.split(' ').collect(),
            None => HashSet::new(),
        }
    }
}

impl AttributeMap {
    pub fn new(attr_map: HashMap<String, String>) -> Self {
        AttributeMap(attr_map)
    }

    pub fn lookup(&self, attribute: &str) -> Option<&str> {
        self.0.get(attribute).map(String::as_str)
    }
}

impl Default for AttributeMap {
    fn default() -> Self {
        AttributeMap(HashMap::default())
    }
}
