use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read},
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "suffixsorting.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

}

struct SuffixNode {
    child: Vec<SuffixNode>, 
    start: usize,
    end: usize,
    index: usize,
    input: Vec<char>, 
}

impl SuffixNode {
    pub fn new(start: usize, end: usize) -> Self {
        Self {child: vec![], start: start, end: end, index: 0 } // Is this index correct 
    }
}

struct Active {
    active_length: usize,
    active_node: SuffixNode,
    active_edge: i32, 
}
impl Active {
    pub fn new(node: SuffixNode) -> Self {
        Self {active_length: 0, active_node: node, active_edge: -1} // Is this index correct 
    }

struct SuffixTree {
    root: SuffixNode,
    active: Active,
    remaining: usize,
}

fn select_node_index(tree: SuffixTree, index: usize) -> Option<SuffixNode>{
    tree.active_node.child.get(tree.input[index])
}

fn select_node(tree: SuffixTree, index: usize) -> Option<SuffixNode> {
    tree.active_node.child.get(tree.input[tree.active.active_edge])
}

fn build(tree: SuffixTree) {
    let mut root = SuffixNode::new(1, 0);
    root.index = -1;
    let mut active = Active::new(root);
    tree.end = -1;

    for i in 0..tree.input.len() {
        start_phase(tree, i);
    }
}


fn nextChar (tree: SuffixTree, i: usize) {
    let node = select_node();
    if 
}


fn start_phase (tree: SuffixTree, i: usize) {
    let mut lastCreatedInternalNode: Option<SuffixNode> = None;

    tree.end += 1;

    tree.remaining += 1;

    while tree.remaining > 0 {
        if tree.active_length == 0 {
            if let Some(selected_node) = select_node(i) {
                tree.active.active_edge = selected_node.start;
                tree.active.active_length += 1;
                break;
            }
            else {
                tree.root.child[tree.input[i]] = SuffixNode::new(i, end);
                tree.remaining -= 1;
            }
        }
        else {

        }
    }


    

}
