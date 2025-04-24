use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read},
    cell::RefCell,
    rc::Rc
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


const TOTAL: usize = 256;
const UNIQUE_CHAR: char = '$';

#[derive(Debug)]
struct End {
    end: i32,
}

impl End {
    fn new(end: i32) -> Self {
        End { end }
    }
}

#[derive(Debug)]
struct SuffixNode {
    children: Vec<Option<Rc<RefCell<SuffixNode>>>>,
    start: i32,
    end: Rc<RefCell<End>>,
    index: i32,
    suffix_link: Option<Rc<RefCell<SuffixNode>>>,
}

impl SuffixNode {
    fn new(start: i32, end: Rc<RefCell<End>>) -> Rc<RefCell<Self>> {
        let children = vec![None; TOTAL];
        Rc::new(RefCell::new(SuffixNode {
            children,
            start,
            end,
            index: -1,
            suffix_link: None,
        }))
    }
}


#[derive(Debug)]
struct Active {
    active_node: Rc<RefCell<SuffixNode>>,
    active_edge: i32,
    active_length: i32,
}

impl Active {
    fn new(active_node: Rc<RefCell<SuffixNode>>) -> Self {
        Active {
            active_node,
            active_edge: -1,
            active_length: 0,
        }
    }
}

#[derive(Debug)]
struct SuffixTree {
    root: Rc<RefCell<SuffixNode>>,
    active: Active,
    remaining_suffix_count: i32,
    end: Rc<RefCell<End>>,
    input: Vec<char>,
}

#[derive(Debug)]
struct EndOfPath;

impl SuffixTree {
    fn new(s: &str) -> Self {
        let mut input: Vec<char> = s.chars().collect();
        input.push(UNIQUE_CHAR);
        let dummy_end = Rc::new(RefCell::new(End::new(-1)));
        let root = SuffixNode::new(1, Rc::new(RefCell::new(End::new(0))));
        let active = Active::new(root.clone());
        SuffixTree {
            root,
            active,
            remaining_suffix_count: 0,
            end: dummy_end,
            input,
        }
    }

    fn build(&mut self) {
        self.root.borrow_mut().index = -1;
        self.active = Active::new(self.root.clone());
        self.end.borrow_mut().end = -1;
        for i in 0..self.input.len() {
            self.start_phase(i);
        }
        if self.remaining_suffix_count != 0 {
            println!("Something wrong happened");
        }
        self.set_index_using_dfs(self.root.clone(), 0, self.input.len() as i32);
    }

    fn start_phase(&mut self, i: usize) {
        let mut last_created_internal_node: Option<Rc<RefCell<SuffixNode>>> = None;
        self.end.borrow_mut().end += 1;
        self.remaining_suffix_count += 1;
        while self.remaining_suffix_count > 0 {
            if self.active.active_length == 0 {
                if let Some(node) = self.select_node_with_index(i) {
                    self.active.active_edge = node.borrow().start;
                    self.active.active_length += 1;
                    break;
                } else {
                    let idx = self.input[i] as usize;
                    self.root.borrow_mut().children[idx] = Some(SuffixNode::new(i as i32, self.end.clone()));
                    self.remaining_suffix_count -= 1;
                }
            } else {
                match self.next_char(i) {
                    Ok(ch) => {
                        if ch == self.input[i] {
                            if let Some(ref last) = last_created_internal_node {
                                last.borrow_mut().suffix_link = self.select_node();
                            }
                            self.walk_down(i);
                            break;
                        } else {
                            let node = self.select_node().unwrap();
                            let old_start = node.borrow().start;
                            node.borrow_mut().start += self.active.active_length;
                            let new_internal = SuffixNode::new(
                                old_start,
                                Rc::new(RefCell::new(End::new(old_start + self.active.active_length - 1))),
                            );
                            let new_leaf = SuffixNode::new(i as i32, self.end.clone());
                            let index_char = self.input[(new_internal.borrow().start + self.active.active_length) as usize] as usize;
                            new_internal.borrow_mut().children[index_char] = Some(node.clone());
                            new_internal.borrow_mut().children[self.input[i] as usize] = Some(new_leaf);
                            new_internal.borrow_mut().index = -1;
                            let key = self.input[new_internal.borrow().start as usize] as usize;
                            self.active.active_node.borrow_mut().children[key] = Some(new_internal.clone());
                            if let Some(ref last) = last_created_internal_node {
                                last.borrow_mut().suffix_link = Some(new_internal.clone());
                            }
                            last_created_internal_node = Some(new_internal.clone());
                            new_internal.borrow_mut().suffix_link = Some(self.root.clone());
                            if !Rc::ptr_eq(&self.active.active_node, &self.root) {
                                if let Some(link) = self.active.active_node{
                                    self.active.active_node = link.clone();
                                };
                            } else {
                                self.active.active_edge += 1;
                                self.active.active_length -= 1;
                            }
                            self.remaining_suffix_count -= 1;
                        }
                    }
                    Err(_) => {
                        let node = self.select_node().unwrap();
                        node.borrow_mut().children[self.input[i] as usize] = Some(SuffixNode::new(i as i32, self.end.clone()));
                        if let Some(ref last) = last_created_internal_node {
                            last.borrow_mut().suffix_link = Some(node.clone());
                        }
                        last_created_internal_node = Some(node.clone());
                        if !Rc::ptr_eq(&self.active.active_node, &self.root) {
                            if let Some(ref link) = self.active.active_node.borrow().suffix_link {
                                self.active.active_node = link.clone();
                            }
                        } else {
                            self.active.active_edge += 1;
                            self.active.active_length -= 1;
                        }
                        self.remaining_suffix_count -= 1;
                    }
                }
            }
        }
    }

    fn walk_down(&mut self, i: usize) {
        let node = self.select_node().unwrap();
        if self.diff(&node) < self.active.active_length {
            self.active.active_node = node.clone();
            self.active.active_length -= self.diff(&node);
            let idx = self.input[i] as usize;
            if let Some(child) = self.active.active_node.borrow().children[idx].clone() {
                self.active.active_edge = child.borrow().start;
            }
        } else {
            self.active.active_length += 1;
        }
    }

    fn next_char(&mut self, i: usize) -> Result<char, EndOfPath> {
        let node = self.select_node().unwrap();
        if self.diff(&node) >= self.active.active_length {
            let pos = node.borrow().start + self.active.active_length;
            return Ok(self.input[pos as usize]);
        }
        if self.diff(&node) + 1 == self.active.active_length {
            if node.borrow().children[self.input[i] as usize].is_some() {
                return Ok(self.input[i]);
            }
        } else {
            self.active.active_node = node.clone();
            self.active.active_length = self.active.active_length - self.diff(&node) - 1;
            self.active.active_edge += self.diff(&node) + 1;
            return self.next_char(i);
        }
        Err(EndOfPath)
    }

    fn select_node(&self) -> Option<Rc<RefCell<SuffixNode>>> {
        if self.active.active_edge < 0 {
            return None;
        }
        let idx = self.input[self.active.active_edge as usize] as usize;
        self.active.active_node.borrow().children[idx].clone()
    }

    fn select_node_with_index(&self, i: usize) -> Option<Rc<RefCell<SuffixNode>>> {
        let idx = self.input[i] as usize;
        self.active.active_node.borrow().children[idx].clone()
    }

    fn diff(&self, node: &Rc<RefCell<SuffixNode>>) -> i32 {
        let end_val = node.borrow().end.borrow().end;
        end_val - node.borrow().start
    }

    fn set_index_using_dfs(&self, node: Rc<RefCell<SuffixNode>>, mut val: i32, size: i32) {
        val += node.borrow().end.borrow().end - node.borrow().start + 1;
        if node.borrow().index != -1 {
            node.borrow_mut().index = size - val;
            return;
        }
        for child in &node.borrow().children {
            if let Some(ref child_node) = child {
                self.set_index_using_dfs(child_node.clone(), val, size);
            }
        }
    }

    fn dfs_traversal(&self) {
        let mut result: Vec<char> = Vec::new();
        for child in &self.root.borrow().children {
            if let Some(ref node) = child {
                self.dfs_traversal_rec(node.clone(), &mut result);
            }
        }
    }

    fn dfs_traversal_rec(&self, node: Rc<RefCell<SuffixNode>>, result: &mut Vec<char>) {
        if node.borrow().index != -1 {
            for i in node.borrow().start..=node.borrow().end.borrow().end {
                result.push(self.input[i as usize]);
            }
            for ch in result.iter() {
                print!("{}", ch);
            }
            println!(" {}", node.borrow().index);
            for _ in node.borrow().start..=node.borrow().end.borrow().end {
                result.pop();
            }
            return;
        }
        for i in node.borrow().start..=node.borrow().end.borrow().end {
            result.push(self.input[i as usize]);
        }
        for child in &node.borrow().children {
            if let Some(ref child_node) = child {
                self.dfs_traversal_rec(child_node.clone(), result);
            }
        }
        for _ in node.borrow().start..=node.borrow().end.borrow().end {
            result.pop();
        }
    }
}
