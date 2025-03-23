use std::{fs, io::{self, Read}, u16};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "fenwick.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_>= content.lines();
    let mut results: String = String::new();

    let first_line: &str = lines.next().unwrap(); // Get the first line of the file 

    let (n, _q): (usize, usize) = {
        let mut words = first_line.split_whitespace();
        (words.next().unwrap().parse().unwrap(),
        words.next().unwrap().parse().unwrap())
    };

    let mut fen_tree: Fenwick = init_fewick(n);

    for line in lines {
        // If the line starts with + we can be sure that it is a update operation
        if line.starts_with("+ ") {
            let mut words = line.split_whitespace().skip(1);
            let index: usize = words.next().unwrap().parse().unwrap();
            let value: i32 = words.next().unwrap().parse().unwrap();   
            fen_tree.update(index, value);
        }
        // Otherwise we know we want to query the tree
        else {
            let mut words = line.split_whitespace().skip(1);
            let index: usize = words.next().unwrap().parse().unwrap();
            let value: i64 = fen_tree.query(index);
            results.push_str(&(value.to_string() + "\n"));
        }
    }

    println!("{}", results);

}


fn init_fewick(n: usize) -> Fenwick{
    let mut tree :Vec<i64> = Vec::new();
    tree.resize(n, 0); // Fill the tree with zeros 
    Fenwick {size: n, tree: tree}
}
struct Fenwick {
    size: usize,
    tree: Vec<i64>,
}

/*
Note that this particular implementation of the Fenwick tree uses 0 based indexing 
 */
impl Fenwick {
    /*
    This function will perform an update of the Fenwick tree, i.e add the diff to the entry with with the provided index
    We are going to start by applying the diff to the index in question then we are going to propagate this change up 
    through the tree until we reach an index that is outside of the tree.

    The way that we chose the next index to propagate to is with "index | (index + 1)".
    What this will do is flip the least significate 0 bit, this will in our representation be the next aggregate element in the tree  

    -- Complexity 
    As the "index | (index + 1)" operation is always going to flip one bit we would expect it to take a maximum of log_2 n iterations,
    where n is the size of the tree.
    As such the complexity is O(log n)
     */
    fn update (&mut self, mut index: usize, diff: i32) {
        while index < self.size {
            // Adjust the value of node at index with diff
            self.tree[index] += diff as i64;
            // Jump to the next aggregate node
            index = index | (index + 1); 
        }
    }
    /*
    This function will perform a query of the Fenwick tree, this means that we will take the sum of the largest aggregate elements
    that are less then the requested index, thereby producing the total sum up to the index.

    -- Complexity 
    To find the next aggregate element in the tree we use the bit operation of "(index & (index + 1)) - 1"
    This has the effect of jumping up one level in the tree and as the tree will at most have a depth of 
    log_2 n (where n is again the size of the tree) the complexity of this function will O(log n)
     */
    fn query (&self, index: usize) -> i64 {
        let mut result: i64 = 0;
        let mut index = (index) as isize;
        index =  index - 1;
        while index >= 0 {
            // Add sum of the node to the result
            result += self.tree[index as usize] as i64;

            // Jump to next aggregate node 
            index = (index & (index + 1)) - 1;
        }
        result // Return the result
    }
}

