use std::{io::{self, Read}, usize};

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

/*
For this exercise we are trying to efficiently unify and query some set of subsets. 
To do this we are going represent the subsets as trees and use the two functions of 
find and unify to perform the query and unify our subsets.
 */
fn main() {
    // Read from file 
    // let file_path: &str = "unionfind.in";
    // let content: String = fs::read_to_string(file_path).expect("Failed to read file");

    // Read from standard input  
    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    // The final string we are printing to the terminal
    let mut results: String = String::new();

    // The parents vector will for each index contain the index of the parent 
    let mut parents: Vec<usize> = Vec::new();

    // This sizes vector will contain the size of each tree,
    let mut sizes: Vec<usize> = Vec::new();
    
    for line in content.lines() {
        let first_char = line.split_whitespace().next().unwrap();
        // If this parse succeeds it means that the first char is a usize (numeric) and we assume that a new test case has started
        if let Some(set_size) = first_char.parse::<usize>().ok() { 
            sizes.clear(); 
            parents.clear();
            parents = (0..set_size).collect();
            sizes.resize(set_size, 1);
        }

        // Given the first char we determine whether we want to query or join  
        else if first_char == "?" {
            let (a, b ) =  read_entry(line);
            // If it is the case that both nodes have the same parent then we know they are part of the same set
            if is_same(&mut parents, a, b) {
                results.push_str("yes\n");
            }
            else {
                results.push_str("no\n");
            }
        }
        // Perform unification
        else if first_char == "=" {
            let (a, b ) =  read_entry(line);
            union(a, b, &mut parents, &mut sizes);
        }  
        // All cases must be handled in rust, however assuming the input is correct we will never reach this panic
        else {
            panic!("AAAAAAAAAA")
        }
    }
    // Print the result
    print!("{}", results);

}


/*
This function will check whether two nodes belong to the same set by checking
if the two nodes have the same parent.
 */
fn is_same(parents: &mut Vec<usize>, a: usize, b: usize) -> bool {
    find(a, parents) == find(b, parents)
}


/* 
For some node x we want to see what final parent this node has.
As such we are going to recursively go through the parent structure until we find 
some node that is the parent of itself, signifying that it has no parent

-- Complexity 
The time complexity of this function is O(\alpha(n))
where: \alpha denotes is the inverse Ackermann function 
       n is total size of all the sets

Consider that increasing n it will not lead to a bigger search complexity for find.
This is because each time we use find for some node it will flatten the structure of the tree meaning the next 
time we call find on some node the search time will be even smaller.
*/
fn find(x: usize, parents: &mut Vec<usize>) -> usize{
    if parents[x] != x {
        // Set the parent of this node to the parent of my parent, thereby flatting the tree.
        parents[x] = find(parents[x], parents);
    }
    return parents[x];
}

/*
For nodes x and y try to unify in which they are located, if it is the case that they are in the same subset 
do nothing.
If it is the case that the nodes are not part of the same subset we are going to unify them by setting giving 
both sets the same root.
Here we priorities making the trees as small as possible, thereby reducing the search time for the find function.

-- Complexity 
The time complexity of this function is O(\alpha(n))
where: \alpha denotes is the inverse Ackermann function 
       n is total size of all the sets

Consider that the complexity from this function comes only from the find function.
Here we are calling find two times but that still gives the same complexity.
 */
fn union(x: usize, y: usize, parents: &mut Vec<usize>, sizes: &mut Vec<usize>) { 
    let x_root: usize = find(x, parents);
    let y_root: usize = find(y, parents);

    if x_root == y_root{
        return
    }

    // If the size of the root for the x tree is smaller then add that tree to the y tree,  
    if sizes[x_root] < sizes[y_root] {
        parents[x_root] = y_root;
        sizes[y_root] += sizes[x_root];
    }
    else { 
        parents[y_root] = x_root;
        sizes[x_root] += sizes[y_root];
    }
}


/*
Small helper function to parse a line 
 */
fn read_entry (line: &str) -> (usize, usize) {
    let mut numbers = line
    .split_whitespace()
    .skip(1) // Skip "?"
    .filter_map(|s| s.parse::<usize>().ok());

    let (Some(n1), Some(n2)) = (numbers.next(), numbers.next()) else {panic!()};

    return (n1,n2);
}