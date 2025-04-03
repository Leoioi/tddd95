use std::{
    fs,
    io::{self, Read},
    mem::swap,
    result,
};


/*
Author: Leo Jarhede 
LiuID: leoja464
*/


// Union-find functions

fn is_same(parents: &mut Vec<usize>, a: usize, b: usize) -> bool {
    find(a, parents) == find(b, parents)
}

fn find(x: usize, parents: &mut Vec<usize>) -> usize {
    if parents[x] != x {
        // Set the parent of this node to the parent of my parent, thereby flatting the tree.
        parents[x] = find(parents[x], parents);
    }
    return parents[x];
}

fn union(x: usize, y: usize, parents: &mut Vec<usize>, sizes: &mut Vec<usize>) {
    let x_root: usize = find(x, parents);
    let y_root: usize = find(y, parents);

    if x_root == y_root {
        return;
    }

    // If the size of the root for the x tree is smaller then add that tree to the y tree,
    if sizes[x_root] < sizes[y_root] {
        parents[x_root] = y_root;
        sizes[y_root] += sizes[x_root];
    } else {
        parents[y_root] = x_root;
        sizes[x_root] += sizes[y_root];
    }
}



fn main() {
    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let mut result = String::new();

    loop {
        let n = next() as usize;
        let m = next() as usize;

        if n == 0 && m == 0 {
            break;
        }

        let mut edges: Vec<(usize, usize, i64)> = vec![];

        for _ in 0..m {
            edges.push((next() as usize, next() as usize, next()));
        }

        let mut tree_edges = mst(edges, n);

        // If the number of edges in the tree is not equal to then number of nodes - 1 then we know
        // that it is not possible to create a single spanning tree
        if tree_edges.len() != n - 1 {
            result.push_str("Impossible\n");
        } else {
            tree_edges.iter_mut().for_each(|e| {
                if e.0 > e.1 {
                    swap(&mut e.0, &mut e.1);
                }
            });
            // Here we (apparently) need to do some special sorting to get the output to the form
            // that kattis expectes.
            tree_edges.sort();

            result.push_str(&((tree_edges.iter().fold(0, |acc, x| acc + x.2)).to_string() + "\n"));
            for t_edge in tree_edges {
                result.push_str(&(t_edge.0.to_string() + " " + &t_edge.1.to_string() + "\n"));
            }
        }
    }

    print!("{}", result);
}


/*
 * This is an implementation of Kruskal's algorithm which will find the minimum spanning tree given
 * some graph defined as a list of edges and thier assosiated cost. 
 * We also utilize union find to check of some nodes belong to the same set.
 * This funktion also needs the number of edges in the graph to define parents and sizes vectors
 * needed for this union find.
 *
 * The algorithm simply works by sorting all of the edges by thire assosiated cost placing the edge
 * with the lowest cost first in the list. The we define as many sets as there are nodes each of
 * these set will contain a node. 
 * Then we iterate over these sorted edges adding them to the restuling tree edges and unifiying
 * thier sets, as long as both
 * of the end edges are not both in already in the tree. 
 * To check this last requierment we use union find. If the end points belong to the same set we
 * know that end points also both are in the same tree and as such we dont include it.
 *
 * Complexity:
 * The complexity of this funktion can be broken down in parts.
 * First we perform a sorting operation over all of the edges O(e log e) where e is the number of
 * edges.
 * As for the second part we are going to iterate over the edges once and for each perfrom a check
 * to see of the two end points are in the same set. Then the complexity can be written as 
 * O(e alpha(v)) where v is the number of nodes and alpha denotes the inverse ackerman function.
 * Then the total complexity is O(e log(e) + e alpha(v)) sense the inverse ackerman grows very very
 * slowly we can rewrite this as just O(e log(e)).
 *
 */
fn mst(mut edges: Vec<(usize, usize, i64)>, n: usize) -> Vec<(usize, usize, i64)> {
    let mut sizes: Vec<usize> = vec![1; n];
    let mut parents: Vec<usize> = (0..n).collect();

    edges.sort_by(|&a, &b| a.2.cmp(&b.2));

    let mut tree_edges: Vec<(usize, usize, i64)> = vec![];

    for i in 0..edges.len() {
        let edge = edges[i];
        if !is_same(&mut parents, edge.0, edge.1) {
            union(edge.0, edge.1, &mut parents, &mut sizes);
            tree_edges.push(edge);
        }
    }

    return tree_edges;
}
