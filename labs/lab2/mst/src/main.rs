use std::{
    fs, io::{self, Read}, mem::swap, result
};

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

        let mut sizes: Vec<usize> = vec![1; n];
        let mut parents: Vec<usize> = (0..n).collect();

        for _ in 0..m {
            edges.push((next() as usize, next() as usize, next()));
        }

        edges.sort_by(|&a, &b| a.2.cmp(&b.2));

        let mut tree_edges: Vec<(usize, usize, i64)> = vec![];

        for i in 0..edges.len() {
            let edge = edges[i];
            if !is_same(&mut parents, edge.0, edge.1) {
                union(edge.0, edge.1, &mut parents, &mut sizes);
                tree_edges.push(edge);
            }
        }

        if tree_edges.len() != n - 1 {
            result.push_str("Impossible\n");
        } else {
            tree_edges.iter_mut().for_each(|e| if e.0 > e.1 { swap(&mut e.0, &mut e.1);});
            tree_edges.sort();

            result.push_str(&((tree_edges.iter().fold(0, |acc, x| acc + x.2)).to_string() + "\n"));
            for t_edge in tree_edges {
                result.push_str(&(t_edge.0.to_string() + " " + &t_edge.1.to_string() + "\n"));
            }
        }
    }

    print!("{}", result);
}
