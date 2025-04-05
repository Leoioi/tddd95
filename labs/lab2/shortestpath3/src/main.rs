use std::{
    fs, i32,
    io::{self, Read},
    iter, usize, vec,
};

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "shortestpath3.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");

    let mut lines: std::str::Lines<'_> = content.lines();

    loop {
        let nmqs: Vec<usize> = lines
            .by_ref()
            .next()
            .unwrap()
            .split_whitespace()
            .map(|num| num.parse::<usize>().unwrap())
            .collect();
        let (n, m, q, s) = (nmqs[0], nmqs[1], nmqs[2], nmqs[3]); // number of nodes, number of edges number of queries  index of starting node
                                                                 
        if n == 0 && m == 0 && q == 0 && s == 0 {
            break;
        }

        let nodes = (0..n).collect();
        let edges: Vec<((usize, usize), i32)> = (0..m)
            .map(|_| {
                let edge_line: Vec<i32> = lines
                    .by_ref()
                    .next()
                    .unwrap()
                    .split_whitespace()
                    .map(|n| n.parse::<i32>().unwrap())
                    .collect();

                ((edge_line[0] as usize, edge_line[1] as usize), edge_line[2])
            })
            .collect();

        let (distances, predecessor) = bellman_ford(nodes, edges, s);

        (0..q).for_each(|_| {
            match distances[lines.by_ref().next().unwrap().parse::<usize>().unwrap()] {
                Ok(dist) => {
                    //println!("{:?}", predecessor);
                    println!("{:?}", dist);
                }
                Err(NodeErrors::Unreachable) => println!("Impossible"),
                Err(NodeErrors::NegInfinite) => println!("-Infinity"),
            }
        });
    }
}

#[derive(Debug, Clone)]
enum NodeErrors {
    Unreachable,
    NegInfinite,
}
/*
This is an implementation of the bellman_ford algorithm. It will compute the costs (and path) from the starting node
to all other nodes in the graph. Unlike dijkstra this algorithm can compute path cost in graphs with negative wights.
This implementation will also detect what nodes have a infinite negative wight by detecting negative cycles.

The algorithms is separated into two steps,
Firstly we perform relaxation. In this step we iterate over each of node and pick the shortest path for each of the neighbors.
At this step we should have the optimal cost on each node assuming that there is no negative paths.

Secondly we detect negative cycle. In this step we try to iterate over the nodes again, if there are still some more optimal path
we know that there must be a negative path somewhere. We then mark the nodes in the cycle as 'NodeErrors::NegInfinite' then also branch
out to see which nodes are reachable by nodes in the negative cycle and mark them as well.

-- Time Complexity
As we are iterating over all of the nodes twice and for each node in these we are also iterating over every single edge we can write the time complexity as
O(2* n * m) = O(n * m) where n = is the number of nodes and m is the number of edges.

 */
fn bellman_ford(
    nodes: Vec<usize>,
    edges: Vec<((usize, usize), i32)>,
    start: usize,
) -> (Vec<Result<i32, NodeErrors>>, Vec<Option<usize>>) {
    let mut distances: Vec<Result<i32, NodeErrors>> =
        vec![Err(NodeErrors::Unreachable); nodes.len()];
    let mut predecessor: Vec<Option<usize>> = vec![None; nodes.len()];

    distances[start] = Ok(0);

    for _ in 0..nodes.len() {
        let mut updated = false;
        for &((u, v), w) in &edges {
            if let Ok(d_u) = distances[u] {
                let current = distances[v].clone().unwrap_or(i32::MAX);
                if d_u != i32::MAX && d_u + w < current {
                    predecessor[v] = Some(u);
                    distances[v] = Ok(d_u + w);
                    updated = true;
                }
            }
        }
        if !updated {
            break; // Early exit if no more updates
        }
    }

    for _ in 0..nodes.len() {
        for &((u, v), w) in edges.iter().by_ref() {
            if let Err(NodeErrors::NegInfinite) = distances[u] {
                distances[v] = Err(NodeErrors::NegInfinite);
            } else if let Ok(distance) = distances[u] {
                let current = distances[v].clone().unwrap_or(i32::MAX);
                if distance + w < current {
                    distances[v] = Err(NodeErrors::NegInfinite);
                }
            }
        }
    }
    return (distances, predecessor);
}
