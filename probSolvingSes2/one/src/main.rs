use std::{collections::VecDeque, fs, io::{self, Read}, vec};

fn main() {
    let file_path = "one.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut content = String::new();
    // io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let mut result = String::new();

    let n = next() as usize;
    let m = next() as usize;

    let mut adj_edges: Vec<Vec<usize>> = vec![vec![]; n+1 ];
    let mut belledges: Vec<((usize, usize), i32)> = vec![];

    for _ in 0..m { 
        let s = next() as usize;
        let t =  next() as usize;
        adj_edges[s].push(t);
        belledges.push(((s,t), -1));
    }


    let (distances, _) = bellman_ford((0..=n).collect(), belledges, 1);

    if distances[2].is_err() {
        println!("inf");
        return;
    }

    let mut visited: Vec<i8> = vec![0; n + 1];
    visited[2] = 1;

    let mut pred: Vec<usize> = vec![0; n + 1];

    let mut queue: Vec<usize> = Vec::new();
    queue.push(1);

    let mut tot = 0;

    while let Some(town) = queue.pop() {
        if visited[town] == 1 {
            let mut cur = town;
            tot += 1;
            while cur != 1 {
                visited[cur] = 1;
                cur = pred[cur];
            }
        }
        else {
            for nib in adj_edges[town].clone() {
                // if visited[nib] {
                //     tot += 1;
                //     continue;
                // }
                pred[nib] = town;
                queue.push(nib);
                //visited[nib] = true;
            }
        }
    } 

    println!("{:?}", tot);
}


#[derive(Debug, Clone)]
enum NodeErrors {
    Unreachable,
    NegInfinite,
}

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
                predecessor[v] = Some(u);
            } else if let Ok(distance) = distances[u] {
                let current = distances[v].clone().unwrap_or(i32::MAX);
                if distance + w < current {
                    predecessor[v] = Some(u);
                    distances[v] = Err(NodeErrors::NegInfinite);
                }
            }
        }
    }
    return (distances, predecessor);
}
