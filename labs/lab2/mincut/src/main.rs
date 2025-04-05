use std::{
    collections::{HashSet, VecDeque}, fs, io::{self, Read}
};

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

#[derive(Debug, Clone, Copy)]
struct Edge {
    to: usize,
    rev: usize,
    cap: i64,
}

fn add_edge(graph: &mut Vec<Vec<Edge>>, from: usize, to: usize, cap: i64) {
    let rev_from = graph[to].len();
    let rev_to = graph[from].len();
    graph[from].push(Edge {
        to,
        rev: rev_from,
        cap,
    });
    graph[to].push(Edge {
        to: from,
        rev: rev_to,
        cap: 0,
    });
}

/*
 *
 */

fn max_flow(graph: &mut Vec<Vec<Edge>>, source: usize, sink: usize) -> i64 {
    let mut flow = 0;
    loop {
        let mut pred = vec![None; graph.len()];
        let mut queue = VecDeque::new();
        queue.push_back(source);
        while let Some(u) = queue.pop_front() {
            for (i, edge) in graph[u].iter().enumerate() {
                if pred[edge.to].is_none() && edge.cap > 0 && edge.to != source {
                    pred[edge.to] = Some((u, i));
                    if edge.to == sink {
                        break;
                    }
                    queue.push_back(edge.to);
                }
            }
        }

        if let Some((u, _)) = pred[sink] {
            let mut df = i64::MAX;
            let mut v = sink;
            while v != source {
                let (u, i) = pred[v].unwrap();
                df = df.min(graph[u][i].cap);
                v = u;
            }

            v = sink;
            while v != source {
                let (u, i) = pred[v].unwrap();
                graph[u][i].cap -= df;
                let rev = graph[u][i].rev;
                graph[v][rev].cap += df;
                v = u;
            }

            flow += df;
        } else {
            break flow;
        }
    }
}

fn main() {
    // let file_path = "mincut.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let n = next() as usize; 
    let m = next() as usize; 
    let s = next() as usize;
    let t = next() as usize;

    let mut graph: Vec<Vec<Edge>> = vec![vec![]; n];

    for _ in 0..m {
        let u = next() as usize;
        let v = next() as usize;
        let w = next();
        add_edge(&mut graph, u, v, w);
    }

    let _ =  max_flow(&mut graph, s, t);

    let mut stack: Vec<usize> = vec![];
    stack.push(s);
    let mut found_nodes: HashSet<usize> = HashSet::new();
    found_nodes.insert(s);

    while let Some(top_node) = stack.pop() {
        let neighbors = &graph[top_node];    
        for n_edge in neighbors {
            if !found_nodes.contains(&n_edge.to) && n_edge.cap > 0 {
                found_nodes.insert(n_edge.to);
                stack.push(n_edge.to);
            }
        }
    }

    println!("{:?}", found_nodes.len());
    for node in found_nodes {
        println!("{:?}", node);
    }




}
