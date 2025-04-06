use std::{
    collections::VecDeque,
    fs,
    io::{self, Read},
    usize, vec,
};

#[derive(Debug, Clone, Copy, PartialEq)]
struct Edge {
    cap: i64,
    flow: i64,
    s: usize,
    t: usize,
    rev: usize,
    is_rev: bool,
}

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    // let file_path = "maxflow.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    let nmqs = lines.next().unwrap().split_whitespace().collect::<Vec<_>>();
    let n: usize = nmqs[0].parse().unwrap();
    let m: usize = nmqs[1].parse().unwrap();
    let s: usize = nmqs[2].parse().unwrap();
    let t: usize = nmqs[3].parse().unwrap();

    let mut edges: Vec<Edge> = (0..m)
        .map(|i| {
            let line: Vec<usize> = lines
                .next()
                .unwrap()
                .split_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<usize>>();
            Edge {
                cap: line[2] as i64,
                flow: 0,
                s: line[0],
                t: line[1],
                rev: 0,
                is_rev: false,
            }
        })
        .collect();

    (0..m).for_each(|i| {
        edges[i].rev = edges.len();
        edges.push(Edge {
            cap: 0,
            flow: 0,
            s: edges[i].t,
            t: edges[i].s,
            rev: i,
            is_rev: true,
        });
    });

    let mut adj_edges: Vec<Vec<usize>> = vec![vec![]; n];
    edges.iter().enumerate().for_each(|(i, e)| {
        adj_edges[e.s].push(i);
    });

    let (flow, flow_edges) = maxflow(s, t, edges, adj_edges);

    println!("{:?} {:?} {:?}", n, flow, flow_edges.len());
    flow_edges
        .iter()
        .for_each(|&(u, v, w)| println!("{:?} {:?} {:?}", u, v, w));
}


/*
 *This algoritm to find the maxflow is an implementation of Edmonds-Karp. 
 * It works by iterativly performing a breath-firts search to find the shortest path between the source and the sink. 
 * For this path we know that the maximum flow through the path is going to be limited by the
 * smallest capacity along the path. As such we reduce the capacity by so much through out the
 * path. 
 * This we continue until no path can be found essentilly meaning that we have exausted the
 * capacity of the network and can send no more through it.
 * 
 * Input/Output:
 * The inputs for this implementation of the algorithm is the index of the source and the target
 * (s, t). The edges of the graph as well as as what nodes are connected to what other nodes (adj_edges).
 * The output of the function is simply the maximum flow found as well as a vector of the edges and
 * the resulting flow in them.
 *
 * Complexity:
 * The complexity of this algorithm is going to be O(v*e^2) where v is the number of nodes and e
 * the number of edges. We know this is the case as breath-first search is going to have a
 * complexity of O(v*e) and we know that for each path we find we are going to be saturating at
 * least one edge in the network so the number of times that we will have to perform this seach is
 * e times.
 *
 *
*/

fn maxflow(
    s: usize,
    t: usize,
    mut edges: Vec<Edge>, 
    adj_edges: Vec<Vec<usize>>,
) -> (i64, Vec<(usize, usize, usize)>) {
    let mut flow = 0;
    loop {
        let mut queue: VecDeque<usize> = VecDeque::new();
        queue.push_back(s);
        let mut pred: Vec<Option<usize>> = vec![None; adj_edges.len()];
        while !queue.is_empty() && pred[t] == None {
            let cur = queue.pop_front().unwrap();
            for &i_edge in &adj_edges[cur] {
                if pred[edges[i_edge].t] == None
                    && edges[i_edge].t != s
                    && edges[i_edge].cap > edges[i_edge].flow
                {
                    pred[edges[i_edge].t] = Some(i_edge);
                    queue.push_back(edges[i_edge].t);
                }
            }
        }

        if pred[t] != None {
            let mut df = i64::MAX;

            let mut i_edge = pred[t];
            while i_edge.is_some() {
                df = df.min(edges[i_edge.unwrap()].cap as i64 - edges[i_edge.unwrap()].flow);

                i_edge = pred[edges[i_edge.unwrap()].s];
            }

            let mut i_edge = pred[t];
            while i_edge.is_some() {
                df = df.min(edges[i_edge.unwrap()].cap as i64 - edges[i_edge.unwrap()].flow);
                edges[i_edge.unwrap()].flow += df;
                let rev_edge_id = edges[i_edge.unwrap()].rev;
                edges[rev_edge_id].flow -= df;

                i_edge = pred[edges[i_edge.unwrap()].s];
            }
            flow += df;
        } else {
            break;
        }
    }

    let mut flow_edges: Vec<(usize, usize, usize)> = vec![];
    for (i_n, con) in adj_edges.iter().enumerate() {
        for &i_e in con {
            if edges[i_e].flow != 0 && !edges[i_e].is_rev {
                flow_edges.push((i_n, edges[i_e].t, edges[i_e].flow as usize));
            }
        }
    }
    (flow, flow_edges)
}
