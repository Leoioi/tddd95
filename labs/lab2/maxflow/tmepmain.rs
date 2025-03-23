use std::{
    collections::VecDeque,
    fs,
    io::{self, Read},
    usize, vec,
};

fn main() {
    let file_path = "maxflow.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    let nmqs = lines.next().unwrap().split_whitespace().collect::<Vec<_>>();
    let n: usize = nmqs[0].parse().unwrap();
    let m: usize = nmqs[1].parse().unwrap();
    let s: usize = nmqs[2].parse().unwrap();
    let t: usize = nmqs[3].parse().unwrap();

    let edges: Vec<((usize, usize), usize)> = (0..m)
        .map(|_| {
            let line: Vec<usize> = lines
                .next()
                .unwrap()
                .split_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<usize>>();
            ((line[0], line[1]), line[2])
        })
        .collect();

    let mut adj_and_capacity : Vec<Vec<(usize, i32)>> = vec![vec![]; n];
    //let mut capacity: Vec<Vec<i32>> = vec![vec![0; n]; n];
    edges.iter().for_each(|&((u, v), w)| {
        adj_and_capacity[u].push((v,w as i32));
        adj_and_capacity[v].push((u,0));
        //capacity[u][v] += w  as i32;
    });

    let bfs = |s: usize,
               t: usize,
               parent: &mut Vec<i32>,
               adj_and_capacity: &Vec<Vec<(usize, i32)>> |
             //  capacity: Vec<Vec<i32>>|
     -> i32 {
        *parent = vec![-1; n];
        parent[s] = -2;
        let mut queue: VecDeque<(i32, i32)> = VecDeque::new();

        queue.push_back((s as i32, i32::MAX));

        while let Some((cur, flow)) = queue.pop_front() {
            for &(next, next_cap) in &adj_and_capacity[cur as usize] {
                //if parent[next] == -1 && capacity[cur as usize][next] != 0 {
                if parent[next] == -1 && next_cap != 0 {
                    parent[next] = cur;
                    //let new_flow = flow.min(capacity[cur as usize][next] as i32);
                    let new_flow = flow.min(next_cap as i32);
                    if next == t {
                        return new_flow;
                    }
                    queue.push_back((next as i32, new_flow));
                }
            }
        }
        return 0;
    };

    let maxflow = |s: usize, t: usize, adj_and_capacity: &mut Vec<Vec<(usize, i32)>>| {
        let mut flow = 0;
        let mut parent = vec![0; n];

        let mut new_flow = bfs(s, t, &mut parent, &adj_and_capacity);
        while new_flow != 0 {
            flow += new_flow;
            let mut cur = t;
            while cur != s {
                let prev = parent[cur];
                println!("{:?}", parent);
                adj_and_capacity[prev as usize][cur].1 -= new_flow;
                adj_and_capacity[cur][prev as usize].1 += new_flow;
                cur = prev as usize;
            }
            new_flow = bfs(s, t, &mut parent, &adj_and_capacity);
        }
        return flow;
    };

    println!("{:?}", adj_and_capacity);
    let max_flow = maxflow(s, t, &mut adj_and_capacity);
    println!("{:?}", adj_and_capacity);
    let mut flow_edges: Vec<(usize, usize, usize)> = vec![];
    // for (i_n, con) in adj.iter().enumerate() {
    //     for &i_c in con {
    //         if capacity[i_c][i_n] != 0 {
    //             flow_edges.push((i_n, i_c, capacity[i_c][i_n] as usize));
    //         }
    //     }
    // }

    // println!("{:?} {:?} {:?}", n, max_flow, flow_edges.len());
    // flow_edges
    //     .iter()
    //     .for_each(|&(u, v, w)| println!("{:?} {:?} {:?}", u, v, w));
}
