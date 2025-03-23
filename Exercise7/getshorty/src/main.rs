use std::fs;
use std::{
    collections::BinaryHeap,
    io::{self, Read},
};

use std::cmp::Ordering;

#[derive(Debug, Copy, Clone, PartialEq)]
struct TotalF64(f64);

impl Eq for TotalF64 {}

impl PartialOrd for TotalF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TotalF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}

fn main() {
    let file_path = "getshorty.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> f64 { tokens.next().unwrap().parse().unwrap() };

    loop {
        let n = next() as usize;
        let m = next() as usize;

        if n == 0 && m == 0 {
            break;
        }

        let mut adj_edges: Vec<Vec<(TotalF64, usize)>> = vec![vec![]; n];

        for _ in 0..m {
            let x = next() as usize;
            let y = next() as usize;
            let f = next();
            adj_edges[x].push((TotalF64(f.ln()), y));
            adj_edges[y].push((TotalF64(f.ln()), x));
        }

        let mut p_q: BinaryHeap<(TotalF64, usize)> = BinaryHeap::new();
        p_q.push((TotalF64(0.0), 0));
        let mut is_visted: Vec<bool> = vec![false; n];
        let mut cost_to_goal = 0.0;

        while let Some((f, current_n)) = p_q.pop() {
            if current_n == n-1 {
                cost_to_goal = f.0;
                break;
            }

            if is_visted[current_n] {
                continue;
            }

            is_visted[current_n] = true;

            let neighbours = &adj_edges[current_n];
            for &n_edge in neighbours {

                p_q.push((TotalF64(f.0 + n_edge.0 .0), n_edge.1));
            }
        }

        println!("{}", format!("{:.4}", cost_to_goal.exp()));
    }
}
