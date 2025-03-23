use std::{collections::{HashSet, VecDeque}, fs, io::{self, Read}, iter::FlatMap, result, usize, vec};

fn main() {
    let file_path = "equiv.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);
    
    let mut lines = content.lines();

    let t: u32 = lines.next().unwrap().parse().unwrap();

    let mut result = String::from("");

    for _ in 0..t {
        let mut nmi = lines.by_ref().next().unwrap().split_whitespace();
        let n: usize = nmi.next().unwrap().parse().unwrap(); // number of nodes 
        let m: usize = nmi.next().unwrap().parse().unwrap();

        let mut adj: Vec<Vec<usize>> =  vec![vec![]; n + 1];
        lines.by_ref().take(m).map(|l| l.split_whitespace().map(|n| n.parse::<usize>().unwrap()).collect()).for_each(|l: Vec<usize>| adj[l[0]].push(l[1]) ) ;

        let scc = |adj: Vec<Vec<usize>>| -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
            let n = adj.len();
            let mut components: Vec<Vec<usize>> = vec![];
            let mut order: Vec<usize> = vec![];
        
            let mut visited: Vec<bool> = vec![false; n];

            (1..n).for_each(|i|if !visited[i] {dfs(i, &adj, &mut order, &mut visited);});
            let mut adj_rev: Vec<Vec<usize>> = vec![vec![]; n];
            (1..n).for_each(|v|  adj[v].iter().for_each(| u| adj_rev[*u].push(v)) );
            
            visited =  vec![false; n];
            order.reverse();
            
            
            let mut roots: Vec<usize> = vec![0; n];
            order.iter().for_each(|v| if !visited[*v] { 
                let mut component: Vec<usize> = vec![]; 
                dfs(*v, &adj_rev, &mut component, &mut visited); 
                let root = component.iter().min().unwrap(); 
                component.iter().for_each(|u| roots[*u] = *root);
                components.push(component);
            });
            
            let mut adj_cond: Vec<Vec<usize>> = vec![vec![]; n];
            (1..n).for_each(|v| adj[v].iter().for_each(|u| if roots[v] != roots[*u] {adj_cond[roots[v]].push(roots[*u]);}));
            (components, adj_cond)
        };
        
        let (components, adj_cond ) = scc(adj);
        println!("{:?}", components);
        println!("{:?}", adj_cond);
        let not_out_count = components.iter().map(|c| c.iter().min().unwrap() ).fold(0, |acc, r| if adj_cond[*r].is_empty() {acc + 1} else {acc});

        let not_in_count = components.len() - (adj_cond.clone().concat()).iter().collect::<HashSet<_>>().iter().len();

        if components.len() == 1 {result.push_str(  &(0.to_string() + "\n")); continue;}

        result.push_str(  &(not_out_count.max(not_in_count).to_string() + "\n"));
    }
    println!("{}", result);
}

fn dfs (v: usize, adj: &Vec<Vec<usize>>, output: &mut Vec<usize>, visited: &mut Vec<bool>) {
    visited[v] = true;
    adj[v].iter().for_each(|u| if !visited[*u] {dfs(*u, adj, output, visited)});
    output.push(v);
}