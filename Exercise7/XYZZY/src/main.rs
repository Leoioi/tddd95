use std::{
    fs, io::{self, Read}
};

fn main() {
    // let file_path = "xyzzy.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();    
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let mut result = String::new();

    loop {
        let n = match next() {
            -1  => break,
            n => n
        };
        if n == 0 {
            result.push_str("hopeless\n");
            continue;
        }

        let mut edges: Vec<Vec<(usize, i64)>> = vec![vec![]; (n + 1 ) as usize];

        let mut energies: Vec<i64> = vec![0; (n + 1) as usize ];


        for i in 1..=n as usize{
            let energy: i64 = next();
            let rooms: usize = next() as usize;

            energies[i] = energy;
             
            for _ in 0..rooms {
                let connecting_room: usize = next() as usize;
                edges[i as usize].push((connecting_room, 0));
            }
        }
        //println!("{:?}", energies);
        edges[0].push((1, -energies[1] - 100));

        for i in 1..=n as usize {
            for (v, w) in edges[i].iter_mut() {
                *w = -energies[*v];
            }
        }


        // println!("{:?}", edges);
        
        let res_dist = bellman_ford(edges, 0);

        //println!("dist {:?}", res_dist);
        // println!("pres {:?}", pre);

        match res_dist[n as usize] {
            Ok(dist) => {
                if dist < 0 {
                    result.push_str("winnable\n");
                }
                else {
                    result.push_str("hopeless\n");
                }
            }
            Err(NodeErrors::Unreachable) => {
                result.push_str("hopeless\n");
            }
            Err(NodeErrors::NegInfinite) => {
                result.push_str("winnable\n");
            }
            
        } 
    }
    print!("{}", result);
}

#[derive(Debug, Clone, PartialEq)]
enum NodeErrors {
    Unreachable,
    NegInfinite,
}

fn bellman_ford(
    edges: Vec<Vec<(usize, i64)>>,
    start: usize,
) -> Vec<Result<i64, NodeErrors>> {
    let mut distances: Vec<Result<i64, NodeErrors>> =
        vec![Err(NodeErrors::Unreachable); edges.len()];

    distances[start] = Ok(0);

    let mut updated: bool = false;
    for _ in 0..edges.len() {
        for u in  0..edges.len() {
            if let Ok(d_u) = distances[u] {
                for &(v, w) in edges[u].iter(){
                    let sum = d_u + w;
                    if sum >= 0 {
                        continue;
                    }

                    let d_v = distances[v].clone().unwrap_or(i64::MAX);
                    if d_u + w < d_v {
                        distances[v] = Ok(d_u + w);
                        updated = true;
                    }
                }
            }
        }
        if !updated {
            break; // Early exit if no more updates
        }
    }

    for _ in 0..edges.len() {
        let mut updated = false;
        for u in 0..edges.len() {
            match distances[u] {
                Ok(d_u) => {
                    for &(v, w) in &edges[u] {
                        let sum = d_u + w;
                        if sum >= 0 {
                            continue;
                        }
                        match distances[v] {
                            Ok(d_v) => {
                                if  d_v > sum {
                                    distances[v] = Err(NodeErrors::NegInfinite);
                                    updated = true;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Err(NodeErrors::NegInfinite) => {
                    for &(v, _) in &edges[u] {
                        distances[v] = Err(NodeErrors::NegInfinite);
                        updated = true;
                    }
                }
                _ => {}
            }


        }
        if !updated {
            break;
        }
    }
    return distances;
}
