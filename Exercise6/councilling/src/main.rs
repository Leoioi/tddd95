use std::{
    collections::{HashMap, VecDeque}, fs, io::{self, Read}
};

#[derive(Debug, Clone, Copy)]
struct Edge {
    to: usize,
    rev: usize,
    cap: i32,
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();
    
    let t: usize = lines.next().unwrap().parse().unwrap();
    let mut result = String::new();
    
    for case in 0..t {
        if case > 0 {
            result.push_str("\n");
        }

        let n: usize = lines.next().unwrap().parse().unwrap();
        let mut residents = Vec::new();
        let mut parties = HashMap::new();
        let mut clubs = HashMap::new();

        for _ in 0..n {
            let line = lines.next().unwrap();
            let parts: Vec<&str> = line.split_whitespace().collect();
            let name = parts[0].to_string();
            let party = parts[1].to_string();
            let num_clubs: usize = parts[2].parse().unwrap();
            let resident_clubs: Vec<String> = parts[3..3 + num_clubs]
                .iter()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();

            residents.push((name, party.clone(), resident_clubs.clone()));

            if !parties.contains_key(&party) {
                parties.insert(party.clone(), parties.len());
            }

            for club in &resident_clubs {
                if !clubs.contains_key(club) {
                    clubs.insert(club.clone(), clubs.len());
                }
            }
        }

        let p = parties.len();
        let c = clubs.len();

        if c == 0 {
            result.push_str("Impossible.\n");
            continue;
        }

        let party_limit = (c - 1) / 2;
        if party_limit == 0 && c > 1 {
            result.push_str("Impossible.\n");
            continue;
        }

        let total_nodes = n + p + c + 2;
        let source = n + p + c;
        let sink = source + 1;

        let mut graph: Vec<Vec<Edge>> = vec![Vec::new(); total_nodes];

        for (club, &club_id) in &clubs {
            let club_node = n + p + club_id;
            add_edge(&mut graph, source, club_node, 1);
        }

        for (resident_id, (_, party, resident_clubs)) in residents.iter().enumerate() {
            let party_id = parties[party] + n;

            add_edge(&mut graph, resident_id, party_id, 1);


            for club in resident_clubs {
                let club_id = clubs[club];
                let club_node = n + p + club_id;
                add_edge(&mut graph, club_node, resident_id, 1);
            }
        }

        for (_, &party_id) in &parties {
            let party_node = n + party_id;
            add_edge(&mut graph, party_node, sink, party_limit as i32);
        }


        let max_flow = max_flow(&mut graph, source, sink);

        if max_flow != c as i32 {
            result.push_str("Impossible.\n");
        } else {

            let mut output = Vec::new();
            for (club_name, &club_id) in &clubs {
                let club_node = n + p + club_id;
                for edge in &graph[club_node] {
                    if edge.cap == 0 && edge.to < n {

                        let resident_id = edge.to;
                        let (resident_name, _, _) = &residents[resident_id];
                        output.push(format!("{} {}", resident_name, club_name));
                        break;
                    }
                }
            }
            result.push_str(&output.join("\n"));
            result.push('\n');
        }
    }

    print!("{}", result.trim_end());
}

fn add_edge(graph: &mut Vec<Vec<Edge>>, from: usize, to: usize, cap: i32) {
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

fn max_flow(graph: &mut Vec<Vec<Edge>>, source: usize, sink: usize) -> i32 {
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
            let mut df = i32::MAX;
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
