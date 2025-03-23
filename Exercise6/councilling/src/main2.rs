use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
    io::{self, Read},
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

fn main() {
    // let file_path = "councilling.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    let t: usize = lines.next().unwrap().parse().unwrap();

    let mut result = String::new();

    for case in 0..t {
        if case > 0 {
            result.push_str("\n");
        }

        let n: usize = lines.next().unwrap().parse().unwrap();
        let mut edges: Vec<Edge> = vec![];
        let mut words_to_id: HashMap<String, usize> = HashMap::new();

        let mut party_ids: HashSet<usize> = HashSet::new();
        let mut clubs_ids: HashSet<usize> = HashSet::new();

        for _ in 0..n {
            let line = lines.next().unwrap();
            let parts: Vec<&str> = line.split_whitespace().collect();
            let mut name: String = parts[0].to_string();
            let party: &str = parts[1];
            let num_clubs: usize = parts[2].parse().unwrap();
            let clubs = &parts[3..(3 + num_clubs)];

            let len: usize = words_to_id.len();

            while words_to_id.contains_key(&name.to_owned()) {
                name = name + "1" // to deal with dub names 
            } 

            let name_id: usize = *words_to_id.entry(name.to_owned()).or_insert(len);

            let len: usize = words_to_id.len();
            let party_id: usize = *words_to_id.entry(party.to_owned()).or_insert(len);

            party_ids.insert(party_id);

            edges.push(Edge {
                cap: 1,
                flow: 0,
                s: name_id,
                t: party_id,
                rev: 0,
                is_rev: false,
            });

            for &club in clubs {
                let cleaned_club = club
                    .trim_end_matches(|c: char| !c.is_alphabetic())
                    .to_string();
                if cleaned_club.is_empty() {
                    continue;
                }

                let len = words_to_id.len();
                let club_id = *words_to_id.entry(cleaned_club).or_insert(len);

                clubs_ids.insert(club_id);

                edges.push(Edge {
                    cap: 1,
                    flow: 0,
                    s: club_id,
                    t: name_id,
                    rev: 0,
                    is_rev: false,
                });
            }
        }

        if clubs_ids.is_empty() {
            result.push_str("Impossible.\n");
            continue;
        }

        let s_id = n + clubs_ids.len() + party_ids.len();
        let t_id: usize = n + clubs_ids.len() + party_ids.len() + 1;
        let party_limit = (clubs_ids.len() - 1) / 2;

        if party_limit == 0 {
            result.push_str("Impossible.\n");
            continue;
        }

        for &party_id in party_ids.iter().clone() {
            edges.push(Edge {
                cap: party_limit as i64,
                flow: 0,
                s: party_id,
                t: t_id,
                rev: 0,
                is_rev: false,
            });
        }

        for &club_id in clubs_ids.iter().clone() {
            edges.push(Edge {
                cap: 1,
                flow: 0,
                s: s_id,
                t: club_id,
                rev: 0,
                is_rev: false,
            });
        }

        let edges_len = edges.len();
        for i in 0..edges_len {
            let rev_edge = Edge {
                cap: 0,
                flow: 0,
                s: edges[i].t,
                t: edges[i].s,
                rev: i,
                is_rev: true,
            };
            edges[i].rev = edges.len();
            edges.push(rev_edge);
        }


        let mut adj_edges: Vec<Vec<usize>> =
            vec![vec![]; (n + clubs_ids.len() + party_ids.len() + 2)];
        
        for (i, edge) in edges.iter().enumerate() {
            adj_edges[edge.s].push(i);
        }

        let mut flow = 0;
        loop {
            let mut queue: VecDeque<usize> = VecDeque::new();
            queue.push_back(s_id);
            let mut pred: Vec<Option<usize>> = vec![None; adj_edges.len()];
            while !queue.is_empty() && pred[t_id] == None {
                let cur = queue.pop_front().unwrap();
                for &i_edge in &adj_edges[cur] {
                    if pred[edges[i_edge].t].is_none()
                        && edges[i_edge].t != s_id
                        && edges[i_edge].cap > edges[i_edge].flow
                    {
                        pred[edges[i_edge].t] = Some(i_edge);
                        queue.push_back(edges[i_edge].t);
                    }
                }
            }

            if !pred[t_id].is_none() {
                let mut df = i64::MAX;
                let mut path = Vec::new();
                let mut u = t_id;
                while u != s_id {
                    let edge_i = pred[u].unwrap();
                    let edge = edges[edge_i];
                    df = df.min(edge.cap - edge.flow);
                    path.push(edge_i);
                    u = edge.s;
                }
    
                for &edge_i in &path {
                    edges[edge_i].flow += df;
                    let rev_i = edges[edge_i].rev;
                    edges[rev_i].flow -= df;
                }
    
                flow += df;
            } else {
                break;
            }
        }

        let mut id_to_word = vec![String::new(); words_to_id.len() + 2];
        for (word, &id) in &words_to_id {
            id_to_word[id] = word.clone();
        }

        id_to_word[s_id] = "s".to_owned();
        id_to_word[t_id] = "t".to_owned();

        if flow == clubs_ids.len() as i64 {
            let mut output = Vec::new();
            for edge in edges.iter() {
                if edge.flow != 0 && !edge.is_rev && clubs_ids.contains(&edge.s) {
                    output.push(format!("{} {}", &id_to_word[edge.t].trim_end_matches(|c: char| c.is_numeric()) , id_to_word[edge.s]));
                }
            }
            if output.len() == clubs_ids.len() {
                result.push_str(&output.join("\n"));
                result.push('\n');
            } else {
                result.push_str("Impossible.\n");
            }
        } else {
            result.push_str(&("Impossible. \n"));
        }
    }

    println!("{}", result.trim_end());
}
