use std::{cmp::Reverse, collections::{BinaryHeap, HashMap}, fs, io::{self, Read}};



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
    let file_path = "two.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut content = String::new();
    // io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    //let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };

    let x1 = tokens.next().unwrap().parse::<usize>().unwrap() as usize;
    let y1 = tokens.next().unwrap().parse::<usize>().unwrap() as usize;
    let x2 = tokens.next().unwrap().parse::<usize>().unwrap() as usize;
    let y2 = tokens.next().unwrap().parse::<usize>().unwrap() as usize;

    let mut all_stops: Vec<(usize, usize)> = vec![];
    let mut position_id_map: HashMap<(usize, usize), usize> = HashMap::new();

    position_id_map.insert((x1, y1), all_stops.len());
    all_stops.push((x1, y1));    
    position_id_map.insert((x2, y2), all_stops.len());

    all_stops.push((x2, y2));

    let mut subway_lines: Vec<Vec<(usize, usize)>> = vec![];


    let mut is_end_ln = false;
    let mut i = 0;
    loop {
        if is_end_ln {
            break;
        }
        subway_lines.push(vec![]);

        loop {
            let mut x: i64= 0;
            if let Some(maybe_x) = tokens.next() {
                x = maybe_x.parse::<i64>().unwrap();
            }
            else {
                is_end_ln = true;
                break;
            }

            let y = tokens.next().unwrap().parse::<i64>().unwrap();

            if x == -1 && y == -1 {
                break;
            }
            subway_lines[i].push((x as usize,y as usize));
            position_id_map.insert((x as usize, y as usize), all_stops.len());
            all_stops.push((x as usize, y as usize));

        }
        i += 1;
    }

    subway_lines.pop();

    let mut adj_grapf: Vec<Vec<(usize, TotalF64)>> = vec![vec![]; all_stops.len()];

    // println!("{:?}", subway_lines);


    for subway in subway_lines {
        for (&stop1, stop2) in subway.iter().zip(subway[1..].to_vec()) {
            adj_grapf[position_id_map[&stop1]].push((position_id_map[&stop2], dist_sub(&stop1, &stop2)));
            adj_grapf[position_id_map[&stop2]].push((position_id_map[&stop1], dist_sub(&stop1, &stop2)));  
        }
    }


    for &stop1 in all_stops.iter().by_ref() {
        for &stop2 in all_stops.iter().by_ref() {
            adj_grapf[position_id_map[&stop1]].push((position_id_map[&stop2], dist_walk(&stop1, &stop2)));
        }
    }
    //println!("{:?}", adj_grapf);

    let best_cost = dijkstra(all_stops.len(), position_id_map[&all_stops[0]], adj_grapf);
    //println!("{:?}", best_cost);


    println!("{:.0}", best_cost[ position_id_map[&all_stops[1]]].0.0 / 60.0);

}

fn dist_walk (stop1: &(usize, usize), stop2: &(usize, usize)) -> TotalF64 {
    return TotalF64(((stop1.0 as f64 - stop2.0 as f64 ).powf(2.0) + (stop1.1 as f64 - stop2.1 as f64).powf(2.0)).sqrt()  * (3.6/10.0)) ;
}


fn dist_sub (stop1: &(usize, usize), stop2: &(usize, usize)) -> TotalF64 {
    return TotalF64(((stop1.0 as f64 - stop2.0 as f64 ).powf(2.0) + (stop1.1 as f64 - stop2.1 as f64).powf(2.0)).sqrt() * (3.6/40.0)) ;
}

fn dijkstra(n: usize, start_node: usize, graph: Vec<Vec<(usize, TotalF64)>>) -> Vec<(TotalF64   , Option<usize>)> {
    let mut best_cost: Vec<(TotalF64, Option<usize>)> = vec![( TotalF64 (f64::MAX), None); n];
    best_cost[start_node] = (TotalF64(0.0), Some(0));

    let mut queue: BinaryHeap<Reverse<(TotalF64, usize)>> = BinaryHeap::new();
    queue.push(Reverse((TotalF64(0.0), start_node)));

    while let Some(Reverse((current_dist, u))) = queue.pop() {
        if current_dist > best_cost[u].0 {
            continue;
        }
        for &(neighbor_node, weigh) in &graph[u] {
            let new_weighs = TotalF64(current_dist.0 + weigh.0);
            if best_cost[neighbor_node].0 > new_weighs {
                best_cost[neighbor_node] = (new_weighs, Some(u));
                queue.push(Reverse((new_weighs, neighbor_node)));
            };
        }
    }
    best_cost
}
