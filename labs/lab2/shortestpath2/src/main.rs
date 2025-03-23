use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
    fs,
    io::{self, Read}, ops::ControlFlow,
};

fn main() {
    let file_path = "shortestpath2.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    loop {
        let nmqs = match lines.next() {
            Some(line) => line.split_whitespace().collect::<Vec<_>>(),
            None => break,
        };

        let n: usize = nmqs[0].parse().unwrap();
        let m: usize = nmqs[1].parse().unwrap();
        let q: usize = nmqs[2].parse().unwrap();
        let s: usize = nmqs[3].parse().unwrap();

        if n == 0 && m == 0 && q == 0 && s == 0 {
            break;
        }

        let mut graph: Vec<Vec<(usize, usize, usize, usize)>> = vec![vec![]; n]; // destination, t0, p, d
        for _ in 0..m {
            let edge_line = lines.next().unwrap().split_whitespace().collect::<Vec<_>>();
            let u: usize = edge_line[0].parse().unwrap();
            let v: usize = edge_line[1].parse().unwrap();
            let t0: usize = edge_line[2].parse().unwrap();
            let p: usize = edge_line[3].parse().unwrap();
            let d: usize = edge_line[4].parse().unwrap();
            graph[u].push((v, t0, p, d));
        }

        let queries: Vec<usize> = (0..q)
            .map(|_| lines.by_ref().next().unwrap().parse::<usize>().unwrap())
            .collect();

        let best_cost = dijkstra(n, s, graph);

        queries.iter().for_each(|q| {
            if best_cost[*q].1.is_none() {
                println!("{}", "Impossible")
            } else {
                println!("{:?}", best_cost[*q].0);
                // let path = {
                //     // Back track to the start node
                //     let mut current_node = *q;
                //     let mut path: Vec<usize> = vec![];
                //     while best_cost[current_node].1 != Some(current_node) {
                //         path.push(current_node);
                //         current_node = best_cost[current_node].1.unwrap();
                //     }
                //     path.reverse();
                //     path
                // };
                // If the path is desired uncomment the line below
                //println!("{:?}", path);
            }
        });
    }
}

/*
The function below implements the dijkstra algorithm, this algorithm will start at some node and
explore outwards considering only the nodes that are neighbors to the current node. These neighbors will
be added to a priory queue where the nodes with the lowest cost are the once that are considered first.
When adding a node to the queue we are going to add the cost of the current node to the cost of going to
that neighbor node. If however the cost to get to some neighbor node is less then this new cost then we know
that there exists some better path to get to the neighbor and we dont need to consider the new cost.

-- Complexity
The complexity of this algorithm will be the cost be cost of adding then poping each node from the priority queue.
For the binary heap the cost of adding an element will be constant O(1) and the cost of poping an element will be
O(log n) if we have n nodes. The the time complexity fo adding and removing the nodes will be O(n log n).
On top of this we also have to consider that in the worst case we will have to explore every edge O(v) where is
the number of edges.
The final time complexity will as such be
O(v + n log n)
*/
fn dijkstra(
    n: usize,
    start_node: usize,
    graph: Vec<Vec<(usize, usize, usize, usize)>>,
) -> Vec<(usize, Option<usize>)> {
    let mut best_time: Vec<(usize, Option<usize>)> = vec![(usize::MAX, None); n];
    best_time[start_node] = (0, Some(0));

    let mut queue: BinaryHeap<Reverse<(usize, usize)>> = BinaryHeap::new();
    queue.push(Reverse((0, start_node)));

    while let Some(Reverse((current_time, u))) = queue.pop() {
        if current_time > best_time[u].0 {
            continue;
        }
        for &(neighbor_node, start_time, period, duration) in &graph[u] {
            let new_time = match calc_new_cost(start_time, period, duration, current_time) {
                Some(value) => value,
                None => continue,
            };

            if best_time[neighbor_node].0 > new_time {
                best_time[neighbor_node] = (new_time, Some(u));
                queue.push(Reverse((new_time, neighbor_node)));
            };
        }
    }
    best_time
}

fn calc_new_cost(start_time: usize, period: usize, duration: usize, current_time: usize) -> Option<usize> {
    let new_time = {
        if start_time >= current_time {
            start_time.abs_diff(current_time)
        } else {
            if period == 0 {
                return None;
            }
            (period - ((current_time - start_time) % period)) % period
        }
    } + duration
        + current_time;
    Some(new_time)
}
