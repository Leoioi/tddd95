use std::{cmp::Ordering, collections::{BinaryHeap, HashMap, VecDeque}, fs, io::{self, Read}, iter::once, u32};


struct UnionFind {
    parents: HashMap<(i32, i32), (i32, i32)>, 
    sizes: HashMap<(i32, i32), usize>,  
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parents: HashMap::new(),
            sizes: HashMap::new(),
        }
    }
    fn find(&mut self, x: (i32, i32)) -> (i32, i32) {
        if self.parents.get(&x).copied().unwrap_or(x) != x {
            let parent = self.find(*self.parents.get(&x).unwrap());
            self.parents.insert(x, parent);
        }
        *self.parents.get(&x).unwrap_or(&x)
    }
    fn union(&mut self, x: (i32, i32), y: (i32, i32)) {
        let x_root = self.find(x);
        let y_root = self.find(y);

        if x_root == y_root {
            return;
        }

        let size_x = *self.sizes.get(&x_root).unwrap_or(&1);
        let size_y = *self.sizes.get(&y_root).unwrap_or(&1);

        if size_x < size_y {
            self.parents.insert(x_root, y_root);
            self.sizes.insert(y_root, size_x + size_y);
        } else {
            self.parents.insert(y_root, x_root);
            self.sizes.insert(x_root, size_x + size_y);
        }
    }
    fn make_set(&mut self, x: (i32, i32)) {
        self.parents.insert(x, x);
        self.sizes.insert(x, 1);
    }

}

fn main() {
    let file_path = "borg.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut lines = content.lines();

    let t: u32 = lines.next().unwrap().split_whitespace().next().unwrap().parse().unwrap();

    for _ in 0..t {
        let mut nmi = lines.next().unwrap().split_whitespace();
        let x: u32 = nmi.next().unwrap().parse().unwrap();
        let y: u32 = nmi.next().unwrap().parse().unwrap();

        let map: Vec<Vec<char>> = lines.clone().take(y as usize).map(|l| l.chars().map(|c| c ).collect()).collect();

        for _ in 0..(y as usize) {
            lines.next();
        }

        let start: (i32, i32) = {let y_pos = map.clone().iter().enumerate().find(|(_, l)| l.contains(&'S')).unwrap().0;
                        (y_pos.try_into().unwrap(), map.clone()[y_pos].iter().enumerate().find(|(_, c)| **c == 'S').unwrap().0.try_into().unwrap())};
        


        let s_a_a: Vec<(i32, i32)> = map.iter()
                        .enumerate()
                        .flat_map(|(i, row)| row.iter()
                                                                        .enumerate()
                                                                        .filter_map(move |(j, &c)| if c == 'A' { Some((i as i32, j as i32)) } else { None }))
                                                                        .chain(once(start))
                        .collect();
        

        let valid_moves = |p: (i32, i32)| -> Vec<(i32, i32)>{
                        let directions = [(-1, 0), (1, 0), (0, 1), (0, -1)];
                        directions
                                    .iter().filter_map(|&(dx, dy)| {
                                    if let (Some(nx), Some(ny)) = (offset(p.0, dx), offset(p.1, dy)) {
                                        if let Some(row) = map.get(nx as usize)  {
                                            if let Some(cell) = row.get(ny as usize) { if *cell != '#' {
                                                    return Some((nx, ny)); 
                                                } 
                                            }
                                        }
                                    }
                                    None
                                }).collect()
                            };

        let bfs = |a: (i32, i32), bs: Vec<(i32, i32)>| -> Vec<((i32, i32), i32)> {
            let mut queue  = VecDeque::new();
            let mut dists= HashMap::new();
            
            queue.push_back((a, 0));
            dists.insert(a, 0);

            while let Some((pos, dist)) = queue.pop_front() {                
                valid_moves(pos).iter().for_each(|n_pos| if !dists.contains_key(n_pos) { dists.insert(*n_pos, dist + 1); queue.push_back((*n_pos, dist + 1)); });
                }
            bs.iter().filter_map(|&b| dists.get(&b).map(|&d| (b, d))).collect()
        };

        let mut sorted_edges: Vec<(((i32, i32), (i32, i32)), i32)>  = vec![];

        for i in 0..s_a_a.len() {
            sorted_edges.append(&mut bfs(s_a_a[i], (&s_a_a[(i + 1)..]).to_vec() ).iter().map(|(b, c)| ((s_a_a[i], *b), *c) ).collect());
        }
 

        
        //let pairs = s_a_a.iter().enumerate().flat_map(|(i, x)| s_a_a.iter().skip(i + 1).map(move |y| (x, y)));
        // let mut sorted_edges: Vec<(((i32, i32), (i32, i32)), i32)> = pairs
        //                     .filter_map(|(a, b)| a_star_cost(*a,*b).map(|cost| ((*a, *b), cost))).collect();
        sorted_edges.sort_by_key(|(_, cost)| *cost);

        let mut union_sets = UnionFind::new();
        s_a_a.iter().for_each(|a| union_sets.make_set(*a) ); 

        let mut tot_cost = 0;
        //println!("{:?}", sorted_edges);
        sorted_edges.iter().for_each(|((a,b, ), cost)| { if union_sets.find(*a) != union_sets.find(*b) {tot_cost += cost; union_sets.union(*a, *b); }});


        println!("{:?}", tot_cost);
    }

}



fn offset(index: i32, delta: isize) -> Option<i32> {
    let new_index = index as isize + delta;
    if new_index >= 0 {
        Some(new_index as i32)
    } else {
        None
    }
}