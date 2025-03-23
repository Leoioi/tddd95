use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap, VecDeque},
    fs,
    io::{self, Read},
    iter::once,
    u32, usize,
};
fn is_same(parents: &mut Vec<usize>, a: usize, b: usize) -> bool {
    find(a, parents) == find(b, parents)
}

fn find(x: usize, parents: &mut Vec<usize>) -> usize {
    if parents[x] != x {
        // Set the parent of this node to the parent of my parent, thereby flatting the tree.
        parents[x] = find(parents[x], parents);
    }
    return parents[x];
}

fn union(x: usize, y: usize, parents: &mut Vec<usize>, sizes: &mut Vec<usize>) {
    let x_root: usize = find(x, parents);
    let y_root: usize = find(y, parents);

    if x_root == y_root {
        return;
    }

    // If the size of the root for the x tree is smaller then add that tree to the y tree,
    if sizes[x_root] < sizes[y_root] {
        parents[x_root] = y_root;
        sizes[y_root] += sizes[x_root];
    } else {
        parents[y_root] = x_root;
        sizes[x_root] += sizes[y_root];
    }
}

fn main() {
    let file_path = "borg.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    let t: u32 = lines
        .next()
        .unwrap()
        .split_whitespace()
        .next()
        .unwrap()
        .parse()
        .unwrap();

    for _ in 0..t {
        let mut nmi: std::str::SplitWhitespace<'_> = lines.by_ref().next().unwrap().split_whitespace();
        let x: usize = nmi.next().unwrap().parse().unwrap();
        let y: usize = nmi.next().unwrap().parse().unwrap();

        let map: Vec<Vec<char>> = lines
            .by_ref()
            .take(y as usize)
            .map(|l| l.chars().map(|c| c).collect())
            .collect();

        let start: (usize, usize) = {
            let y_pos = map
                .clone()
                .iter()
                .enumerate()
                .find(|(_, l)| l.contains(&'S'))
                .unwrap()
                .0;
            (
                y_pos.try_into().unwrap(),
                map.clone()[y_pos]
                    .iter()
                    .enumerate()
                    .find(|(_, c)| **c == 'S')
                    .unwrap()
                    .0
                    .try_into()
                    .unwrap(),
            )
        };

        let s_a_a: Vec<(usize, usize)> = map
            .iter()
            .enumerate()
            .flat_map(|(i, row)| {
                row.iter().enumerate().filter_map(
                    move |(j, &c)| {
                        if c == 'A' {
                            Some((i, j))
                        } else {
                            None
                        }
                    },
                )
            })
            .chain(once(start))
            .collect();

        let valid_moves = |p: (usize, usize)| -> Vec<(usize, usize)> {
            let directions = [(-1, 0), (1, 0), (0, 1), (0, -1)];
            directions
                .iter()
                .filter_map(|&(dx, dy)| {
                    if let (Some(nx), Some(ny)) = (offset(p.0 as i32, dx), offset(p.1 as i32, dy)) {
                        if let Some(row) = map.get(nx as usize) {
                            if let Some(cell) = row.get(ny as usize) {
                                if *cell != '#' {
                                    return Some((nx, ny));
                                }
                            }
                        }
                    }
                    None
                })
                .collect()
        };

        let mut queue = VecDeque::new();
        let mut exp_map: Vec<Vec<Option<(usize, usize)>>> = vec![vec![None; x]; y];
        //let mut best_cost_map: Vec<Option<usize>> = vec![None; s_a_a.len()];
        let mut tot = 0;
        let mut parents: Vec<usize> = (0..s_a_a.len()).collect();
        let mut sizes: Vec<usize> = vec![1; s_a_a.len()];

        s_a_a.iter().enumerate().for_each(|(id, &(ys, xs))| {
            exp_map[ys][xs] = Some((id, 0));
            queue.push_back(((ys, xs), id));
        });

        while let Some((pos, id)) = queue.pop_front() {
            // print_exp_map(&exp_map, &mut parents);
            // println!("");
            let v_moves = valid_moves(pos);

            let mut maybe_best_nib: Vec<(usize, usize)> = vec![];
            for v_move in v_moves {
                if exp_map[v_move.0][v_move.1].is_none() {
                    exp_map[v_move.0][v_move.1] = Some((id, exp_map[pos.0][pos.1].unwrap().1 + 1));
                    queue.push_back((v_move, id));
                } else {
                    if !is_same(&mut parents, exp_map[v_move.0][v_move.1].unwrap().0, id) {
                        
                        if maybe_best_nib.len() > 0 {
                            if exp_map[maybe_best_nib[0].0][maybe_best_nib[0].1].unwrap().1
                                > exp_map[v_move.0][v_move.1].unwrap().1
                            {
                                maybe_best_nib = vec![v_move];
                            }
                            else if exp_map[maybe_best_nib[0].0][maybe_best_nib[0].1].unwrap().1
                            == exp_map[v_move.0][v_move.1].unwrap().1  {
                                if maybe_best_nib.iter().all(|&added_v| !is_same(&mut parents, exp_map[added_v.0][added_v.1].unwrap().0, exp_map[v_move.0][v_move.1].unwrap().0)) {
                                    maybe_best_nib.push(v_move);
                                }
                            }
                        }
                        else {
                            maybe_best_nib.push(v_move);
                        }
                    }
                }
            }
            for best_nib in maybe_best_nib {
                let id_parent = find(id, &mut parents);

                union(
                    exp_map[best_nib.0][best_nib.1].unwrap().0,
                    id_parent,
                    &mut parents,
                    &mut sizes,
                );
                tot += exp_map[pos.0][pos.1].unwrap().1
                    + exp_map[best_nib.0][best_nib.1].unwrap().1
                    + 1;
                // let old_id = if find(id_parent, &mut parents) == id_parent {
                //     exp_map[best_nib.0][best_nib.1].unwrap().0
                // } else {
                //     id_parent
                // };
                // best_cost_map[old_id] = Some(
                //     exp_map[pos.0][pos.1].unwrap().1
                //         + exp_map[best_nib.0][best_nib.1].unwrap().1
                //         + 1,
                // );
                // println!("my pos {:?}", pos);
                // println!("best nib pos  {:?}", best_nib);
                // println!("{}", tot);
                //     println!("{:?}", best_cost_map);
                //     println!("newly added cost {:?}", best_cost_map[old_id]);
                //     println!("old_id: {:?}", old_id);
            }
        }
        println!("{:?}", tot);
    }
}

fn offset(index: i32, delta: isize) -> Option<usize> {
    let new_index = index as isize + delta;
    if new_index >= 0 {
        Some(new_index as usize)
    } else {
        None
    }
}

fn print_exp_map(matrix: &[Vec<Option<(usize, usize)>>], parents: &mut Vec<usize>) {
    for row in matrix {
        for cell in row {
            match cell {
                //Some((a, b)) => print!("({}, {}) ", a , b),
                Some((a, b)) => print!("({}, {}) ", find(*a, parents), b),
                None => print!(" None  "),
            }
        }
        println!();
    }
}
