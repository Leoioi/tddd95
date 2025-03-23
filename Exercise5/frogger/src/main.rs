use std::{collections::{HashSet, VecDeque}, fs, io::{self, Read}};


fn main() {
    // let file_path = "frogger.in";
    // let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);
    
    let mut lines = content.lines();

    let t: u32 = lines.next().unwrap().split_whitespace().next().unwrap().parse().unwrap();

    for _ in 0..t {
        let x : u32 = lines.next().unwrap().parse().unwrap();
        let mut nmi = lines.next().unwrap().split_whitespace();
        let n: u32 = nmi.next().unwrap().parse().unwrap();
        let m: u32 = nmi.next().unwrap().parse().unwrap();

        let map: Vec<Vec<char>> = lines.clone().take((n + 2) as usize).map(|l| l.chars().map(|c| c ).collect()).collect();
        for _ in 0..((n + 2) as usize) {
            lines.next();
        }

        let time_s_maps: Vec<Vec<Vec<char>>> = (0..m).map(|s| {
                                                    let mut new_map = map.clone();
                                                    new_map.iter_mut().skip(1).take(n as usize).enumerate()
                                                            .for_each(|(i_l , l)| if (n - (i_l as u32)) % 2 == 1 {
                                                                l.rotate_right(s as usize); 
                                                            } else {
                                                                l.rotate_left(s as usize);
                                                            });
                                                    new_map
                                                    }).collect();

        let start = (map.len() - 1, map.last().unwrap().iter().enumerate().find(|(_, c)| **c == 'F').unwrap().0 );

        let mut queue: VecDeque<((usize, usize), usize)> = VecDeque::new();

        let valid_moves = |(x, y), t: usize| -> Vec<(usize, usize)>{
            let directions = [(-1, 0), (1, 0), (0, 1), (0, -1), (0, 0)];
            directions
                       .iter().filter_map(|&(dx, dy)| {
                        if let (Some(nx), Some(ny)) = (offset(x, dx), offset(y, dy)) {
                            if let Some(row) = time_s_maps[(t + 1) % m as usize].get(nx)  {
                                if let Some(cell) = row.get(ny) { if *cell != 'X' { return Some((nx,ny)); } }
                            }
                        }
                        None
                    }).collect()
                };
    
        let mut considered_moves: HashSet<((usize, usize), usize)> = HashSet::new();
        let mut update_queue =  |moves: Vec<(usize, usize)>, t: usize, queue: &mut VecDeque<((usize, usize), usize)>| {
            moves.iter().for_each(|mo| {
                if !considered_moves.contains(&(*mo, (t as u32 % m) as usize)) {
                    considered_moves.insert((*mo, (t as u32 % m) as usize));
                    queue.push_back((*mo, t + 1));
                }
            });
        };

        update_queue(valid_moves(start, 0), 0, &mut queue);

        let mut goal_move: Option<u32> = None;
        
        loop {
            if let Some((n_move, t_m)) = queue.pop_front() {
                if t_m > x as usize {
                    break;
                }
                if time_s_maps[(t_m as u32 % m )as usize][n_move.0][n_move.1] == 'G'  {goal_move = Some(t_m as u32); break; }
                update_queue(valid_moves(n_move, t_m), t_m, &mut queue);
            } else {
                break;
            }
        }
        if goal_move.is_some() {
            println!("The minimum number of turns is {}.", goal_move.unwrap());
        }
        else {
            println!("The problem has no solution.");
        }
    }
}


fn offset(index: usize, delta: isize) -> Option<usize> {
    let new_index = index as isize + delta;
    if new_index >= 0 {
        Some(new_index as usize)
    } else {
        None
    }
}
