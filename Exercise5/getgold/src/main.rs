use std::{collections::VecDeque, fs, io::{self, Read}};

#[derive(Clone, PartialEq)]
enum State {
    Drafty,
    Unknown,
    Safe,
    Wall,
    Visited,
    Planed
}

#[derive(Clone)]
struct AgentState {
    state: Vec<Vec<State>>
}

impl AgentState {
    fn new(w: usize, h: usize) -> AgentState {
        AgentState { state: vec![vec![State::Unknown; w]; h]}
    }
    fn update(&mut self, x: usize , y: usize, map: &Map) {
        self.mark_visited(x, y);

        let directions = [(-1, 0), (1, 0), (0, 1), (0, -1)];

        let mark_state = if directions.iter().any(|&(dx, dy)| {
            if let (Some(nx), Some(ny)) = (offset(x, dx), offset(y, dy)) {
                map.get(nx)
                    .and_then(|row| row.get(ny))
                    .unwrap_or(&'.')
                    == &'T'
            } else {
                // Fallback to current cell if the offset is out-of-bounds
                false
            }
        }) {
            State::Drafty
        } else {
            State::Safe
        };

        for &(dx, dy) in &directions {
            if let (Some(nx), Some(ny)) = (offset(x, dx), offset(y, dy)) {
                if let Some(row) = self.state.get_mut(nx) {
                    if let Some(cell) = row.get_mut(ny) {
                        if *cell == State::Unknown || *cell == State::Drafty {
                            *cell = mark_state.clone();
                        }
                    }
                }
            }
        }
    }



    fn get_moves (&mut self, x: usize, y: usize) -> Vec<(usize, usize)> {
        let directions = [(-1, 0), (1, 0), (0, 1), (0, -1)];
        directions
            .iter()
            .filter_map(|&(dx, dy)| {
                if let (Some(nx), Some(ny)) = (offset(x, dx), offset(y, dy)) {
                    if let Some(row) = self.state.get_mut(nx) {
                        if let Some(cell) = row.get_mut(ny) {
                            if *cell == State::Safe {
                                *cell = State::Planed;
                                return Some((nx, ny));
                            }
                        }
                    }
                }
                None
            })
            .collect()
    }


    fn mark_wall (&mut self, x: usize, y:usize ) {
        self.state[x][y] = State::Wall;
    }

    fn mark_visited (&mut self, x: usize, y:usize ) {
        self.state[x][y] = State::Visited;
    }
    


    fn print(&self) {
        for row in &self.state {
            for cell in row {
                let symbol = match cell {
                    State::Drafty => "D",
                    State::Unknown => "U",
                    State::Safe => "S",
                    State::Wall => "W",
                    State::Visited => "V",
                    State::Planed => "P"
                };
                print!("{} ", symbol);
            }
            println!();
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

type Map = Vec<Vec<char>>;

fn main() {
    let file_path = "getgold.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);
    
    let mut lines: std::str::Lines<'_> = content.lines();
    let mut first_line = lines.next().unwrap().split_whitespace();
    let w: usize = first_line.next().unwrap().parse().unwrap();
    let h: usize = first_line.next().unwrap().parse().unwrap();

    let map: Map = lines.map(|l| l.chars().map(|c| c ).collect::<Vec<char>>()).collect();
    let l_p_pos = map.clone()
                                    .iter()
                                    .enumerate()
                                    .find(|(_, l)| l.contains(&'P'))
                                    .unwrap()
                                    .0;
                                
    let p_pos = (l_p_pos, 
                                map
                                    .clone()[l_p_pos]
                                    .iter()
                                    .enumerate()
                                    .find(|(_, c)| **c == 'P')
                                    .unwrap()
                                    .0);

    
    let mut agent_state = AgentState::new(w, h);

    agent_state.state[p_pos.0][p_pos.1] = State::Safe;
    
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();
    
    agent_state.update(p_pos.0, p_pos.1, &map);
    agent_state.get_moves(p_pos.0, p_pos.1).iter().for_each(|m: &(usize, usize)| queue.push_back(*m));


    let mut allt_mitt_guuuuld = 0;
    
    while !queue.is_empty() {
        
        let n_move = queue.pop_front().unwrap();
        if map[n_move.0][n_move.1] == '#' {
            agent_state.mark_wall(n_move.0, n_move.1);
        }
        else {
            if map[n_move.0][n_move.1] == 'G' {
                allt_mitt_guuuuld += 1;
            }
            agent_state.update(n_move.0, n_move.1, &map);
            agent_state.get_moves(n_move.0, n_move.1).iter().for_each(|m: &(usize, usize)| queue.push_back(*m));

        }
    }

    println!("{}", allt_mitt_guuuuld);
    

}