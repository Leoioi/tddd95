
use std::{
    clone, collections::{HashSet, VecDeque}, env::vars, fs, io::{self, Read}, usize, vec
};

fn main() {
    let file_path = "satisfiability.in";
    let content = fs::read_to_string(file_path).expect("failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);
    
    let mut lines = content.lines();

    let t: usize = lines.next().unwrap().parse().unwrap();

    for _ in 0..t {
        let mut nm = lines.next().unwrap().split_whitespace();
        let n: u32 = nm.next().unwrap().parse().unwrap();
        let m: usize = nm.next().unwrap().parse().unwrap();

        let mut clauses: Vec<Vec<(char, usize)>> = vec![vec![]; m];

        for i in 0..m {
            let mut vars = lines.next().unwrap().split_whitespace().map(|word| word.chars().collect::<Vec<char>>());

            while let Some(litteral) = vars.next() {
                if litteral[0] == 'v' {
                    continue;
                }

                clauses[i].push((litteral[0], litteral
                    .iter()
                    .cloned()
                    .skip(litteral.iter().cloned().position(|x| x == 'X').unwrap() + 1).collect::<String>().parse::<usize>().unwrap() - 1 )); 

            }
        }

        let mut assignment: u32 = 0;
        let mut is_satis = false; 

        while assignment <= 2_u32.pow(n + 1) + 1{
            if apply_assignment(&clauses, assignment) {
                is_satis = true;
                break;
            }
            assignment += 1;
        }
        if is_satis {
            println!("satisfiable");
        }
        else {
            println!("unsatisfiable");
        }
    }
}



fn apply_assignment (clauses: &Vec<Vec<(char, usize)>>, assignment: u32) -> bool {
    for clause in clauses {
        if !clause.iter().map(|(n, v)| (*n != '~') == get_assignment(*v, assignment) ).any(|t| t){
            return false;
        }
    } 
    return true;
}

fn get_assignment (index: usize, assignment: u32) -> bool {
    let mask = 1 << index;
    ((assignment & mask) >> index) == 1
}
