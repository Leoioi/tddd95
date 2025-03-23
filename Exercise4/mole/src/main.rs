use core::{fmt, num};
use std::{cmp::max, collections::{HashMap, HashSet}, fs, i64, io::{self, BufRead, Read}, mem::swap, usize, vec};

type DpMatrix = [[i32; 32]; 32]; 
type Cord = (i32, i32);

fn dist ((c11, c12): Cord, (c21,c22): Cord)  -> f64{((c12 - c22).pow(2) as f64 + (c11 - c21).pow(2) as f64).sqrt()} 
fn diff ((c11, c12): Cord, (c21,c22): Cord)  -> Cord{(c11-c21, c12-c22)}
fn add ((c11, c12): Cord, (c21,c22): Cord)  -> Cord{(c11+c21, c12+c22)}
fn scalar_mult ((c11, c12): Cord, s: i32)  ->  Cord{(c11*s,c12*s)}
fn scalar_div ((c11, c12): Cord, s: i32)  ->  Cord{(c11/s,c12/s)}


fn gcd(mut a: i32, mut b: i32) -> i32 {
    if a < b {
        swap(&mut a, &mut b);
    }
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a.abs()
}

fn print_upper_corner(matrix: &[[i32; 32]; 32], size: usize) {
    let size = size.min(20); // Ensure the size does not exceed 20

    for i in 0..size {
        for j in 0..size {
            print!("{:4} ", matrix[i][j]); 
            
        }
        println!(); 
        
    }
}

fn pre_calc () -> HashMap<(Cord, Cord), Vec<Cord>> {
    let mut coordinates: Vec<Cord> = Vec::new();

    for x in 0..32 {
        for y in 0..32 {
            coordinates.push((x, y));
        }
    }

    let mut pairs  = Vec::new();
    
    for i in 0..coordinates.len() {
        for j in i..coordinates.len() {
            pairs.push((coordinates[i], coordinates[j]));
        }
    }

    let mut all_possible_inbetweens: HashMap<(Cord, Cord), Vec<Cord>> = HashMap::new();

    for (start, end ) in pairs {
        if dist(start, end) > 5 as f64 {
            continue;  
        }

        let diff_cord: (i32, i32) = diff(end, start);

        let steps = gcd(diff_cord.0, diff_cord.1);

        let mut between_squares: Vec<Cord> = vec![];

        for i in 0..steps {
            between_squares.push(add( start, scalar_mult(scalar_div(diff_cord, steps), i)));
        }

        between_squares.push(end);
        all_possible_inbetweens.insert((start, end), between_squares);
    }

    all_possible_inbetweens
}

fn main() {
    //let file_path = "worst_case_input.in";
    let file_path = "mole.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);
    
    let mut lines: std::str::Lines<'_> = content.lines();

    let between_table = pre_calc();
    
    loop {
        let mut first_line = lines.next().unwrap().split_whitespace().map(|n| n.parse::<i32>().unwrap());
        let n = first_line.next().unwrap();
        let d = first_line.next().unwrap();
        let m = first_line.next().unwrap();
        
        if n == 0 && d == 0 && m == 0 {
            break;
        }

        let mut incoming_moles: Vec<Vec<Cord>> = vec![vec![]; 11 as usize]; // time and corresponding moles for that time step 
        for _ in 0..m {
            let mut first_line = lines.next().unwrap().split_whitespace().map(|n| n.parse::<i32>().unwrap());
            let x = first_line.next().unwrap();
            let y = first_line.next().unwrap();
            let t: usize = first_line.next().unwrap() as usize;
            incoming_moles[t].push((y+6,x+6));
        }

        let mut last_matrix: DpMatrix =  [[0; 32]; 32]; 

        // println!("{:?}",between_table );
        // println!("{:?}", incoming_moles);
        for moles in incoming_moles.iter().skip(1) {
            let mut temp: DpMatrix = [[0; 32]; 32];
            for (start, end) in between_table.keys() {
                if dist(*start, *end) > d as f64 {
                    continue;  
                }

                let mole_score =  between_table.get(&(*start, *end)).unwrap().iter().filter(|m| moles.contains(*m)).count();
                temp[start.1 as usize][start.0 as usize] = (mole_score as i32 + last_matrix[end.1 as usize][end.0 as usize]).max(temp[start.1 as usize][start.0 as usize]);
                temp[end.1 as usize ][end.0 as usize] = (mole_score as i32 + last_matrix[start.1 as usize][start.0 as usize]).max(temp[end.1 as usize ][end.0 as usize]);
            }

            // println!("");
            // print_upper_corner(&temp, 29);

            for x in 0..32 {
                for y in 0..32 {
                    last_matrix[x as usize][y as usize] = temp[x as usize][y as usize];
                }
            }
        }
        println!("{:?}", last_matrix.map(|f| f.iter().cloned().fold(0, i32::max)).iter().cloned().fold(0, i32::max) );
    }
}


