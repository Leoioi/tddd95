use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read}, usize,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

const TOTAL: usize = 256; // The number of possible letters

fn main() {
    //let file_path = "dvaput.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    let i: usize = lines.next().unwrap().parse::<usize>().unwrap();

    for _ in 0..i {
        let mut line_cont = lines.next().unwrap().split_whitespace();
        let x_1 = line_cont.next().unwrap().parse::<i32>().unwrap();
        let x_2 = line_cont.next().unwrap().parse::<i32>().unwrap();

        let opt =  line_cont.next().unwrap();
        let y_1 = line_cont.next().unwrap().parse::<i32>().unwrap();
        let y_2 = line_cont.next().unwrap().parse::<i32>().unwrap();
        


        
    }
}

#[derive(Debug)]
struct Rational {
    numerator: i64,
    denominator: i64,
}

impl std::ops::Add<Rational> for Rational {
    type Output = Self;

    fn add (self, other: Self) -> Self {
        Rational { numerator: 1, denominator: 1 }
    }

    
}

