use std::{
    fs, i128,
    io::{self, Read},
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let file_path = "aspenavenue.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut tokens = content.split_whitespace();
    let mut next = || -> u32 { tokens.next().unwrap().parse().unwrap() };
    let n = next();
    let l = next();
    let w = next();

    for _ in 0..n {
        let tree = next();

    }
}


fn dist(p1: (u32,u32), p2: (u32, u32)) -> u32 {
    return (p1.0 - p2.0).pow(2) + (p1.1 - p2.1).pow(2);
}
