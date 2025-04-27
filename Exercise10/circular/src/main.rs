
use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read},
    usize, vec,
};



fn main() {
    //let file_path = "circular.in";
    //let content = fs::read_to_string(file_path).expect("failed to read file");


    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);
    
    let mut tokens = content.split_whitespace();
    let mut next = || -> i64 { tokens.next().unwrap().parse().unwrap() };
    let k = next();

    for _ in 0..k {
        let a = next();
        let b = next();
        let q = next();
        let p = next();

        let c = next();
        let d = next();
        let r = next();
        let s = next();
        
        let res = (a - b - c + d) % gcd4(p, q, r, s);

        if res == 0 {
            println!("Yes");
        }
        else {
            println!("No");
        }
    }
}



fn gcd(a: i64, b: i64) -> i64 {
    let (mut m, mut n) = (a, b);
    while n != 0 {
        let r = m % n;
        m = n;
        n = r;
    }
    m
}

fn gcd4(a: i64, b: i64, c: i64, d: i64) -> i64 {
    let ab = gcd(a, b);
    let cd = gcd(c, d);
    gcd(ab, cd)
}

