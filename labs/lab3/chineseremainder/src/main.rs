use std::{
    fs, i128, io::{self, Read}, u128, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "chineseremainder.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);
    
    let mut tokens = content.split_whitespace();
    let mut next = || -> u128 { tokens.next().unwrap().parse().unwrap() };
    let t = next();

    for _ in 0..t {
        let (a, n, b ,m) = (next(), next(), next(), next());
        match chinese_remainder_theorem(vec![(a, n), (b, m)]) {
            Some(res) => println!("{:?} {:?}", res.0, res.1),
            None => println!("no solution")
        };
    }  
}

fn chinese_remainder_theorem(mut cong_equations: Vec<(u128, u128)>) -> Option<(u128, u128)> {
    if cong_equations.len() == 1 {
        return Some(cong_equations[0]);
    }

    let mut sol = None;
    for (&(a,n), &(b, m)) in cong_equations.iter().zip(cong_equations.iter().skip(1)) {
        let (gcd, bez_cof1, bez_cof2) = extended_euclidean(n, m);

        if mod_opt(a as i128, gcd) != mod_opt(b as i128, gcd) {
            return None;
        }
        
        // Equivalent to lcm
        let new_mod = (m * n) / gcd;

        let x = mod_opt(((a as i128 * bez_cof1 * m as i128) + (b as i128 * bez_cof2 * n as i128)) / gcd as i128, new_mod);
        
        sol = Some((x, new_mod));
    }

    return sol;

}


fn extended_euclidean(a: u128, b: u128) -> (u128, i128, i128) {
    let (mut old_r, mut r) = (a as i128, b as i128);
    let (mut old_s, mut s) = (1,0);
    let (mut old_t, mut t) = (0,1);

    while r != 0 {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_s, s) = (s, old_s - quotient * s);
        (old_t, t) = (t, old_t - quotient * t);
    }
    (old_r as u128, old_t, old_s)
}


// As the % operator in Rust is not quite a modulo we define out own here
fn mod_opt(a: i128, n: u128) -> u128 {
    let res = a % n as i128;
    if res < 0 {
        (res + n as i128) as u128
    }
    else {
        res as u128
    }
}


