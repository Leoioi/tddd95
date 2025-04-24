use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read}, usize, vec,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let file_path = "primesieve.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);
    
    let mut tokens = content.split_whitespace();
    let mut next = || -> usize{ tokens.next().unwrap().parse().unwrap() };
    let n = next();
    let q = next();

    print!("{:?}", prime_sieve(n))

}


fn prime_sieve(n: usize) -> Vec<bool> {
    let mut is_prime = vec![true; n+1];
    is_prime[0] = false;
    is_prime[1] = false;

    for i in 2..=n {
        if is_prime[i] && i * i <= n {
            for j in ((i * i)..=n).step_by(i) {
            //for (int j = i * i; j <= n; j += i)
                is_prime[j] = false;
            }
        }
    }
    is_prime
}

