use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read},
    usize, vec,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let file_path = "rasterized.in";
    let content = fs::read_to_string(file_path).expect("failed to read file");

    //let mut buffer = vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("failed to read from stdin");
    //let content = string::from_utf8_lossy(&buffer);

    let mut tokens = content.split_whitespace();
    let mut next = || -> usize { tokens.next().unwrap().parse().unwrap() };
    let n = next();
    let q = next();
}

fn factorize(mut n: usize, primes: Vec<usize>) -> Vec<usize> {
    let mut factors = vec![];

    for prime in primes {
        if n == 1 || prime * prime > n {
            break;
        }

        while n % prime == 0 {
            factors.push(prime);
            n /= prime
        }
    }

    if n != 0 {
        factors.push(n);
    }

    factors
}

fn prime_sieve(n: usize) -> Vec<usize> {
    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;

    for i in 2..=n {
        if is_prime[i] && i * i <= n {
            for j in ((i * i)..=n).step_by(i) {
                is_prime[j] = false;
            }
        }
    }
    is_prime
        .iter()
        .enumerate()
        .filter(|(_, b)| **b)
        .map(|(i, _)| i)
        .collect::<Vec<usize>>()
}
