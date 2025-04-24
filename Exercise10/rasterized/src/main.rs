use std::{
    collections::{HashSet}, fs, io::{self, Read}, usize, vec
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "rasterized.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut tokens = content.split_whitespace();
    let mut next = || -> usize { tokens.next().unwrap().parse().unwrap() };
    let t = next();

    let primes = prime_sieve(10000000);

    for _ in 0..t {
        let n = next();
        let n_lines: usize = divisors(n, &primes).iter().map(|div| {
            totient(div + 1, &primes)
        }).sum(); 
        println!("{:?}", n_lines);
    }


}

fn divisors(mut n: usize, primes: &Vec<usize>) -> Vec<usize> {
    let mut divs: HashSet<usize> = HashSet::from([1]);

    for prime in primes {
        if n == 1 || prime * prime > n {
            break;
        }

        while n % prime == 0 {
            divs.extend(divs.iter().map(|div| div * prime).collect::<Vec<usize>>());
            n = n / prime
        }
    }

    if n != 1 {
        divs.extend(divs.iter().map(|div| div * n).collect::<Vec<usize>>());
    }
    
    let mut divs = divs.into_iter().collect::<Vec<usize>>();
    divs.sort();
    divs
}

fn totient(mut n: usize, primes: &Vec<usize>) -> usize {
    let mut result = n;

    for prime in primes {
        if n == 1 || prime * prime > n {
            break;
        }
        if n % prime != 0 {
            continue;
        }
        while n % prime == 0 {
            n = n / prime;
        }

        result -= result / prime;
    }
    if n > 1 {
        result -= result / n;
    }

    result
}

fn prime_sieve(n: usize) -> Vec<usize> {
    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;

    if n < 2 {
        return vec![];
    }

    let mut primes = vec![2];

    for i in (3..=n).step_by(2) {
        if is_prime[i] {
            
            primes.push(i);

            for j in ((i * i)..=n).step_by(i * 2) {
                is_prime[j] = false;
            }
        }
    }
    primes


}
