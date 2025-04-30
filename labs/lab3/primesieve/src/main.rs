use std::{
    fs,
    io::{self, Read}, usize, vec,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "primesieve.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);
    
    let mut tokens = content.split_whitespace();
    let mut next = || -> usize{ tokens.next().unwrap().parse().unwrap() };
    let n = next();
    let q = next();
    
    let (primes, count) = prime_sieve(n);
    // The number of prime numbers less then or equal to n 
    println!("{:?}", count);
    for _ in 0..q {
       println!("{:?}", primes[next()] as usize);
    }
}


fn prime_sieve(n: usize) -> (BitVec, usize) {

    let mut is_prime = BitVec::new(n + 1 , true);
    // let mut is_prime = vec![true; n+1];
    //is_prime[0] = false;
    //is_prime[1] = false;
    
    let _ = is_prime.clear(0);
    let _ = is_prime.clear(1);

    let mut count = 0;

    for i in 2..=n {
        if is_prime[i] && i * i <= n {
            for j in ((i * i)..=n).step_by(i) {
                //is_prime[j] = false;
    
                if is_prime.get(j).unwrap() {
                    count += 1;
                }

                let _ = is_prime.clear(j);
            }
        }
    }

    count = n - count - 1; 

    (is_prime, count)
}

// As there is not standared implemntation of bitvec in rust i have one implemntated here
pub struct BitVec {
    storage: Vec<u64>,
    len: usize,
}

impl BitVec {
    pub fn new(len: usize, fill_value: bool) -> Self {
        let blocks = (len + 63) / 64;
        let storage = if !fill_value { vec![0; blocks] } else { vec![u64::MAX; blocks] };
        BitVec { storage, len }
    }

    pub fn with_capacity(bits: usize) -> Self {
        let blocks = (bits + 63) / 64;
        BitVec { storage: vec![0; blocks], len: bits } 
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get(&self, index: usize) -> Option<bool> {
        if index >= self.len {
            return None;
        }
        let block = index / 64;
        let offset = index % 64;
        let val = (self.storage[block] >> offset) & 1;
        Some(val == 1)
    }

    pub fn set(&mut self, index: usize) -> Result<(), &'static str> {
        if index >= self.len {
            return Err("Index out of bounds");
        }
        let block = index / 64;
        let offset = index % 64;
        self.storage[block] |= 1 << offset;
        Ok(())
    }

    pub fn clear(&mut self, index: usize) -> Result<(), &'static str> {
        if index >= self.len {
            return Err("Index out of bounds");
        }
        let block = index / 64;
        let offset = index % 64;
        self.storage[block] &= !(1 << offset);
        Ok(())
    }

    pub fn push(&mut self, value: bool) {
        let block = self.len / 64;
        let offset = self.len % 64;
        if block >= self.storage.len() {
            self.storage.push(0);
        }
        if value {
            self.storage[block] |= 1 << offset;
        }
        self.len += 1;
    }
}

use std::ops::Index;
impl Index<usize> for BitVec {
    type Output = bool;

    fn index(&self, idx: usize) -> &Self::Output {
        if self.get(idx).unwrap_or(false) {
            &true
        } else {
            &false
        }
    }
}
