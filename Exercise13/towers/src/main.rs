use core::{f64, str};
use std::{
    cmp::Ordering,
    fs,
    io::{self, Read},
    result, u64, usize,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "towers.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    let n = lines.next().unwrap().parse::<usize>().unwrap();
    //let mut towers: Vec<((Vec<u64>, (f64, u64)), usize)> = vec![];
    let mut towers: Vec<((Vec<u64>, usize, f64), usize)> = vec![];
    let mut og_powers = vec![];
    for i in 0..n {
        let power_text = lines.next().unwrap();

        let mut powers = power_text
            .split("^")
            .map(|str_num| str_num.parse::<u64>().unwrap())
            .take_while(|&exp| exp != 1)
            .collect::<Vec<u64>>();
        if powers.is_empty() {
            powers = vec![1];
        };

        og_powers.push(power_text);
        towers.push((leo_tower_form(powers), i));
    }

    towers.sort_by(|(a, _), (b, _)|{ leo_cmp(a, b) });

    println!("Case 1:");
    for tower in towers {
        println!("{}", og_powers[tower.1])
    }
}

fn leo_cmp(mixed_tower1: &(Vec<u64>, usize,  f64), mixed_tower2: &(Vec<u64>,usize, f64)) -> Ordering {
    if mixed_tower1.1 != mixed_tower2.1 {
        return if mixed_tower1.1 > mixed_tower2.1 {Ordering::Greater} else {Ordering::Less};
    }

    if mixed_tower1.2 != mixed_tower2.2 {
        return if mixed_tower1.2 > mixed_tower2.2 {Ordering::Greater} else {Ordering::Less};
    }

    // down o
    if let Some(first_unsame) = mixed_tower1
        .0
        .iter()
        .rev()
        .zip(mixed_tower2.0.iter().rev())
        .skip_while(|(exp1, exp2)| exp1 == exp2)
        .next()
    {
        return if first_unsame.0 > first_unsame.1 {Ordering::Greater} else {Ordering::Less} ;
    }

    Ordering::Equal
}

// a^b^c^d => log(log(a^b^c^d)) = log( log(a) * b^c^d) = log(log(a)) + log(b)*c^d
// a^b^c => log(log(a^b^c)) = log( log(a) * b^c) = log(log(a)) + log(b)*c
fn leo_tower_form(powers: Vec<u64>) -> (Vec<u64>, usize, f64) {
    let threshold: f64 = f64::MAX.log2();

    let mut c: f64 = 1.0;

    //while let Some(&b) = powers_iter.next() {
    for (i,&b) in powers.iter().enumerate().rev() {

        if c * (b as f64).log2() as f64 > threshold {

            if i == 0 {
                return (vec![], 1 ,c * (b as f64).log2());
            }
            else {
                return (powers[..i].to_vec(), i + 1, (powers[i - 1] as f64).log2().log2() + ((b as f64).log2() * c));
            }
        }
        c = (b as f64).powf(c);
    }
    return (vec![], 0,  c);
}
