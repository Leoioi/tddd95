use core::{fmt, num};
use std::{fs, io::{self, Read}, mem::swap, result, usize, vec};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "six.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut line  = content.lines().next().unwrap().split_whitespace(); 
    let mut num= line.next().unwrap().split(".");

    let whole: i64 = num.next().unwrap().parse().unwrap();
    let decimals = num.next().unwrap();
    let total_size: u32 = decimals.len() as u32;
    let decimals: i64 = decimals.parse().unwrap();
    let rip: u32 = line.next().unwrap().parse().unwrap();

    let mut smallest_denominator = i64::MAX;
    let mut smallest_numerator = 0;

    let mut result = String::from("");

    let mut current_denominator = (10_i64.pow(total_size) - 10_i64.pow(total_size - rip)) ;
    let mut current_numerator =  decimals - (decimals / 10_i64.pow(rip) as i64);
    let current_gcd = gcd(current_denominator, current_numerator );

    current_denominator /= current_gcd;
    current_numerator /= current_gcd;

    if current_denominator <= smallest_denominator  {
        smallest_denominator = current_denominator;
        smallest_numerator = current_numerator;
    }

    result.push_str(&((smallest_denominator*whole + smallest_numerator).to_string() + "/" + &smallest_denominator.to_string() + "\n"));

    println!("{}", result);

}


fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}
