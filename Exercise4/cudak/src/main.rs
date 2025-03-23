use core::{fmt, num};
use std::{fs, io::{self, BufRead, Read}, u64, usize};

type DpTable = [[[u64; 2]; 17]; 136]; // [sum][pos][is_bounded]



fn main() {
    let file_path = "cudak.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut input= content.lines().next().unwrap().split_whitespace().map(|n| n.parse::<u64>().unwrap()); 

    let A = input.next().unwrap() as usize;
    let B = input.next().unwrap() as usize;
    let S = input.next().unwrap() as usize;

    let first_count  = sum_count(S, B);
    let second_count  = sum_count(S, A - 1);
    let total =first_count -second_count;  

    let mut digits: Vec<u64> = ( vec![0u64; B.to_string().chars().count() - A.to_string().chars().count()].into_iter().chain(A.to_string().chars().map(|c| c.to_digit(10).unwrap() as u64))).rev().collect();    
    let mut current_sum: u64 = digits.clone().iter().sum();
    current_sum = S as u64 - current_sum;

    for digit in digits.iter_mut() {
        if current_sum == 0 {
            break;
        }
        let digit_diff = 9-*digit;
        if  digit_diff < current_sum {
            *digit += digit_diff;
            current_sum -= digit_diff;
        }
        else {
            *digit += current_sum;
            current_sum -= current_sum;
        }

    }
    digits.reverse();

    let smallest_sum_num = digits.iter().fold(0, |acc, &digit| (acc * 10) + digit);

    println!("{:?}", total);
    println!("{:?}", smallest_sum_num)

}

fn sum_count(S: usize, max_size: usize) -> u64 {
    let digits: Vec<u32> = max_size.to_string().chars().map(|c| c.to_digit(10).unwrap()).collect();

    let mut table: DpTable  = [[[0; 2]; 17]; 136];
    
    table[0][0][1] = 1;
    // init 

    for sum_i in 0..=S {
        for pos_i in 0..digits.len() {
            for is_bounded in 0..=1 {
                let dp_entry_count = table[sum_i][pos_i][is_bounded];
                if dp_entry_count == 0 {
                    continue;
                }
                let upper_bound = if is_bounded == 1 {digits[pos_i]} else {9};
                for digit in 0..=upper_bound {
                    let is_next_bounded = is_bounded==1 && digit == upper_bound;
                    let new_sum = sum_i + digit as usize;

                    if new_sum > S {
                        continue;
                    }   
 
                }
            }
        }
    }


    
    for sum_i in 0..=S {
        for pos_i in 0..digits.len() {
            table[sum_i ][pos_i][0] +=  table[sum_i ][pos_i][1];
            table[sum_i ][pos_i][1] = 0;
        }
    }
    
    
    let total = table[S][digits.len()][0];

    total
    }


