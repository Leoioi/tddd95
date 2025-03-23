use core::{fmt, num};
use std::{fs, io::{self, Read}, mem::swap, usize, vec};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "class.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_> = content.lines();
    let first_line: Vec<&str> = lines.next().unwrap().split_whitespace().collect();
    let n: usize = first_line[1].parse().unwrap();
    let k: usize = first_line[0].parse().unwrap();

    let mut all_act: Vec<(i64, i64)> = vec![];

    for _ in 0..k {
        let line = lines.next().unwrap();
        let text_line: Vec<&str> = line.split_whitespace().collect();
        all_act.push((text_line[0].parse().unwrap(), text_line[1].parse().unwrap()));
    }
    all_act.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

    let mut all_class: Vec<i64> = vec![0; n];

    println!("{:?}", all_act);

    let mut num = 0;
    for activ in all_act {
        println!("{:?}", all_class);
        for class_i in 0..n {
            if all_class[class_i] < activ.0 {
                all_class[class_i] = activ.1; 
                num += 1;
                break;
            }
        }
    }

    println!("{:?}", num);

}


