use std::{fs, io::{self, Read}, result};

const NUMBER_MAP: [u16; 7]  = [0, 1, 2, 5, 9, 8, 6];


fn main() {
    let file_path = "ignoregrabage.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_> = content.lines();

    let mut result = String::from("");

    for line in lines {
        let mut serving_num: usize = line.parse().unwrap();

        while serving_num != 0 {
            let base_7_num: usize = serving_num % 7;
            serving_num = serving_num / 7;
            result.push_str( &(NUMBER_MAP[base_7_num].to_string()) );
        }
        result.push_str( &("\n") );
    }

    println!("{}", result);


}