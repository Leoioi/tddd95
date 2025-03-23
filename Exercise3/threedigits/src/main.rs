use std::{env::current_exe, fs, io::{self, Read}, result};


fn main() {
    let file_path = "threedigits.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut factorial_num: u64 = content.lines().next().unwrap().parse().unwrap();

    let mut current_num: u64 = 1;

    for i in 1..factorial_num + 1  {
        current_num *= i ;
        while current_num % 10 == 0 {
            current_num /= 10;
        }
        current_num %= 1000000;
    }

    println!("{}", current_num.to_string().chars().rev().take(3).collect::<Vec<_>>().into_iter().rev().collect::<String>() );
}