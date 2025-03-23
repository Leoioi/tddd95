use std::{env::current_exe, fs, io::{self, Read}, result};


fn main() {
    let file_path = "deadfraction.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_> = content.lines();
    let mut result = String::from("");

    for line in lines {
        if line == "0" {
            break;
        }
        
        let mut decimals: i64 = line.split(".").nth(1).unwrap().parse().unwrap();
        let total_size = line.split(".").nth(1).unwrap().len() as u32;

        let mut smallest_denominator = i64::MAX;
        let mut smallest_numerator = 0;

        for repeating_size in 1..total_size + 1 {

            let mut current_denominator = (10_i64.pow(total_size) - 10_i64.pow(total_size - repeating_size)) ;
            let mut current_numerator =  decimals - (decimals / 10_i64.pow(repeating_size) as i64);
            let current_gcd = gcd(current_denominator, current_numerator );

            current_denominator /= current_gcd;
            current_numerator /= current_gcd;

            if current_denominator <= smallest_denominator  {
                smallest_denominator = current_denominator;
                smallest_numerator = current_numerator;
            }
        }
        result.push_str(&(smallest_numerator.to_string() + "/" + &smallest_denominator.to_string() + "\n"));
    } 

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

    