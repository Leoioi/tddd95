use std::{fs, io::{self, Read}};


fn main() {
    let file_path = "perfectp.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);
    
    let lines = content.lines().map(|n| n.parse::<i64>().unwrap());

    let mut result = String::from("");

    for number in lines {
        if number == 0 {
            break;
        }

        let p = perfect_p_power(number);

        result.push_str(&(p.to_string() + "\n" ));

    }
    println!("{}", result);
}

fn perfect_p_power (number: i64) -> i64 {
    for b in 2..=(number.abs() as f64).sqrt() as i64 { // the base will start at 2 
        let mut x = number.abs();
        let mut p = 0; 

        while x % b == 0 {
            x = x / b;
            p += 1;
        }

        if x != 1 {
            continue;
        }

        if number > 0 {
            return p;
        } 

        if p % 2 == 1 {
            return p;
        }
    }
    return 1;
}