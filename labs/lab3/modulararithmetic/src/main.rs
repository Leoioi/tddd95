use std::{
    fs, i128, io::{self, Read}, u128, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let file_path = "modulararithmetic.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    loop {
        let mut ni = lines.next().unwrap().split_whitespace();
        let n: u128 = ni.next().unwrap().parse::<u128>().unwrap();
        let i: usize = ni.next().unwrap().parse::<usize>().unwrap();

        if n == 0 && i == 0 {
            break;
        }

        for _ in 0..i {
            let mut line_cont = lines.next().unwrap().split_whitespace();
            let x = line_cont.next().unwrap().parse::<u128>().unwrap();
            let opt =  line_cont.next().unwrap();
            let y = line_cont.next().unwrap().parse::<u128>().unwrap();

            let res: i128 = match opt {
                "+" => mod_add(x, y, n) as i128,
                "-" => mod_sub(x, y, n) as i128,
                "*" => mod_mul(x, y, n) as i128,
                "/" => if let Some(res) = mod_div(x, y, n) {res as i128} else {-1} ,
                _ => panic!() // This should never happen so its fine =)
            };

            println!("{:?}", res);
        }
    }
}


fn mod_add(a: u128, b: u128, n: u128) -> u128 {
    (a + b) % n
}

fn mod_sub(mut a: u128, b: u128, n: u128) -> u128 {
    if a < b {
        a = a + (((b - a) / n) + 1) * n;
    }
    (a - b) % n
}

fn mod_mul(a: u128, b: u128, n: u128) -> u128 {
    (a * b) % n
}

fn mod_div(a: u128, b: u128, n: u128) -> Option<u128> {
    if let Some(b_inv) = modular_inverse(b, n) {
        Some(mod_mul(a, b_inv, n))
    }
    else {
        None
    }
}



fn modular_inverse(a: u128, b: u128) -> Option<u128> {
    let (mut old_r, mut r) = (a as i128, b as i128);
    let (mut old_s, mut s) = (1,0);
    let (mut old_t, mut t) = (0,1);

    while r != 0 {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_s, s) = (s, old_s - quotient * s);
        (old_t, t) = (t, old_t - quotient * t);
    }
    
    // There is no modular inverse 
    if old_r != 1 {
        return None;
    }

    // If the result is negative we need to make sure that the retrun is positive
    let res = old_s % b as i128;
    if  res < 0 {
        return Some((res + b as i128 ) as u128);
    }

    return Some(res as u128)
}
