use std::{
    collections::{HashSet, VecDeque}, fs, io::{self, Read}, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "dvaput.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

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

            let res = match opt {
                "+" => mod_add(x, y, n),
                "-" => mod_sub(x, y, n),
                "*" => mod_mul(x, y, n),
                //"/" => rat / rat2,  
                _ => panic!() // This should never happen so its fine =)
            };
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

//fn mod_div(a: u128, b: u128, n: u128) -> u128 {
//}



fn extended_gcd(a: u128, b: u128) -> ((u128, u128), u128, (u128, u128)) {


}
