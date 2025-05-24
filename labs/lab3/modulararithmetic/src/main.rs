use std::{
    fs, i128, io::{self, Read}
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "modulararithmetic.in";
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


/**
* Calculate addition between a and b, where  a,b in Z/aZ 
* = Complexity constant O(1)
*/
fn mod_add(a: u128, b: u128, n: u128) -> u128 {
    (a + b) % n
}

/**
* Calculates inverse addition (subtraction) between a and b where a, b in Z/aZ
* = Complexity constant O(1)
*/
fn mod_sub(mut a: u128, b: u128, n: u128) -> u128 {
    if a < b {
        a = a + (((b - a) / n) + 1) * n;
    }
    (a - b) % n
}

/**
* Calculates the product between a and b where a,b in Z/aZ 
* = Complexity constant O(1)
*/
fn mod_mul(a: u128, b: u128, n: u128) -> u128 {
    (a * b) % n
}

/**
* Calculates the modular divition of a and b with a, b in Z/aZ
* It dose this by calculating the modular inverse and multiplying with that inverse to achive the
* same result as divition 
*
* = Complexity
* As we utalize the modular_inverse function this function is also 
* O(log(min(a,b)))
*
*/
fn mod_div(a: u128, b: u128, n: u128) -> Option<u128> {
    if let Some(b_inv) = modular_inverse(b, n) {
        Some(mod_mul(a, b_inv, n))
    }
    else {
        None
    }
}


/**
* An implementation of the extended euclidian algoritms. That algoritm will calculate the gcd
* between the input a and b but also Bezout's identity coefficents in the form of old_ab and old_b.
* 
* From this we can get the modular inverse if we consider the Bezout's identity gives that if a and
* b are coprimer then,
*  s * a + t * b = 1  
* Where s and t are some integer and the coefficents for Bezout's identity 
* If that is the case then 
* t * b equiv 1 mod a
*
* As such t is the inverse for b in the group Z/aZ
* 
* = Complexity
* As this is simply an expanded gcd function it also has the complexity of 
* O(log(min(a, b))
* 
*
*/
fn modular_inverse(a: u128, b: u128) -> Option<u128> {
    let (mut old_r, mut r) = (a as i128, b as i128);
    let (mut old_ab, mut ab) = (1,0);
    let (mut old_bb, mut bb) = (0,1);

    while r != 0 {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_ab, ab) = (ab, old_ab - quotient * ab);
        (old_bb, bb) = (bb, old_bb - quotient * bb);
    }
    
    // There is no modular inverse, i.e gcd(a,b) != 1 
    if old_r != 1 {
        return None;
    }

    // If the result is negative we need to make sure that the return is positive
    let res = old_ab % b as i128;
    if  res < 0 {
        return Some((res + b as i128 ) as u128);
    }

    return Some(res as u128)
}
