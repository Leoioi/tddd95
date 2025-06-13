use std::{
    fs, i128, io::{self, Read}, u128, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "chineseremainder.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);
    
    let mut tokens = content.split_whitespace();
    let mut next = || -> u128 { tokens.next().unwrap().parse().unwrap() };
    let t = next();

    for _ in 0..t {
        let (a, n, b ,m) = (next(), next(), next(), next());
        match chinese_remainder_theorem(vec![(a, n), (b, m)]) {
            Some(res) => println!("{:?} {:?}", res.0, res.1),
            None => println!("no solution")
        };
    }  
}


/*
 * This is an implementation of the chinese remainder theorem where we can take some number of
 * congurent equations of the form x \equiv a_i mod m_i, where (a_i, m_i) is some element in the
 * input vector.
 * 
 * The equation to calculate the remainder relies on Bezout identity which we can inturn calculate
 * with the extended version of eclideans algorithm.
 *
 * As in this case we dont assume that the moduli are co-prime we need an extra check to be sure
 * that there is a solution or not. We check if a_i \equiv a_j mod(gcd(m_i, m_j)) if this is the
 * case then there is also going to be a unique solution to for that pair of equations.
 * Consider that if the moduli really were co-prime gcd(m_i, m_j) = 1 and this check would always
 * pass.
 * 
 * If we find that there is no possible solution then we return None, else we return (x, new_mod)
 * where x value that satisfied all equations, new_mod is the interval of x
 *
 * Complexity, 
 * The time complextiy of this funciton is going to scale with the number of equations (e_n). For each
 * each equations in cong_equations we are going to have to calculate the Bezout coifficients with
 * the extended eclidean algorithm. The time complexity for the eclidean algorithm is going to be
 * O(log min(m_i, m_j)) where m_i, m_j are the inputs to the extended_euclidean function.
 * The final time complexity for this function is O(e_n log(m)) where m is the maximum is the
 * largest of all moduli.
 *
 */
fn chinese_remainder_theorem(cong_equations: Vec<(u128, u128)>) -> Option<(u128, u128)> {
    if cong_equations.len() == 1 {
        return Some(cong_equations[0]);
    }

    let mut sol = None;
    for (&(a,n), &(b, m)) in cong_equations.iter().zip(cong_equations.iter().skip(1)) {
        let (gcd, bez_cof1, bez_cof2) = extended_euclidean(n, m);

        if mod_opt(a as i128, gcd) != mod_opt(b as i128, gcd) {
            return None;
        }
        
        // Equivalent to lcm
        let new_mod = (m * n) / gcd;

        let x = mod_opt(((a as i128 * bez_cof1 * m as i128) + (b as i128 * bez_cof2 * n as i128)) / gcd as i128, new_mod);
        
        sol = Some((x, new_mod));
    }

    return sol;

}


fn extended_euclidean(a: u128, b: u128) -> (u128, i128, i128) {
    let (mut old_r, mut r) = (a as i128, b as i128);
    let (mut old_s, mut s) = (1,0);
    let (mut old_t, mut t) = (0,1);

    while r != 0 {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_s, s) = (s, old_s - quotient * s);
        (old_t, t) = (t, old_t - quotient * t);
    }
    (old_r as u128, old_t, old_s)
}


// As the % operator in Rust is for remainder not modulo we define out own here
fn mod_opt(a: i128, n: u128) -> u128 {
    let res = a % n as i128;
    if res < 0 {
        (res + n as i128) as u128
    }
    else {
        res as u128
    }
}


