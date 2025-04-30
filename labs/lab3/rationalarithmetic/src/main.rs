use core::num;
use std::{
    collections::{HashSet, VecDeque}, fs, i128, io::{self, Read}, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "rationalarithmetic.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    let i: usize = lines.next().unwrap().parse::<usize>().unwrap();

    for _ in 0..i {
        let mut line_cont = lines.next().unwrap().split_whitespace();
        let x_1 = line_cont.next().unwrap().parse::<i128>().unwrap();
        let x_2 = line_cont.next().unwrap().parse::<i128>().unwrap();

        let opt =  line_cont.next().unwrap();
        let y_1 = line_cont.next().unwrap().parse::<i128>().unwrap();
        let y_2 = line_cont.next().unwrap().parse::<i128>().unwrap();
        
        let rat = Rational {numerator: x_1, denominator: x_2};
        let rat2 = Rational {numerator: y_1, denominator: y_2};

        //println!("{:?}", rat + rat2);

        let res = match opt {
            "+" => rat + rat2,  
            "-" => rat - rat2,  
            "*" => rat * rat2,  
            "/" => rat / rat2,  
            _ => panic!() // This should never happen so its fine =)
        };

        println!("{:?} / {:?}", res.numerator, res.denominator);
    }
}

#[derive(Debug, Eq)]
struct Rational {
    numerator: i128,
    denominator: i128,
}

impl Rational {
    pub fn new(numerator: i128, denominator: i128) -> Self {
        let greatest_commnon_denominator = gcd(numerator, denominator);
        let (numerator, denominator) = (numerator / greatest_commnon_denominator, denominator / greatest_commnon_denominator);
        let (numerator, denominator) = if denominator < 0 { (-numerator, -denominator) } else { (numerator, denominator) };
        Rational {numerator, denominator}
    }
}

// Overload (+) operator 
impl std::ops::Add<Rational> for Rational {
    type Output = Self;

    fn add (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.denominator + other.numerator * self.denominator;
        let new_denominator = self.denominator * other.denominator;
        Rational::new(new_numerator, new_denominator)
    }
}


// Overload (-) operator
impl std::ops::Sub<Rational> for Rational {
    type Output = Self;

    fn sub (self, other: Self) -> Self {
        self +  Rational { numerator: (-1) * other.numerator, denominator: other.denominator} 
    }
}

// Overload (/) operator
impl std::ops::Div<Rational> for Rational {
    type Output = Self;

    fn div (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.denominator;
        let new_denominator = self.denominator * other.numerator;
        Rational::new(new_numerator, new_denominator)
    }
}

// Overload (*) operator
impl std::ops::Mul<Rational> for Rational {
    type Output = Self;

    fn mul (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.numerator;
        let new_denominator = self.denominator * other.denominator;
        Rational::new(new_numerator, new_denominator)
    }
}

impl PartialEq for Rational {
    fn eq(&self, other: &Self) -> bool {
        self.numerator == other.numerator && self.denominator == self.denominator        
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_numerator = self.numerator * other.denominator;
        let other_numerator = other.numerator * self.denominator;
        
        self_numerator.partial_cmp(&other_numerator)
    }
}
impl Ord for Rational {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_numerator = self.numerator * other.denominator;
        let other_numerator = other.numerator * self.denominator;
        
        self_numerator.cmp(&other_numerator)
    }
}

fn gcd(a: i128, b: i128) -> i128 {
    let (mut m, mut n) = if a > b { (a, b) } else { (b, a) };
    while n != 0 {
        let r = m % n;
        m = n;
        n = r;
    }
    m
}
