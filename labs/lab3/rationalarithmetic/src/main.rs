use std::{
    fs, io::{self, Read}
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let file_path = "rationalarithmetic.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    let i: usize = lines.next().unwrap().parse::<usize>().unwrap();

    for _ in 0..i {
        let mut line_cont = lines.next().unwrap().split_whitespace();
        let x_1 = line_cont.next().unwrap().parse::<i128>().unwrap();
        let x_2 = line_cont.next().unwrap().parse::<i128>().unwrap();

        let opt =  line_cont.next().unwrap();
        let y_1 = line_cont.next().unwrap().parse::<i128>().unwrap();
        let y_2 = line_cont.next().unwrap().parse::<i128>().unwrap();
        
        let rat1 = Rational {numerator: x_1, denominator: x_2};
        let rat2 = Rational {numerator: y_1, denominator: y_2};

        let res = match opt {
            "+" => rat1 + rat2,  
            "-" => rat1 - rat2,  
            "*" => rat1 * rat2,  
            "/" => rat1 / rat2,  
            _ => panic!() // This should never happen so its fine =)
        };

        println!("{}", res);
    }
}

#[derive(Debug, Eq)]
struct Rational {
    numerator: i128,
    denominator: i128,
}

impl Rational {
    /**
    * Here we define a constructor for the Rational struct/(class) here every time we crate a
    * rational number from a numerator and denominator we alos didvide wit the gcd to normalize the
    * new rational.
    *
    * = Complexity as we call gcd from the normalization we have a complexity of
    * O(log(min(numerator, denominator)))
    */ 
    pub fn new(numerator: i128, denominator: i128) -> Self {
        let greatest_commnon_denominator = gcd(numerator, denominator);
        let (numerator, denominator) = (numerator / greatest_commnon_denominator, denominator / greatest_commnon_denominator);
        let (numerator, denominator) = if denominator < 0 { (-numerator, -denominator) } else { (numerator, denominator) };
        Rational {numerator, denominator}
    }

    /**
    * Given a string of the format "a / b" we interpret this as a rational number
    */
    pub fn new_from_str(s: &str) -> Rational {
        let mut container = s.split_whitespace();
        let a = container.next().unwrap().parse::<i128>().unwrap();
        container.next(); // Skip the "/" character
        let b = container.next().unwrap().parse::<i128>().unwrap();
        Rational::new(a, b) 
    }
}

/** 
* Overloads the display function for to_string 
* With this we are able to use the {} formatter
*/
impl std::fmt::Display for Rational {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} / {}", self.numerator, self.denominator)
    }
}

/** 
* Overload (+) operator 
*
* = Complexity is going to be O(log(min(self.numerator * other.denominator + other.numerator * self.denominator;
, self.denominator * other.denominator))) as we use gcd to normalize the rational
*/
impl std::ops::Add<Rational> for Rational {
    type Output = Self;

    fn add (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.denominator + other.numerator * self.denominator;
        let new_denominator = self.denominator * other.denominator;
        Rational::new(new_numerator, new_denominator)
    }
}

/** 
* Overload (-) operator
*
* = Complexity is going to be O(log(min(self.numerator * other.denominator + other.numerator * self.denominator;
, self.denominator * other.denominator))) as we call the (+) operator here.
*/
impl std::ops::Sub<Rational> for Rational {
    type Output = Self;

    fn sub (self, other: Self) -> Self {
        self +  Rational { numerator: (-1) * other.numerator, denominator: other.denominator} 
    }
}

/**
* Overload (/) operator
*
* = Complexity is going to be O(log(min(self.numerator * other.denominator, self.denominator *
* other.numerator) as we normalize with gcd when we call Rational::new()
*/
impl std::ops::Div<Rational> for Rational {
    type Output = Self;

    fn div (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.denominator;
        let new_denominator = self.denominator * other.numerator;
        Rational::new(new_numerator, new_denominator)
    }
}


/**
* Overload (*) operator
*
* = Complexity is going to be O(log(min(self.numerator * other.numerator, self.denominator *
* other.denominator) as we normalize with gcd when we call Rational::new()
*/
impl std::ops::Mul<Rational> for Rational {
    type Output = Self;

    fn mul (self, other: Self) -> Self {
        let new_numerator = self.numerator * other.numerator;
        let new_denominator = self.denominator * other.denominator;
        Rational::new(new_numerator, new_denominator)
    }
}

/** 
* Overload (!=, ==) operator
* 
* = Complexity, Constant O(1)
*/
impl PartialEq for Rational {
    fn eq(&self, other: &Self) -> bool {
        self.numerator == other.numerator && self.denominator == self.denominator        
    }
}

/** 
* Overload (<, <=, >, and >=) operator
* 
* = Complexity, Constant O(1)
*/
impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_numerator = self.numerator * other.denominator;
        let other_numerator = other.numerator * self.denominator;
        
        self_numerator.partial_cmp(&other_numerator)
    }
}

/**
* Overload (<, <=, >, and >=) operator and indicate and equivlents relations
* = Complexity, Constant O(1)
*/
impl Ord for Rational {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_numerator = self.numerator * other.denominator;
        let other_numerator = other.numerator * self.denominator;
        
        self_numerator.cmp(&other_numerator)
    }
}

/*
* Simple implementation of a gcd function, 
*
* = Parameters 
* a and b, are intergers 
*
* = Complexity,
* O(log(min(a,b)))
*
*/
fn gcd(a: i128, b: i128) -> i128 {
    let (mut m, mut n) = if a > b { (a, b) } else { (b, a) };
    while n != 0 {
        let r = m % n;
        m = n;
        n = r;
    }
    m
}
