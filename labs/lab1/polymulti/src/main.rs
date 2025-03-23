use std::f64::consts::PI;
use std::io::{self, Read};
use std::{fs, sync::mpsc::SendError};

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

/*
For this exercise we have to multiply two polynomials together and output a new polynomial 
that is the product of the two inputs. 
To do this we are going to transform both of the polynomials to frequency domain using the 
fft (Fast Fourier Transform). When this is complete we can simply multiply each of the coefficients
of the two polynomials together (instead of performing a convolution over the two sets of coefficients)
 */
fn main() {
    let file_path = "polymulti.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines= content.lines().skip(1);

    // Prase out the first degrees and coefficients for both polynomials
    let first_degree: usize = lines.next().unwrap().parse().unwrap();
    let first_coefficients: Vec<c64> = lines.next().unwrap().split_whitespace().map(|s| s.parse().unwrap()).map(|n| c64::new(n, 0.0)).collect();

    let second_degree: usize = lines.next().unwrap().parse().unwrap();
    let second_coefficients: Vec<c64> = lines.next().unwrap().split_whitespace().map(|s| s.parse().unwrap()).map(|n| c64::new(n, 0.0)).collect();

    // Init the variables we will be sorting the dft
    let mut first_dft: Vec<c64> = first_coefficients.clone();
    let mut second_dft: Vec<c64> = second_coefficients.clone();

    let mut n: usize = 1;

    // Keep performing bit shift operations (to the power of two) until the size of n is bigger then sum of degree for both polynomials 
    // this ensures sufficient room for convolution
    while n < first_coefficients.len() + second_coefficients.len() {
        n <<= 1;
    }

    first_dft.resize(n, c64::new(0.0, 0.0));
    second_dft.resize(n, c64::new(0.0, 0.0));

    fft(&mut first_dft, false);
    fft(&mut second_dft, false);

    // Perform pointwise multiplication of the transformed coefficients.
    // This corresponds to polynomial multiplication in the original domain i.e convolution of all the coefficients.
    for i in 0..n {
        first_dft[i] =  first_dft[i] * second_dft[i]
    }

    // Now transform back to the original domain using the inverse of the fft 
    fft(&mut first_dft, true);

    let resulting_degree = first_degree + second_degree;
    let result: String = first_dft.iter().map(|c| c.real.round().to_string()).take(resulting_degree + 1).collect::<Vec<String>>().join(" ");
    println!("{}", (resulting_degree).to_string());
    println!("{}", result);


}
/* 
This is a recursive divide and conquer implementation of fft (Fast Fourier Transform) this will take some 
polynomial (represented as a vector of its coefficients) in time domain and transform it to its equivalent 
in frequency domain. Or the other way around if the is_invert flag is set to true.

-- Complexity
Consider that for each call of this function we are going to make 2 addition recursive calls and each 
of those will have a halved input size.
If T(n) is the time complexity for input size of n, Then we can model this functions time complexity as 
T(n) = 2*T(n/2) + poly(n)
Where n is the input size i.e the number coefficients, and poly is some time component of polynomial time complexity  
Using the Master theorem we get a time complexity of O(n log n)
 */
fn fft (poly: &mut Vec<c64>, is_invert: bool) {
    let n = poly.len();

    // Base case, the polynomial has a degree of 1 
    if n == 1 {
        return;
    }

    // Create two vectors to hold the even-indexed and odd-indexed coefficients.
    let mut poly0: Vec<c64> = vec![c64::new(0.0, 0.0) ; n / 2];
    let mut poly1: Vec<c64> = vec![c64::new(0.0, 0.0) ; n / 2];

    // Split the polynomial into its even and odd components.
    for i in 0..(n/2) {
        poly0[i] = poly[2*i];
        poly1[i] = poly[(2*i) + 1];
    }

    // Recursive call to compute FFT on both halves.
    fft(&mut poly0, is_invert);
    fft(&mut poly1, is_invert);

    // Calculate the angle for the primitive n-th root of unity.
    let ang: f64 = 2.0 * PI / (n as f64 * {if is_invert {-1.0} else {1.0}});

    let mut w: c64 = c64::new(1.0, 0.0);
    let wn: c64 = c64::new( ang.cos(), ang.sin());

    // Combine the results from the two recursive calls 
    for i in 0..(n/2) {
        poly[i] = poly0[i] + w * poly1[i];
        poly[i + n/2] = poly0[i] - w * poly1[i];

        // If performing the inverse FFT, divide each term by 2 
        if is_invert {
            poly[i] = poly[i] / c64::new(2.0, 0.0);
            poly[i + n/2] = poly[i + n/2] / c64::new(2.0, 0.0);
        }
        w = w * wn;
    } 


}

// Below follows a simple implementation of complex number, as they are not part of the std i rust =(

type c64 = Complex;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Complex {
    real: f64,
    imag: f64,
}

use std::ops::{Add, Mul, Sub, Div};

impl Complex {
    fn new(r: f64, i: f64) -> Complex {
        Complex {real: r, imag: i}
    }
}

impl Mul for Complex {
    type Output = Complex;

    fn mul(self, rhs: Self) -> Self::Output {
        Complex {
            real: (self.real * rhs.real) - (self.imag * rhs.imag),
            imag: (self.real * rhs.imag) + (self.imag * rhs.real),
        }
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, rhs: Self) -> Self::Output {
        Complex {
            real: (self.real + rhs.real),
            imag: (self.imag + rhs.imag),
        }
    }
}

impl Sub for Complex {
    type Output = Complex;

    fn sub(self, rhs: Self) -> Self::Output {
        Complex {
            real: self.real - rhs.real,
            imag: self.imag - rhs.imag,
        }
    }
}

impl Div for Complex {
    type Output = Complex;

    fn div(self, rhs: Self) -> Self::Output {
        let denominator = rhs.real * rhs.real + rhs.imag * rhs.imag;
        if denominator == 0.0 {
            panic!("Cannot divide by zero (Complex division by zero)");
        }
        Complex {
            real: (self.real * rhs.real + self.imag * rhs.imag) / denominator,
            imag: (self.imag * rhs.real - self.real * rhs.imag) / denominator,
        }
    }
}
