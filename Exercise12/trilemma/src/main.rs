use std::{
    fs, i128,
    io::{self, Read},
    u128, usize,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "trilemma.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut tokens = content.split_whitespace();
    let mut next = || -> i32 { tokens.next().unwrap().parse().unwrap() };
    let n = next();
    for i in 0..n {
        let (x1, x2, y1, y2, z1, z2) = (next(), next(), next(), next(), next(), next());
        let (d_xy, d_yz, d_zx) = (
            ((x1 - y1).pow(2) + (x2 - y2).pow(2)),
            ((y1 - z1).pow(2) + (y2 - z2).pow(2)),
            ((z1 - x1).pow(2) + (z2 - x2).pow(2)),
        );
        //if (x1*y2) + (y1*z2) + (z1*x1) - (x1*z2) - (y1*x2) - (z1*y2) == 0 {
        if x1 * (y2 - z2) + y1 * (z2 - x2) + z1 * (x2 - y2)  == 0 {
            println!("Case #{:?}: not a triangle", i+1);
            continue;
        }

        let mut lengths = vec![d_xy, d_yz, d_zx];
        lengths.sort();
        let tri_type = if lengths[0] == lengths[1] || lengths[1] == lengths[2] {
            "isosceles"
        } else {
            "scalene"
        };
        let py_hyp = lengths[0] + lengths[1];
        let angle_type = if py_hyp == lengths[2] {
            "right"
        } else if py_hyp > lengths[2] {
            "acute"
        } else {
            "obtuse"
        };

        println!("Case #{:?}: {} {} triangle", i+1, tri_type, angle_type);
    }
}
