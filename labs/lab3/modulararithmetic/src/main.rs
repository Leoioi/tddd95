use std::{
    collections::{HashSet, VecDeque}, fs, intrinsics::breakpoint, io::{self, Read}, usize
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
        let n: usize = ni.next().unwrap().parse::<usize>().unwrap();
        let i: usize = ni.next().unwrap().parse::<usize>().unwrap();

        if n == 0 && i == 0 {
            break;
        }

        for _ in 0..i {
            let mut line_cont = lines.next().unwrap().split_whitespace();
            let x_1 = line_cont.next().unwrap().parse::<i32>().unwrap();
            let opt =  line_cont.next().unwrap();
            let y_1 = line_cont.next().unwrap().parse::<i32>().unwrap();
        }
    }
}
