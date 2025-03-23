use std::{env::current_exe, fs, io::{self, Read}, result};


fn main() {
    let file_path = "five.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut weigths: Vec<u64>  = content.lines().skip(1).map(|s| s.parse::<u64>().unwrap()).collect();
    let n_weigths = weigths.len();
    let mut table: Vec<Vec<u64>> = vec![vec![0; n_weigths + 1  as usize]; n_weigths + 1 ];
    weigths.push(0);
    let mut result = String::from("");

    //table[0][0] = 1;

    let mut close = 0;

    for w_i in 0..n_weigths {
        table[1][ 1 + w_i] =  weigths[w_i]
    }
    let mut best = 0;

    for w_s in 0..n_weigths {
        for w_ii in 0..n_weigths {
            if ((table[w_s][w_ii] + weigths[w_ii]) as i64 - 1000).abs() == (table[w_s][w_ii] as i64 - 1000).abs()  {
                if ((table[w_s][w_ii] + weigths[w_ii]) as i64 - 1000) > (table[w_s][w_ii] as i64 - 1000) {
                    table[w_s + 1][w_ii + 1] =  table[w_s][w_ii] + weigths[w_ii];
                }
            }
            else if ((table[w_s][w_ii] + weigths[w_ii]) as i64 - 1000).abs() < (table[w_s][w_ii] as i64 - 1000).abs() {
                table[w_s + 1][w_ii + 1] =  table[w_s][w_ii] + weigths[w_ii];
            }
            else {
                table[w_s + 1][w_ii + 1] =  table[w_s][w_ii + 1];
            }


            if (table[w_s + 1][w_ii + 1] as i64 - 1000).abs() == (best as i64 - 1000).abs() {
                if (table[w_s + 1][w_ii + 1] as i64 - 1000) > (best as i64 - 1000) {
                    best = table[w_s + 1][w_ii + 1] as i64;
                }
            }
            else {
                if (table[w_s + 1][w_ii + 1] as i64 - 1000).abs() < (best as i64 - 1000).abs() {
                    best = table[w_s + 1][w_ii + 1] as i64;
                }
            }
        }
    }



    println!("{:?}", best);




}
