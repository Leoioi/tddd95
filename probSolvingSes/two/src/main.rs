use std::{env::current_exe, fs, io::{self, Read}, result};


fn main() {
    let file_path = "two.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_> = content.lines();
    let mut result = String::from("");

    let ds: u32 = lines.next().unwrap().parse().unwrap();

    for _ in 0..ds {
        let mut first_line = lines.next().unwrap().split_whitespace();
        let n: usize  = first_line.next().unwrap().parse().unwrap();
        let m: u64 = first_line.next().unwrap().parse().unwrap();

        let mut all_ms: Vec<usize> = vec![];

        let nums_it = lines.next().unwrap().split_whitespace().map(|n| n.parse::<u64>().unwrap());
        let mut nums = vec![0;n as usize];

        for (i, num,) in nums_it.enumerate() {   
            if num == m {
                all_ms.push(i);
            }
            nums[i] = num;
        }


        let mut best_sum = 0;
        for i_m in all_ms {
            let mut tot_sum = 0;
            let mut cur_i_p: isize  = 0;
            loop {
                tot_sum += nums[i_m + cur_i_p as usize];
                cur_i_p += 1;
            
                if (i_m + cur_i_p as usize) < n {
                    if nums[i_m + cur_i_p as usize ] <= m {
                        break;
                    }
                }

                if (i_m + cur_i_p as usize) == n {
                    break;
                }
            }
            cur_i_p = -1;

            if (i_m  as isize + cur_i_p) >= 0 {
                while nums[(i_m  as isize + cur_i_p) as usize] > m && ((i_m as isize) + cur_i_p) >= 0 {
                    tot_sum += nums[(i_m as isize + cur_i_p) as usize];
                    cur_i_p -= 1;

                    if (i_m  as isize + cur_i_p) < 0 {
                        break;
                    }
                }
            }   

            best_sum = best_sum.max(tot_sum);
        }
        result.push_str(&(best_sum.to_string() + "\n"));
    }

    println!("{}", result);

}