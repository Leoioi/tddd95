use std::{
    char, collections::{HashSet, VecDeque}, fs, io::{self, Read}, iter::FlatMap, usize
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    //let file_path = "one.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    let t = lines.next().unwrap().parse::<usize>().unwrap();
    
    for _ in 0..t {
        let n = lines.next().unwrap().parse::<usize>().unwrap();
        let mut words = (0..n).map(|_| lines.next().unwrap().chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>(); 
        words.sort();

        //println!("{:?}", words);


        let mut is_stop = false;
        for (first, second) in words.iter().zip(words.iter().skip(1)) {
            is_stop = false;
            for (c1, c2) in first.iter().zip(second) {
                if c1 != c2 {
                    is_stop = true;
                    break;
                }
            }
            if !is_stop {
                break;
            }
        }

        if is_stop {
            println!("YES");
        }
        else {
            println!("NO");
        }
    }
}

/*
 * This algoritm will find the position of the pattern in the text paramater. 
 * It dose this by utilizing the prefix function which will when run return a vector of number.
 * Each position i of this vector will be the longest lenght of the prefix that is also a sufix to
 * the substring of the length i.
 *
 * Complexity,
 * As the prefix function is linear?
 *
 */

fn search(pattern: &str, text: &str) -> Vec<usize> {
    let search_string_len = pattern.len();

    let combine = pattern
        .chars()
        .chain(std::iter::once('\0'))
        .chain(&mut text.chars())
        .collect::<Vec<char>>();

    let prefix_vector = prefix(combine);

    let search_position: Vec<usize> = prefix_vector
        .iter()
        .enumerate()
        .filter(|(_, num)| **num == search_string_len)
        .map(|(i, _)| i - search_string_len * 2)
        .collect();

    search_position
}

fn prefix(s: Vec<char>) -> Vec<usize> {
    let n: usize = s.len();
    let mut pi: Vec<usize> = vec![0; n];
    for i in 1..n {
        let mut j = pi[i - 1];
        while j > 0 && s[i] != s[j] {
            j = pi[j - 1];
        }
        if s[i] == s[j] {
            j += 1;
        }
        pi[i] = j;
    }
    pi
}
