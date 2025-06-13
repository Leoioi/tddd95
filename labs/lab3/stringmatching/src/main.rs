use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read},
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

fn main() {
    let mut buffer = Vec::new();
    io::stdin()
        .read_to_end(&mut buffer)
        .expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    while let Some(search_string) = lines.next() {
        let line_maybe = lines.next();
        let line = line_maybe.unwrap();

        let search_position = search(search_string, line);

        println!("");
        search_position.iter().for_each(|n| print!("{} ", n));
    }
}

/*
 * This algoritm will find the position of the pattern in the text paramater.
 * It dose this by utilizing the prefix function which will when run return a vector of number.
 * We start by constructing a new string by prepending the pattern  and a unique character to 
 * the start of the text we are searching. 
 * In this case I have chosen the null character /0 to be the unique character but it could be any
 * character so long as that character dose not appear in the text.
 *
 * This new constructed string ("combine" in the code) is then run through the prefix funciton. 
 * Now we can use the resulting prefix vector to find where in the text our string appears by 
 * finding where in the prefix vector the length of the pattern appears. 
 *
 * Complexity,
 * Let p_n be the size of the pattern and t_n be the size of the text.
 * Consider the fact that the prefix funciton is only ever going to have to go through the input
 * string once. This means that the constructing the prefix vector can be done in O(p_n + t_n)
 * time. There is also some complexity reltated to constructing the "combine" string as well 
 * as searching through the resulting prefix vector but both of these tasks are done in 
 * O(p_n + t_n) time as well so the resulting time complexity is still O(p_n + t_n)
 *
 */
fn search(pattern: &str, text: &str) -> Vec<usize> {
    let search_string_len = pattern.len();

    let combine = pattern
        .chars()
        .chain(std::iter::once('\0'))
        .chain(&mut text.chars())
        .collect::<Vec<char>>();

    let prefix_vector = prefix(combine); // Call prefix funciton

    let search_position: Vec<usize> = prefix_vector
        .iter()
        .enumerate()
        .filter(|(_, num)| **num == search_string_len) // Filter out position in the string where
                                                       // the pattern length appears

        .map(|(i, _)| i - search_string_len * 2) // Calculate where the pattern starts in the text 
        .collect();

    search_position
}

/*
 * In this funciton we implement the Knuth-Morris-Pratt algoritm. It will take as an input a 
 * text string and produce a vector of number. In this vector each position is going to 
 * represent the length of the suffix that matches the prefix of the string. 
 *
 * The algoritm works utilizing the previosly computed prefix length value and iterativly 
 * considers larger and larger substrings. We use old values as we know that 
 * the length of the matching suffix for some substring is only ever going to increase by 
 * 1 if we add another character to the substring.
 * As such we can simply check the length of the last matching suffix and if the next 
 * character of the prefix matches the last new character of the substring then the 
 * new length is simply one longer then the last.
 *
 */
fn prefix(s: Vec<char>) -> Vec<usize> {
    let n: usize = s.len();
    let mut pi: Vec<usize> = vec![0; n]; // prefix vector
    for i in 1..n {
        let mut j = pi[i - 1]; // Length of the last matching suffix
        
        // This loop we continue to decrement j until we find a matching prefix
        while j > 0 && s[i] != s[j] { 
            j = pi[j - 1];
        }

        // At this point we have found the largest matching prefix.
        // If this largest matching prefix is of length 0 then this if statement 
        // can either evaluate to true or false depending on if the last letter of 
        // the substring matches the first or not.
        //
        // If we did find some largest matching prefix with a length larger then 1
        // then this will always evaluate to ture and add an additional length to the
        // prefix vector
        if s[i] == s[j] {
            j += 1;
        }
        pi[i] = j;
    }
    pi
}
