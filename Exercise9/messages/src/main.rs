use std::fs;
use std::io;
use std::io::Read;

fn main() {
    let file_path = "messages.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    let mut dic: Vec<Vec<char>> = vec![];

    while let Some(word) = lines.next() {
        if word == "#" {
            break;
        }
        let word_chars: Vec<char> = word.chars().collect();
        dic.push(word_chars);
    }

    let mut messages: Vec<Vec<char>>= vec![vec![]];

    while let Some(mut word) = lines.next() {
        if word == "#" {
            break;
        }
        loop {
            let mut word_chars: Vec<char> = word.chars().collect();
            if *word_chars.last().unwrap() == '|' {
                word_chars.pop();
                messages.last_mut().unwrap().append(&mut word_chars);
                messages.push(vec![]);
                break;
            }
            else {
                messages.last_mut().unwrap().append( &mut word_chars);
            }

            word = lines.next().unwrap();
        }
    }
    messages.pop(); // Remove the last empty vec
    println!("{:?}", messages);
    
    for message in messages.into_iter() {
        let mut intervals: Vec<(usize, usize)> = vec![];
        for sought_word in &dic {
            
            let word_locations =  search(sought_word.clone(), message.clone());
            for start in word_locations {
                intervals.push((start, start + sought_word.len() - 1) );
            }
        }
        let max_number_of_words = max_disjointsets(intervals);
        println!("{:?}", max_number_of_words);
    }

}


fn max_disjointsets (mut intervals: Vec<(usize, usize)> ) -> usize {
    intervals.sort_by(|&a, &b | a.1.cmp(&b.1) );
    let mut best_interval: Vec<(usize, usize)> = vec![];
    let mut current_bot: i32 = -1;
    for interval in intervals {
        if interval.0 as i32> current_bot {
            best_interval.push(interval);
            current_bot = interval.1 as i32;
        }
    }
    
    best_interval.len()
}

fn search(pattern: Vec<char>, text: Vec<char> ) -> Vec<usize> {
    let search_string_len = pattern.len();

    let combine = pattern
        .into_iter()
        .chain(std::iter::once('\0'))
        .chain(text.into_iter())
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
