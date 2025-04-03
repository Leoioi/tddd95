use std::{
    fs,
    io::{self, Read},
};

fn main() {
    //let file_path = "evil_straw.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut content = String::new();
    io::stdin().read_to_string(&mut content).unwrap();

    let mut tokens = content.split_whitespace();
    let mut next = || -> &str { tokens.next().unwrap() };

    let n: usize = next().parse().unwrap();

    for _ in 0..n {
        let pal: String = next().to_owned();
        match swaps_to_palindorm(pal) {
            Some(n) => println!("{:?}", n),
            None => println!("Impossible"),
        };

    }
}

fn swaps_to_palindorm(pal: String) -> Option<usize> {
    //println!("{:?}", pal);
    let len = pal.len();
    if len == 0 || len == 1 {
        return Some(0);
    }
    let first_c = pal.chars().nth(0).unwrap();
    let last_c = pal.chars().last().unwrap();

    if first_c == last_c {
        return swaps_to_palindorm(pal[1..len - 1].to_string());
    }

    let swaps_l = find_swaps(remove_at_index(pal.clone(), len - 1), last_c);
    let swaps_r = find_swaps(
        remove_at_index(pal.clone(), 0).chars().rev().collect(),
        first_c,
    );

    //println!("{:?}", swaps_l);
    //println!("{:?}", swaps_r);

    if swaps_r.is_none() && swaps_l.is_none() {
        return None;
    }

    let swaps_r = swaps_r.unwrap_or(usize::MAX);
    let swaps_l = swaps_l.unwrap_or(usize::MAX);

    // Oh boy this is going to have to be very complex to handle all edge cases...
    if swaps_l < swaps_r {
        // println!("{:?}", pal);
        let trim = (remove_at_index(remove_at_index(pal, len - 1), swaps_l));
        // println!("{:?}", trim);
        return match swaps_to_palindorm(trim) {
            Some(n) => Some(swaps_l + n),
            None => None,
        };
    } else {
        return match swaps_to_palindorm(remove_at_index(
            remove_at_index(pal, 0),
            (len - 1) - swaps_r - 1,
        )) {
            Some(n) => Some(swaps_r + n),
            None => None,
        };
    }
}

// You should remove q from the word, else we are always going to find a solution
fn find_swaps(word: String, q: char) -> Option<usize> {
    if word.is_empty() {
        return None;
    }

    if word.chars().nth(0).unwrap() == q {
        return Some(0);
    }

    return match find_swaps(remove_at_index(word, 0), q) {
        Some(n) => Some(1 + n),
        none => none,
    };
}

fn remove_at_index(mut s: String, index: usize) -> String {
    // Convert the string into a vector of (byte index, char) pairs.
    let char_indices: Vec<(usize, char)> = s.char_indices().collect();

    // Check if the provided index is valid.
    if index < char_indices.len() {
        // Get the byte index for the character at the given index.
        let (byte_index, _) = char_indices[index];
        s.remove(byte_index);
    }
    s
}
