use std::{
    collections::{HashSet, VecDeque},
    fs,
    io::{self, Read}, usize,
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

const TOTAL: usize = 256; // The number of possible letters

fn main() {
    //let file_path = "suffixsorting.in";
    //let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    //let mut lines = content.lines();

    println!("{:?}", build_suffix_array("Popup$".chars().collect()));
}
/*
* if we have some alpabet of a-d
* then we might want to get the order of the string ba
* 
* the count: [1, 1, 0, 0]
* after cumsum
* count: [1, 2, 2, 2]
*
* The resuting order will then be:
* order: [1, 0]
*/
fn sort_characters (s: &Vec<char>) -> Vec<usize> {
    let mut order = vec![0; s.len()];
    let mut count = [0; TOTAL];
    
    for i in 0..s.len() {
        count[s[i] as usize ] += 1;
    }


    for i in 1..TOTAL {
        count[i] = count[i] + count[i - 1];
    }

    for i in (0..s.len()).rev() {
        let c = s[i] as usize;
        count[c] -= 1;
        order[count[c]] = i;
    }

    order
}

/*
* Here s is the string that we want to find the classses in 
* order is the order of the letters in the string s,
*
* The goal of this function is to return to us the class of each of the letters in the string s.
* This class will represent the valid sorting partions, i.e any reodering of the letters in one
* class will still result a valid ordering. 
*/
fn compute_char_classes (s: &Vec<char>, order: &Vec<usize>) -> Vec<usize> {
    let mut class: Vec<usize> = vec![0; s.len()];

    for i in 1..s.len() {
        if s[order[i]] != s[order[i - 1]] {  // If the two letters is not then same then they
            // must also belong to different classes 
            class[order[i]] = class[order[i - 1]] + 1;
        }
        else { // They do indeed belong to the same class
            class[order[i]] = class[order[i - 1]];
        }
    }
    class
}

fn sort_double(s: &Vec<char>, l: usize, order: Vec<usize>, class: &Vec<usize>) -> Vec<usize> {
    let mut count: Vec<usize> = vec![0; s.len()];
    let mut new_order: Vec<usize> = vec![0; s.len()];

    for i in 0..s.len() {
        count[class[i]] += 1;
    }

    for i in 1..s.len() {
        count[i] = count[i] + count[i - 1];
    }

    for i in (0..s.len()).rev() {
        let start = (order[i] + (s.len() - l)) % s.len();
        let cl = class[start];
        count[cl] -= 1;
        new_order[count[cl]] = start;
    }

    new_order
}



fn update_class(new_order: &Vec<usize>, class: Vec<usize>, l: usize) -> Vec<usize> {
    let n = new_order.len();
    let mut new_class = vec![0; n];
    
    for i in 1..n {
        let cur = new_order[i];
        let prev = new_order[i - 1];

        let mid = (cur + l) % n;
        let mid_prev= (prev + l) % n;

        if class[cur] != class[prev] || class [mid] != class[mid_prev] {
            new_class[cur] = new_class[prev] + 1;
        }
        else {
            new_class[cur] = new_class[prev];
        }
    }

    new_class
}

fn build_suffix_array(s: Vec<char>) -> Vec<usize> {
    let s_len  = s.len();
    let mut order = sort_characters(&s);
    let mut class = compute_char_classes(&s, &order);
    println!("{:?}", class);

    let mut l = 1;
    while l < s_len { 
        order = sort_double(&s, l, order, &class);
        class = update_class(&order , class, l);

        l *= 2;
    }

    order
}

