use std::{
    char, clone, collections::{HashSet, VecDeque}, fs, io::{self, Read}, result, usize, vec
};

/*
Author: Leo Jarhede
LiuID: leoja464
*/

const TOTAL: usize = 256; // The number of possible letters

fn main() {
    let file_path = "two.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");


    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();

    let q = lines.next().unwrap().parse::<usize>().unwrap();

    for _ in 0..q {
        let word = lines.next().unwrap();
        let suf_array = build_suffix_array(word);

        //println!("{:?}", suf_array);
        let mut found_words: HashSet<Vec<char>> = HashSet::new();
        //println!("{:?}", suf_array);

        let lcp = build_lcp(word, suf_array.clone());
        let word = word.chars().collect::<Vec<char>>();

        //println!("{:?}", lcp);

        for (offset, len) in lcp.into_iter().enumerate() {
            if len == 0 {
                continue;
            }
            let mut found_word = word.iter().cloned().skip(suf_array[offset]).take(len).collect::<Vec<char>>();
            found_words.insert(found_word.clone());
            let og_len = found_word.len();
            for _ in 0..og_len {
                //println!("{:?}", found_word);
                found_word.remove(found_word.len() - 1);
                found_words.insert(found_word.clone());
            }
                
        }

        println!("{:?}", found_words.len() - 1);
        println!("{:?}", found_words);
    }
}


fn build_lcp(s: &str, sa: Vec<usize>) -> Vec<usize> {
    let s: Vec<char> = s.chars().collect();
    let n = s.len();
    let mut rank: Vec<usize> = vec![0; n];

    for i in 0..n {
        rank[sa[i]] = i;
    }

    let mut lcp: Vec<usize> = vec![0; n];
    let mut h = 0;

    for i in 0..n {
        if rank[i] == 0 {
            lcp[0] = 0;
        }
        else {
            let j = sa[rank[i] - 1];
            while i + h < n && j + h < n && s[i+h] == s[j+h] {
                h += 1;
            }
            lcp[rank[i]] = h;
            if h > 0 {
                h -= 1;
            }

        }
    }
    lcp
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
fn sort_characters(s: &Vec<char>) -> Vec<usize> {
    let mut order = vec![0; s.len()];
    let mut count = [0; TOTAL];

    for i in 0..s.len() {
        count[s[i] as usize] += 1;
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
fn compute_char_classes(s: &Vec<char>, order: &Vec<usize>) -> Vec<usize> {
    let mut class: Vec<usize> = vec![0; s.len()];

    for i in 1..s.len() {
        if s[order[i]] != s[order[i - 1]] {
            // If the two letters is not then same then they
            // must also belong to different classes
            class[order[i]] = class[order[i - 1]] + 1;
        } else {
            // They do indeed belong to the same class
            class[order[i]] = class[order[i - 1]];
        }
    }
    class
}


/*
* In this function we dont really care about the values of the chars in s only the classes of the
* chars in s
*
* Lets consider an example 
* let s = [a, a, b, a]
*     l = 1
*     order = [0, 1, 3, 2]
*     class = [0, 0, 1, 0]
*
* After the second loop,
*     count = [3, 4, 4, 4]
*
* the first iteration of the last loop (i = 3)
*     start = 2 + 3 % 4 equiv 1 // this start variable will be the start of the 
*     cl = 0
*     count = [2, 4, 4, 4]
*     new_order = [0, 0, 1, 0]
*
* the second iteration (i = 2)
*     start = 3 + 3 % 4 equiv 2
*     cl = 1 
*     count = [2, 3, 4, 4]
*     new_order = [0, 0, 1, 2]
*
* 
*/
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
    let mut classes = 1;

    for i in 1..n {
        let cur = new_order[i];
        let prev = new_order[i - 1];

        let mid = (cur + l) % n;
        let mid_prev = (prev + l) % n;

        if class[cur] != class[prev] || class[mid] != class[mid_prev] {
            classes += 1;
        }
        new_class[new_order[i]] = classes - 1;
        
        //if class[cur] != class[prev] || class[mid] != class[mid_prev] {
        //    new_class[cur] = new_class[prev] + 1;
        //} else {
        //    new_class[cur] = new_class[prev];
        //}
    }

    new_class
}


/*
* First we are going to sort strings of lenght 1 then 2 then 4 and so forth, each time dubbling the
* size of the substrings sorted. We are going to start out sorting the letters of the string and
* from this create both the order vector, denoting the order in which we should grab chars from the
* original string such that the restult is ordered. Then also from that ordering create the
* equivalent classes. The equivalent classes are nessessary as two substrings could be equal and if
* so the order between them will not matter, and we will which to retain this information.
*
*  
* 
*/
fn build_suffix_array(s: &str) -> Vec<usize> {
    let s: Vec<char> = s.chars().chain(std::iter::once('\0')).collect();
    let s_len = s.len();
    let mut order = sort_characters(&s);
    let mut class = compute_char_classes(&s, &order);

    let mut l = 1;
    while l < s_len {
        order = sort_double(&s, l, order, &class);
        class = update_class(&order, class, l);

        l *= 2;
    }

    order.remove(0);
    order
}

