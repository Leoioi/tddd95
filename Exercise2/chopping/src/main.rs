use core::num;
use std::io::{self, Read};
use std::{collections::BinaryHeap, fs, string};
use std::cmp::Reverse;

fn main() {
    let file_path = "chopping.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    let mut buffer = Vec::new();
    io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    let content = String::from_utf8_lossy(&buffer);

    let mut nums = content.lines().map(|n| n.parse::<usize>().unwrap());

    let n = nums.next().unwrap();
    let vs: Vec::<usize> = nums.collect();

    if *vs.last().unwrap() != n + 1 {
        print!("Error");
        return;
    }

    let max_nodes: usize = n + 1;

    let mut current_degrees = vec![0; max_nodes + 1]; // Init the current degrees vector with some zeros 

    for v in &vs {
        current_degrees[*v] += 1;
    }
    
    for x in 1..(max_nodes + 1) {
        current_degrees[x] += 1;
    }

    let mut queue: BinaryHeap<Reverse<usize>> = BinaryHeap::new();

    for x in 1..(max_nodes + 1) {
        if current_degrees[x] == 1 {
            queue.push(Reverse(x));
        }
    }

    //let mut results: String = String::new();
    let mut results: Vec<usize> = vec![0];
    let mut isValid = true;

    for v in vs {

        let Reverse(u) = match queue.pop() {
            Some(u) => u,
            None => {
                isValid = false;
                break;
            }
        };

        if current_degrees[u] != 1 || current_degrees[v] < 1  {
            isValid = false;
            break;
        }

        current_degrees[u] = 0;
        current_degrees[v] -= 1;
        if current_degrees[v] == 1 {
            queue.push(Reverse(v));
        }
        results.push(u);
    }
    results = results.split_off(1); // Remove the first element (it will always be 0)
    if isValid && results.len()== n {
        let mut resultsString: String = String::from("");
        for u in results {
            resultsString.push_str(&(u.to_string() + "\n"));
        }
        print!("{}", resultsString);
    }
    else {
        print!("Error");
    }
}
