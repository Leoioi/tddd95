use core::num;
use std::{fs, io::{self, Read}};

fn main() {
    let file_path = "turbo.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut og_list = content
        .lines()
        .map(|s| s.parse::<usize>().unwrap() - 1); // I use zero index 

    let size: usize = og_list.next().unwrap() + 1;

    let mut index_pairs: Vec<(usize, usize)> = og_list.clone()    
                                                      .enumerate()
                                                      .collect();

    index_pairs.sort_by(|(_, b1), (_, b2)| b1.cmp(b2)); // the first value in these pairs will be the original indices 

    let mut fenwick = Fenwick::new(size as usize);

    for i in 0..fenwick.size {
        fenwick.update(i, 1);
    }

    let mut results = String::new();

    //let (i , j) = (0 as usize , (size - 1) as usize);
    let mut j: usize = size - 1;
    let mut i: usize = 0;

    for iteration_v in 0..size  {
        let mut amount_of_swaps = 0;
        let mut og_index = 0;

        if iteration_v % 2 == 0 {
            (og_index, _) = index_pairs[i]; // The og_index will be the orginal position of some element the the val the the postion that it should be 
            amount_of_swaps = fenwick.query_between(0, og_index);
            i += 1;
        }
        else {
            (og_index, _) = index_pairs[j]; // The og_index will be the orginal position of some element the the val the the postion that it should be 
            amount_of_swaps = fenwick.query_between(og_index + 1 ,  size);
            j -= 1;

        }

        fenwick.update(og_index, -1);
        results.push_str( &(amount_of_swaps.to_string() + "\n"));
    }

    print!("{}", results)

}

struct Fenwick {
    size: usize,
    tree: Vec<i64>,
}

impl Fenwick {

    fn new (n: usize) -> Fenwick{
        let mut tree :Vec<i64> = Vec::new();
        tree.resize(n, 0); // Fill the tree with once  
        Fenwick {size: n, tree: tree}
    }

    fn update (&mut self, mut index: usize, diff: i32) {
        while index < self.size {
            self.tree[index] += diff as i64;
            index = index | (index + 1); // Apply bit magic
        }
    }

    fn query (&self, mut index: usize) -> i64 {
        let mut result: i64 = 0;
        let mut iindex = (index) as isize;
        iindex =  iindex - 1;
        while iindex >= 0 {
            result += self.tree[iindex as usize] as i64;
            iindex = (iindex & (iindex + 1)) - 1;
        }
        result // Return the result
    }

    fn query_between (&self, mut index1: usize,  mut index2: usize) -> i64 {
        (self.query(index1) - self.query(index2)).abs()
    }
}

