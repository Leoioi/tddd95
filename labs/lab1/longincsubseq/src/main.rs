use std::cmp::Ordering;
use std::io::Read;
use std::iter::StepBy;
use std::{fs, io, result, usize};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "longincsubseq.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let lines: Vec<&str> = content.lines().collect();
    let mut results: String = String::new();
    
    let sequence_to_string= |indices: Vec<usize>| {
        let mut result = String::from("");

        // Append the length of the subsequence and the indices to the results string.
        result.push_str(&format!("{}\n", indices.len()));
        for idx in indices {
            result.push_str(&format!("{} ", idx));
        }
        // Remove the trailing space and add a newline.
        if result.ends_with(' ') {
            result.pop();
        }

        result
    };


    for i in (1..lines.len()).step_by(2) {
        let number_str: Vec<&str> = lines[i].split_whitespace().collect();
    
        // Parse the numbers into a sequence.
        let seq: Vec<i64> = number_str
            .iter()
            .map(|s| s.parse::<i64>().expect("Invalid number"))
            .collect();
    
        results.push_str(&sequence_to_string( lis(seq)));
        results.push('\n');
    }

    // Print the final result.
    print!("{}", results);
}

/*
For some sequence of numbers we are trying to find the longest strictly increasing subsequence.
The way that we are going to do this is by storing away the best sequence that we have found thus far 
for each possible length. However, instead of storing away every element of every sequence we are only going 
to be storing a way the last element of each of the sequence. This together with a pointer vector (prev in this code)
will be used to reconstruct the longest sequence. By using this type of representation we use less data and have to do less copy 
instructions thereby improving the speed.

The way that the algorithm works centers around ending_elems. Each element in the vector will be the 
last element of a subsequence that we could construct that has the length of its index in ending_elems. 
So if there is a 10 in index 5 it means that there is some sequence that we could construct that has length 5
and will end on 10.

We are going for each element in the seq perform binary search to find where in the ending_elems this elem could 
be placed. This essentially means that we have found some new sequence of some length such that the last element is smaller.
If the element that we are trying to add is bigger then any element in ending_elems it means that we have found some new sequence
that is longer then any other we have found so far.

To get back the longest sequence we pick the last element in the ending_elems and use the prev to reconstruct the sequence

-- Complexity 
Consider that we are going to iterate over each of the elements in seq O(n)
on top of this we are going to be performing a binary search of ending_elems for EACH element in seq, 
in the worse case this vec will have the same amount of elements as in seq so O(log n).
Putting these two together we get a complexity of O(n log n)

 */
fn lis(seq: Vec<i64>) -> Vec<usize> {

    // This vector will contain the index to the last elem of all the sub sequences that we are considering
    let mut ending_elems: Vec<usize> = Vec::new();

    // This prev vector will essentially function like a vector of pointers.
    // For some index in this vector it will contain the next index for the subsequence 
    let mut prev: Vec<Option<usize>> = vec![None; seq.len()];

    // Iterate over each element in the sequence.
    for j in 0..seq.len() {

        // The partition_point method will perform a binary search given the predicate function (O(log n))
        // In this case we are looking for the position (index) in the ending_elems vector where we should add seq[j] 
        let pos: usize = ending_elems.partition_point(|&a| seq[a] < seq[j]);

        // If it is the case that the position where this element seq[j] should be added is after the last element in the ending_elem
        // then this means we have found some new sub sequence that is longer then any we have considered thus far
        if pos == ending_elems.len() {
            // If we're adding a new element at the end,
            // update the 'prev' pointer if 'ending_elems' is not empty.
            if !ending_elems.is_empty() {
                prev[j] = Some(*ending_elems.last().unwrap());
            }
            ending_elems.push(j);
        } else {
            // If there's an element we can replace,
            // update 'prev' if pos > 0 (i.e. there is a previous element in the subsequence).
            if pos > 0 {
                prev[j] = Some(ending_elems[pos - 1]);
            }
            ending_elems[pos] = j;
        }
    }

    // Backtrack through the 'prev' array to reconstruct the indices of the longest increasing subsequence.
    let mut indices = Vec::new();
    if let Some(&last) = ending_elems.last() {
        // Start with the last element in the longest sequence 
        let mut current_elem = Some(last);
        while let Some(idx) = current_elem {
            indices.push(idx);
            current_elem = prev[idx];
        }
    }
    // The indices were collected in reverse order, so reverse them.
    indices.reverse();

    indices

}
