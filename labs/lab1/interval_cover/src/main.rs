use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Read};
use std::process;

/*
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "interval.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let lines  = content.lines().collect(); 

    let mut test_cases = create_test_cases(lines);

    // Apply the interval covering algorithm to all test cases.
    let all_results: Vec<ResultStruct> = test_cases
        .iter_mut()
        .map(|test_case| interval_cover(test_case))
        .collect();

    println!("{}", format_results(&all_results));
}

/* 
Performs the interval covering algorithm for a single test case.
For this one test case we will be using a greedy approach and will be selecting the interval such that 
we cover as much of the selected interval as possible. 
As the intervals are sorted with regards to the start of each interval we will keep selecting intervals 
up to the point where the start of the next interval bigger then the start of the interval that we want to
cover. At that point we know that we have considered all the intervals of interest and we simply select the one that 
has the biggest end point, i.e covers as much of our selected interval as possible.

-- Complexity 
Consider that for each test case we are going to be sorting the intervals in test_case.intervals_wi is O(n log n)
After that in this function we are going to be iterating over each of the testcases therefore O(n)
The combination of these two operations will result in a complexity of O(n log n).
Where n is the number of intervals that we can choose from, i.e the size of test_case.intervals_wi

*/


fn interval_cover(test_case: &mut TestCase) -> ResultStruct {
    test_case.intervals_wi.sort_by(|a, b| a.ival.a.partial_cmp(&b.ival.a).unwrap());


    let mut result = ResultStruct {
        indices: Vec::new(),
        nival: 0,
    };

    let mut current_index: usize = 0;
    let mut low = test_case.val.a;
    let max = test_case.val.b;
    let sorted_intervals = &test_case.intervals_wi;

    // Continue while the current covered point is less than max,
    // or if no interval has been selected yet.
    while (low < max) || result.indices.is_empty() {
        let mut new_low = low;
        let mut best_index: i64 = -1;

        // Process all intervals that start at or before the current low.
        while current_index < sorted_intervals.len()
            && sorted_intervals[current_index].ival.a <= low
        {
            if sorted_intervals[current_index].ival.b >= new_low {
                new_low = sorted_intervals[current_index].ival.b;
                best_index = sorted_intervals[current_index].index;
            }
            // Iterate the index thereby disregarding shorter intervals from being considered again 
            current_index += 1;
        }

        // If no interval can extend the coverage, the problem is impossible.
        if best_index == -1 {
            return ResultStruct {
                indices: Vec::new(),
                nival: -1,
            };
        }

        low = new_low;
        result.indices.push(best_index);

        // If we've covered the interval, we can exit.
        if low >= max {
            break;
        }
    }

    result.nival = result.indices.len() as i64;
    result
}

#[derive(Debug, Clone)]
struct Interval {
    a: f64,
    b: f64,
}

#[derive(Debug, Clone)]
struct Pair {
    ival: Interval,
    index: i64,
}

#[derive(Debug, Clone)]
struct TestCase {
    val: Interval,
    intervals_wi: Vec<Pair>,
}

#[derive(Debug)]
struct ResultStruct {
    indices: Vec<i64>,
    nival: i64,
}

// Given the input lines, create test cases.
fn create_test_cases(input: Vec<&str>) -> Vec<TestCase> {
    let mut test_cases = Vec::new();
    let mut index: usize = 0;
    while index < input.len() {
        // Assume first line is the interval to cover and the next is the number of intervals.
        let init_interval = &input[index];
        index += 1;
        if index >= input.len() {
            break;
        }
        let number_of_intervals = &input[index];
        index += 1;

        // Collect the following lines for intervals.
        let num_intervals: usize = match number_of_intervals.trim().parse() {
            Ok(n) => n,
            Err(_) => {
                eprintln!("Error parsing number of intervals: {}", number_of_intervals);
                0
            }
        };
        let mut fitting_intervals = Vec::new();
        for _ in 0..num_intervals {
            if index < input.len() {
                fitting_intervals.push(input[index]);
                index += 1;
            }
        }

        // Parse the initial interval.
        let init_words:  Vec<&str> = init_interval.split_whitespace().collect();
        if init_words.len() < 2 {
            continue;
        }
        let a: f64 = init_words[0].parse().unwrap_or(0.0);
        let b: f64 = init_words[1].parse().unwrap_or(0.0);
        let val = Interval { a, b };

        // Parse each fitting interval.
        let mut intervals_wi = Vec::new();
        for (i, line) in fitting_intervals.iter().enumerate() {
            let words: Vec<&str> = line.split_whitespace().collect();
            if words.len() < 2 {
                continue;
            }
            let a: f64 = words[0].parse().unwrap_or(0.0);
            let b: f64 = words[1].parse().unwrap_or(0.0);
            let fival = Interval { a, b };
            let pair = Pair {
                ival: fival,
                index: i as i64,
            };
            intervals_wi.push(pair);
        }

        test_cases.push(TestCase { val, intervals_wi });
    }
    test_cases
}


// Formats the results into a string that can be printed.
fn format_results(results: &[ResultStruct]) -> String {
    let mut formatted_results = String::new();
    for res in results {
        if res.nival == -1 {
            formatted_results.push_str("impossible\n");
        } else {
            formatted_results.push_str(&format!("{}\n", res.nival));
            let indices_list: String = res
                .indices
                .iter()
                .map(|index| format!("{} ", index))
                .collect();
            formatted_results.push_str(&format!("{}\n", indices_list.trim_end()));
        }
    }
    formatted_results
}
