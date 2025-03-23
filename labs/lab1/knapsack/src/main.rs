use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Read};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    let file_path = "knapsack.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let lines = content.lines().collect();

    let test_cases = create_test_cases(lines);

    // Vector to hold the resulting bag configurations.
    let mut resulting_bag_configurations: Vec<BagConfiguration> = Vec::new();


    // For each test case perform dynamic programming
    for test_case in test_cases {
        let config = find_opt_config(test_case);
        resulting_bag_configurations.push(config);
    }

    // Format the resulting bag configurations as a string.
    let mut result = String::new();
    for config in resulting_bag_configurations {
        // First line: number of items in the configuration.
        result.push_str(&format!("{}\n", config.indices.len()));
        // Next line: indices (converted to 0-indexed) separated by a space.
        let indices_str: Vec<String> = config
            .indices
            .iter()
            .map(|&idx| format!("{}", idx - 1))
            .collect();
        result.push_str(&indices_str.join(" "));
        result.push('\n');
    }

    // Output the final result.
    println!("{}", result);
}


/*
This is the function that will find some optimal configuration of items given the testCase parameters (capacity and items)
This is done by a type of dynamic programming by first initializing a two-dimensional table, where each entry dp_table[i][j]
represents the maximum value achievable with the first i items and a capacity of j. It iterates over each item and for each 
possible capacity, decides whether to include the item or not. If the items weight exceeds the current capacity it retains
the previous value otherwise, it takes the maximum of not including the item (dp_table[i-1][j]) and including it (dp_table[i-1][j - weight] + value).
Once the table is filled, the optimal value is found at dp_table[n][W]. 

Once we have found found the optimal value we are able to use a type of back tracking to figure out 
what items correspond to the achieved value. We do this by comparing whether dp_table[i][j] is greater than dp_table[i-1][j].
If it is then that means that we added the corresponding item and we reduce that from the total wight.

-- Complexity 
As the algorithm will simply fill out a matrix the complexity of the algorithm will simply scale along with the size of the 
matrix. The size of the matrix will be n*W, as such the complexity will simply be O(W*n)
where W is the maximum capacity and n the number of items we case choose from.
 */
fn find_opt_config(test_case: TestCase) -> BagConfiguration {
    let n = test_case.items.len();
    let W = test_case.capacity as usize;
    // Create the DP table with dimensions (n+1) x (W+1) initialized to 0.
    let mut dp_table = vec![vec![0; W + 1]; n + 1];

    // Fill out the dynamic programming table.
    for i in 1..=n {
        // Select some item and try to add it to the bag
        let (value, weight) = test_case.items[i - 1];
        for j in 1..=W {
            if (weight as usize) > j {
                // Not enough capacity to include this item. As such the value 
                // dose not change for the last entry 
                dp_table[i][j] = dp_table[i - 1][j];
            } else {
                // If the item is not to heavy, check if including this item might 
                // improve the value or if there is some other configuration without this item 
                // that has a better value
                dp_table[i][j] = dp_table[i - 1][j]
                    .max(dp_table[i - 1][j - weight as usize] + value);
            }
        }
    }

    // Backtrack to find which items were chosen.
    let mut config = BagConfiguration {
        value: dp_table[n][W],
        indices: Vec::new(),
    };
        
    let mut j = W;
    for i in (1..=n).rev() {
        if dp_table[i][j] > dp_table[i - 1][j] {
            config.indices.push(i as i64);
            let weight = test_case.items[i - 1].1 as usize;
            j = j - weight;
        }
    }
    config
}


// Represents a test case containing the bag capacity and a list of items.
// Each item is a tuple: (value, weight)
#[derive(Debug)]
struct TestCase {
    capacity: i64,
    items: Vec<(i64, i64)>,
}

// Represents the configuration of items selected for the knapsack.
// These struct essentially just represent the result of some test case.
#[derive(Debug)]
struct BagConfiguration {
    value: i64,
    indices: Vec<i64>,
}

/// Parses the input lines into a vector of test cases.
fn create_test_cases(input: Vec<&str>) -> Vec<TestCase> {
    let mut test_cases = Vec::new();
    let mut i = 0;
    while i < input.len() {
        // The first line of a test case contains the capacity and number of items.
        let first_line: Vec<String> = input[i].split_whitespace().map(|s| s.to_string()).collect();
        i += 1;
        if first_line.len() < 2 {
            continue;
        }
        let capacity = first_line[0].parse::<i64>().unwrap_or(0);
        let number_of_items = first_line[1].parse::<i64>().unwrap_or(0);
        let mut items = Vec::new();
        let stopping_index = i + number_of_items as usize;

        // Each of the following lines describes an item: first number is value, second is weight.
        for j in i..stopping_index {
            let words: Vec<String> = input[j].split_whitespace().map(|s| s.to_string()).collect();
            let mut numbers = Vec::new();
            for word in words {
                numbers.push(word.parse::<i64>().unwrap_or(0));
            }
            if numbers.len() >= 2 {
                items.push((numbers[0], numbers[1]));
            }
        }
        i = stopping_index;
        test_cases.push(TestCase { capacity, items });
    }
    test_cases
}