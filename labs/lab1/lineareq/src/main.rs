use core::fmt;
use std::{fs, io::{self, Read}, mem::swap, usize};

/* 
Author: Leo Jarhede 
LiuID: leoja464
*/

fn main() {
    // 
    let file_path = "lineareq.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    // let mut buffer = Vec::new();
    // io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    // let content = String::from_utf8_lossy(&buffer);

    let mut lines: std::str::Lines<'_> = content.lines(); 

    let mut result: String = String::from("");

    loop { // loop forever!
        let dim  = lines.next().unwrap().parse::<usize>().unwrap();

        if dim == 0 {
            break;
        }

        // Parse out the A matrix 
        let a: Vec<Vec<f64>> = lines
                                    .by_ref()
                                    .take(dim)
                                    .map(|s| s.
                                                split_whitespace()
                                                .map(|s2: &str| s2.parse().unwrap())
                                                .collect())
                                    .collect();
        // Parse out the b vector
        let b: Vec<f64> = lines
                                .next()
                                .unwrap()
                                .split_whitespace()
                                .map(|s: &str| s.parse().unwrap())
                                .collect();

        // Combine the A and b to the Ab matrix that will be used as input for the gauss function 
        let mut ab: Vec<Vec<f64>> = a.into_iter()
                                    .zip(b)
                                    .map(|(mut a,b)| { 
                                        a.push(b); 
                                        a 
                                    })
                                    .collect();
        
        // Solve the linear system using Gaussian elimination
        // Depending on the outcome, append the result (solution or error message) to the result string
        match gauss(&mut ab) {
            Ok(ans) => result.push_str( &(ans.iter().map(|n| n.to_string()).collect::<Vec<String>>().join(" ") + "\n") ),
            Err(GaussError::MultipleSolution) => result.push_str("multiple\n"),
            Err(GaussError::Inconsistent) => result.push_str("inconsistent\n")
        }
    }

    println!("{}", result);

}


const EPS: f64 = 1e-9;

// Custom error enum for the result of the gauss function
enum GaussError {
    MultipleSolution,
    Inconsistent,
}


/*
This function is an implementation of Gaussian elimination for solving a systems of linear equations of the form, A * x = b
Where A is a matrix of the size n*n containing the coefficients and b a vector of size of 1xn.

We can perform gaussian elimination by firstly selection some col and trying to find the pivot, and finding the element 
with the largest coefficient (to improves numerical stability). Once the pivot has been find we will if necessary swap row 
where the pivot is located to the current selected row, this will bring the pivot element into position. 
Then we preform elimination by subtracting from all other rows subtract the selected row such that the column where 
the pivot is located will only contain the pivot it self and zeros.
This concludes the first step of the algorithm and at this point our a matrix will be in row reduced form (RRF)

At this point we are able to extract a solution by dividing the elements in the last column (b) with our pivot elements.

When a solution has been extracted we can check whether it satisfies right hand side is equal to the left,  if not then the answer is inconsonant
We can also check if there is any column for which we have not selected a pivot if that is the case then we know there are 
multiple solutions.

-- Complexity 
Consider that we need to select n pivot elements and for each of those we will need to at most search through n rows.
As such selecting each of the pivots will be of O(n^2) time complexity.
Then we also need to for each pivot eliminate it from the other rows, a O(n) time complexity operations.
Bringing these two things together we get the resulting complexity of O(n^3)
 */
fn gauss (ab: &mut Vec<Vec<f64>>) -> Result<Vec<f64>, GaussError> {
    let n = ab.len();

    // store the row index corresponding to the pivot for each column
    let mut pivots: Vec<i32> = vec![-1; n];

    let mut row = 0;
    let mut col = 0;
    while col < n && row < n {
        let mut selected_row = row;

        // Find the row with the largest absolute value in the current column
        for i in row..n {
            if ab[i][col].abs() > ab[selected_row][col].abs() {
                selected_row = i;
            }
        }

        // If the pivot is nearly zero, move to the next column
        if ab[selected_row][col].abs() < EPS {
            col += 1;
            continue;
        }

        // For the selected row swap every element in the row with the current 
        // Ensure sel and row are not equal
        if selected_row != row {
            // Determine which index is greater
            let (first_index, second_index) = if selected_row < row { (selected_row, row) } else { (row, selected_row) };

            // Split ab at the higher index. The first slice has indices [0, second_index)
            // and the second slice starts at second_index.
            let (first_part, second_part) = ab.split_at_mut(second_index);

            for i in col..n+1 { // Note that we start at col and swap from there 
                std::mem::swap(&mut first_part[first_index][i], &mut second_part[0][i]);
            }
        }

        pivots[col] = row as i32;

        // preform elimination in the current column from all other columns
        for i in 0..n {
            if i != row {
                let c = ab[i][col] / ab[row][col];
                for j in col..n+1 {
                    ab[i][j] -= ab[row][j] * c;
                }
            }        
        }

        row += 1;
        col += 1;

    }

    let mut ans = vec![0.0; n];

    // Extract the answer    
    for i in 0..n {
        if pivots[i] != -1 {
            ans[i] = ab[pivots[i] as usize][n] / ab[pivots[i] as usize][i];
        }
    }

    // If there is some row such that the left and right hand side are not equal then the answer is inconsistent
    for i in 0..n {
        let mut sum = 0.0;
        for j in 0..n {
            sum += ans[j] * ab[i][j];
        }
        if (sum - ab[i][n]).abs() > EPS {
            return Err( GaussError::Inconsistent );
        }
    }

    // If there is some column where we have not selected a pivot we know that there might be many solutions
    for i in 0..n {
        if pivots[i] == -1 {
            return Err( GaussError::MultipleSolution );
        }
    }
    return Ok(ans);

} 



