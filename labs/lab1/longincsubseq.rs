use std::cmp::Ordering;
use std::fs;

fn main() {
    let file_path = "longincsubseq.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");
    let lines: Vec<&str> = content.lines().collect();
    let mut results = String::new();

    let mut i = 0;
    while i < lines.len() {
        i += 1; // Skip the first line (original code's i++ at loop start)
        if i >= lines.len() {
            break;
        }

        let line = lines[i];
        let numbers: Vec<i64> = line
            .split_whitespace()
            .map(|s| s.parse().expect("Invalid number"))
            .collect();

        let mut ending_elems: Vec<usize> = Vec::new();
        let mut prev: Vec<Option<usize>> = vec![None; numbers.len()];

        for j in 0..numbers.len() {
            let pos = ending_elems
                .binary_search_by(|&a| numbers[a].cmp(&numbers[j]))
                .unwrap_or_else(|x| x);

            if pos == ending_elems.len() {
                if let Some(&last) = ending_elems.last() {
                    prev[j] = Some(last);
                }
                ending_elems.push(j);
            } else {
                if pos > 0 {
                    prev[j] = Some(ending_elems[pos - 1]);
                }
                ending_elems[pos] = j;
            }
        }

        let mut indices = Vec::new();
        if let Some(&last) = ending_elems.last() {
            let mut current = Some(last);
            while let Some(idx) = current {
                indices.push(idx);
                current = prev[idx];
            }
        }
        indices.reverse();

        results.push_str(&format!("{}\n", indices.len()));
        for (i, &idx) in indices.iter().enumerate() {
            if i > 0 {
                results.push(' ');
            }
            results.push_str(&idx.to_string());
        }
        results.push('\n');

        i += 1; // Original for-loop's i++
    }

    print!("{}", results);
}