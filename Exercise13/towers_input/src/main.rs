use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();

    // Number of integers in this test case: 1 ≤ M ≤ 100
    let m = rng.gen_range(1..=100);
    println!("{}", m);

    for _ in 0..m {
        // For each integer representation, choose between 1 and 100 "numbers" in the chain
        let depth = rng.gen_range(1..=100);

        // Generate `depth` numbers each in [1,100]
        let parts: Vec<String> = (0..depth)
            .map(|_| rng.gen_range(1..=100).to_string())
            .collect();

        // Join with caret '^' (no spaces)
        println!("{}", parts.join("^"));
    }
}

