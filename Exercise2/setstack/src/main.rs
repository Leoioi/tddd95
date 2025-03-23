use std::{collections::{BTreeSet, HashMap}, fs, io::{self, Read}};

fn main() {
    let file_path = "setstack.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin().read_to_end(&mut buffer).expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let lines: std::iter::Skip<std::str::Lines<'_>>= content.lines().skip(2); // We dont care about these 

    let mut results = String::new();
    let mut stack: SetStack = SetStack::new();

    for line in lines {
        match line {
            "PUSH" =>add_to_result(&mut results,stack.perform_push()),
            "DUP" => add_to_result(&mut results,stack.perform_dup()),
            "ADD" => add_to_result(&mut results,stack.perform_add()),
            "UNION" => add_to_result(&mut results,stack.perform_union()),
            "INTERSECT" => add_to_result(&mut results, stack.perform_intersection()),
            _ => {
                stack = SetStack::new(); // Reset the stack 
                results.push_str("***\n");
            }
        }
    }
    results.push_str("***\n"); // Add trailing ***
    print!("{}", results)
}

fn add_to_result (res: &mut String, n: usize) {
    res.push_str(&(n.to_string() + "\n"));
}


struct SetCache {
    cache: HashMap<BTreeSet<usize>, usize>, // Map of all the sets that we have ids for 
    reverse_cache: Vec<BTreeSet<usize>>, // index will correspond to some element 
}

impl SetCache {
    fn new() -> Self {
        let empty_set = BTreeSet::new();
        let mut cache = HashMap::new();
        cache.insert(empty_set.clone(), 0); // The empty set has the id of 0
        SetCache {cache, reverse_cache: vec![empty_set]}
    }
    fn get_id (&mut self, set: BTreeSet<usize>) -> usize {
        if let Some(&id) = self.cache.get(&set) {
            id
        }
        else {
            let id: usize = self.reverse_cache.len(); // Generate a new id 
            self.reverse_cache.push(set.clone());
            self.cache.insert(set, id);
            id
        }
    }
}

struct SetStack {
    stack: Vec<usize>,
    cache: SetCache,
}
impl SetStack {
    // Constructor for an empty stack
    fn new() -> Self {
        Self { stack: Vec::new(), cache: SetCache::new() }
    }

    fn perform_add (&mut self) -> usize{
        let first_set_id = self.stack.pop().unwrap(); 
        let second_set_id = self.stack.pop().unwrap(); 
    
        let second_set = &self.cache.reverse_cache[second_set_id]; // Add the first set TO the second set 
        let mut new_set = second_set.clone();

        new_set.insert(first_set_id);
        let cardinality = new_set.len();
        let id = self.cache.get_id(new_set);
        self.stack.push(id);
        cardinality
    }

    fn perform_intersection (&mut self) -> usize {
        let first_set_id = self.stack.pop().unwrap(); 
        let second_set_id = self.stack.pop().unwrap(); 

        let first_set = &self.cache.reverse_cache[first_set_id];
        let second_set = &self.cache.reverse_cache[second_set_id];

        let resulting_intersection: BTreeSet<usize> = first_set.intersection(&second_set).cloned().collect();
        let cardinality: usize = resulting_intersection.len();
        let id = self.cache.get_id(resulting_intersection);
        self.stack.push(id);
        cardinality
    }

    fn perform_union (&mut self) -> usize {
        let first_set_id = self.stack.pop().unwrap(); 
        let second_set_id = self.stack.pop().unwrap(); 

        let first_set = &self.cache.reverse_cache[first_set_id]; // Get the two sets of ids
        let second_set = &self.cache.reverse_cache[second_set_id];

        let resulting_union: BTreeSet<usize> = first_set.union(second_set).cloned().collect(); // perform union on the two sets 
        let cardinality: usize = resulting_union.len();
        let id = self.cache.get_id(resulting_union);
        self.stack.push(id);

        cardinality
    }

    fn perform_dup (&mut self) -> usize {
        let first_set_id = self.stack.pop().unwrap(); 
        let top_set = &self.cache.reverse_cache[first_set_id];

        let cardinality: usize = top_set.len();

        self.stack.push(first_set_id);
        self.stack.push(first_set_id);
        cardinality
    }

    fn perform_push (&mut self) -> usize{
        self.stack.push(0); // for the way that we init the cache the empty set will always have id of 0
        0 // will always return 0 
    }
}
