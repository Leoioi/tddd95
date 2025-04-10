use std::fs;


fn main() {
    let file_path = "messages.in";
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    //let mut buffer = Vec::new();
    //io::stdin()
    //    .read_to_end(&mut buffer)
    //    .expect("Failed to read from stdin");
    //let content = String::from_utf8_lossy(&buffer);

    let mut lines = content.lines();
    
    let mut dic: Vec<Vec<char>> = vec![];

    while let Some(word) = lines.next() {
        if word == "#" {
            break;
        }
        let word_chars: Vec<char> = word.chars().collect();
        dic.push(word_chars);
    }

    let mut messages: Vec<Vec<char>>= vec![];

    while let Some(word) = lines.next() {
        if word == "#" {
            break;
        }
        let word_chars: Vec<char> = word.chars().collect();
        messages.push(word_chars);
    }

    println!("{:?}", dic);
    println!("{:?}", messages);
}
