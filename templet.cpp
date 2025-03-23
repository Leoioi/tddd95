#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

using namespace std;


// Function to read a file and return its lines as a vector of strings
vector<string> readFile(const string& filePath) {
    vector<string> lines;
    ifstream file(filePath);

    if (!file.is_open()) {
        cerr << "Error reading the file: " << filePath << endl;
        return lines;
    }

    string line;
    while (getline(file, line)) {
        lines.push_back(line);
    }

    file.close();
    return lines;
}

// Function to read lines from standard input (using redirection)
vector<string> readInput() {
    vector<string> lines;
    string line;
    while (getline(cin, line)) {
        lines.push_back(line);
    }
    return lines;
}

vector<string> splitWords(string text){
    istringstream iss(text); // Create a string stream
    vector<string> words; // Vector to store the words
    string word;

    // Extract words one by one
    while (iss >> word) {
        words.push_back(word);
    }
    return words;
}

// From the input lines create the test case structs 
vector<struct TestCase> createTestCases(vector<string> input){
}



int main() {
    // Specify the file path here
    string filePath = "interval.in";

    vector<string> lines = readFile(filePath); // readInput()

    vector<TestCase> testCases = createTestCases(lines);


    return 0;

}