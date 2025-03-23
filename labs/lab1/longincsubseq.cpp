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


int main() {
    // Specify the file path here
    string filePath = "longincsubseq.in";

    vector<string> lines = readFile(filePath); // readInput();

    string results = "";

    for (size_t i = 0; i < lines.size(); i++){
        i++;

        const string line =  lines[i];
        vector<string> numberStr = splitWords(line);
        vector<int> seq;

        for (const auto& number : numberStr){
            seq.push_back(stol(number));
        }

        vector<long> endingElems;
        
        vector<long> prev(seq.size(), - 1);


        for (size_t j = 0; j < seq.size(); j++ ) {

            auto iterator = lower_bound(endingElems.begin(), endingElems.end(), j, [&seq](long a, long b) {
                    return seq[a] < seq[b]; 
                });

            if (iterator == endingElems.end()){
                if (!endingElems.empty()) {
                    prev[j] = endingElems.back();
                }
                endingElems.push_back(j);
            } else {
                if (iterator != endingElems.begin()) {
                    prev[j] = *(iterator - 1);
                }
                *iterator = j;
            }
        }

        vector<int> indices;
        long currentElem = endingElems.back();
        while (currentElem != -1){
            indices.push_back(currentElem);
            currentElem = prev[currentElem];
        }
        reverse(indices.begin(), indices.end());


        results.append(to_string(indices.size()) + "\n");

        for (const auto& index : indices) results.append(to_string(index) + " ");

        results.pop_back();
        results.append("\n");

    }
    cout << results;
    return 0;

}