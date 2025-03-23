#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>
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


long long unsigned totShortFall(long long T, vector<long long> w, long long unsigned N) {
    long long  unsigned tot = 0;
    for (long long unsigned i = 0; i < N; i++){
        tot += min(w.at(i),T);
    }
    return tot;
}   

int main() {
    // Specify the file path here
    string filePath = "ljutnja.in";

    vector<string> lines =  readFile(filePath); //readInput();
    long long M = stoll(splitWords(lines.at(0)).at(0)); 
    long long N = stoll(splitWords(lines.at(0)).at(1));

    vector<long long> w = vector<long long>(N);

    long long low = 0;
    long long max = 0;

    for (long long unsigned i = 0; i < N; i++){
        long long unsigned newElement = stoll(lines.at(i+1));
        if (newElement > max){
            max = newElement;
        }
        // if (newElement < low){
        //     low = newElement;
        // }
        w.at(i) = (newElement);
    }

    long long unsigned W = accumulate(w.begin(), w.end(), 0ULL); // tot candy want
    long long unsigned S = W - M; // tot expected short fall


    while (low < max){
        long long unsigned mid = (max + low) / 2;
        if (totShortFall(mid, w, N) > S){
            max = mid;
        }  
        else{
            low = mid + 1; 
        }
    }
    long long T = low - 1;

    if (T < 0){
        T = 0;
    }


    vector<long long unsigned> shortfall = vector<long long unsigned>(N);
    
    for (long long unsigned i = 0; i < N; i++){
        shortfall.at(i) = min(w.at(i), T);
    }
    
    long long unsigned sumFirstPass = 0;
    for (size_t i = 0; i < N; i++){
        sumFirstPass = sumFirstPass + shortfall.at(i);    
    } 

    long long leftover = S - sumFirstPass;

    if (leftover > 0 ){
        long long countDistributed = 0;
        for (long long unsigned i = 0; i < N; i++){
            if (w[i] > T) {
                shortfall.at(i) = shortfall.at(i) + 1;
                countDistributed = countDistributed + 1;
                if (countDistributed == leftover) {
                    break;
                }
            }
        }
    }

    long long unsigned result = 0;
    for (long long unsigned i = 0; i < N; i++) {
        result = result + (shortfall[i] * shortfall[i]);
    }

    printf("%llu\n", result);

    return 0;

}