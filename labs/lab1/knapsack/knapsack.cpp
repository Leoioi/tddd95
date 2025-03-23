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


// Some struct to structure our data and result
struct TestCase {
    long capacity;
    vector<pair<long , long> > items;
};

struct BagConfiguration {
    long value;
    vector<long> indices;
};

// From the input lines create the test case structs 
vector< TestCase> createTestCases(vector<string> input){
    vector< TestCase> testCases;
    for (size_t i = 0; i < input.size();){
        vector<string> firstLine = splitWords(input[i]);
        long capacity = stol(firstLine[0]);
        long numberOfItems =stol(firstLine[1]);
        i++;
        long stoppingIndex = numberOfItems + i;

        vector<pair<long , long> > items;
        for (; i < stoppingIndex; i++){
            vector<string> line = splitWords(input[i]);
            vector<long> lineContent;

            for (const auto& word : line ){
                lineContent.push_back(stol(word));
            }

            pair<long, long> item;
            item.first = lineContent[0];
            item.second = lineContent[1];
            items.push_back(item);
        }
        TestCase testCase = {capacity, items};

        testCases.push_back(testCase);
    }
    return testCases;
}


int main() {
    // Specify the file path here
    string filePath = "knapsack.in";

    vector<string> lines = readFile(filePath); // readInput();

    // Create the structs that will represent some test case 
    vector<TestCase> testCases = createTestCases(lines);

    // Vector to hold the results of the vector
    vector<BagConfiguration> resultingBagConfigurations;


    for (const auto& testCase : testCases){
        long n = testCase.items.size(); // the number of items 
        long W = testCase.capacity; // The capacity of the bag
        long dpTable[n+1][testCase.capacity+1]; // Define an array to hold the table used for dynamic programming 

        // Init the table with 0's
        for (long i = 0; i <= n; i++){
            for (long j = 0; j <= W; j++){
                dpTable[i][j] = 0;
            }
        }

        // Fill out the dynamic programming table 
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= W; j++) {
                
                if (testCase.items[i-1].second > j) {
                    // We don't have enough capacity to add this item
                    dpTable[i][j] = dpTable[i-1][j];  

                } else {
                    // We can now try to add another item but only if there is not some better configuration (with higher value)
                    dpTable[i][j] = max(dpTable[i-1][j], dpTable[i-1][j - testCase.items[i-1].second] + testCase.items[i-1].first); 
                }
            }
        }
        
        // Backtrack over the table to find the indices of the most optimal items
        BagConfiguration config;
        config.value = dpTable[n][W];
        long j = W;
        for (long i = n; i > 0; i-- ){
            // We added i to the bag and increased the value 
            if (dpTable[i][j] > dpTable[i-1][j]){
                config.indices.push_back(i);
                // Decrement the wight of the bag
                j = j - testCase.items[i-1].second;
            }
        }
        // Add the bag configuration to the result vector
        resultingBagConfigurations.push_back(config);
    }


    // Format the bag configurations to one big old string 
    string result = "";
    for (const auto& config : resultingBagConfigurations){
        string testCaseResult = to_string(config.indices.size()) + "\n";

        for (size_t i = 0; i < config.indices.size(); i++ ){
            long correctedIndex = config.indices.at(i) - 1;
            testCaseResult.append(to_string(correctedIndex) + " ");
        }
        testCaseResult.pop_back(); 
        result.append(testCaseResult + "\n");
    }

    cout << result << endl; // Print the (hopefully) correct result

    return 0;

}