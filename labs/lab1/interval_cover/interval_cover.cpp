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

// Defining some structs to store data
struct Interval {
    double a;
    double b;
};

// Each of the intervals should be link with its index such that we dont lose track of it 
struct Pair {
    Interval ival;
    long index;
};

struct TestCase{
    struct Interval val;
    vector<Pair> intervalsWi;
};

struct Result{
    vector<int> indices;
    long nival;
};


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
    vector<TestCase> testCases;
    long index = 0;
    while (index < input.size()){
        // We assume that the first two lines are the interval to cover and the number of intervals 
        string initInterval = input.at(index);
        index++;
        string numberOfIntervals = input.at(index);
        index++;

        vector<string> fittingIntervals;
        for (long i = 0; i < stol(numberOfIntervals); i++){
            fittingIntervals.push_back(input.at(index));
            index++;
        }

        struct TestCase testCase;
        struct Interval ival;
        ival.a = stod(splitWords(initInterval).at(0));
        ival.b = stod(splitWords(initInterval).at(1));
        testCase.val = ival;

        for (long i = 0; i < fittingIntervals.size(); i++){
            struct Interval fival;
            fival.a = stod(splitWords(fittingIntervals.at(i)).at(0));
            fival.b = stod(splitWords(fittingIntervals.at(i)).at(1));
            Pair ivalWi;
            ivalWi.index = i;
            ivalWi.ival = fival;
            testCase.intervalsWi.push_back(ivalWi);
        }

        testCases.push_back(testCase);
    }
    return testCases;
}


bool comparIntervals(Pair a, Pair b) {
    return a.ival.a < b.ival.a;
}

// We sort the intervals in order of start point such that the interval with the earliest start point is first 
vector<TestCase> sortIntervals (vector<TestCase> testCases){
    for (long i = 0; i < testCases.size(); i++){
        sort(testCases.at(i).intervalsWi.begin(), testCases.at(i).intervalsWi.end(), comparIntervals);
    }
    return testCases;
}

// Format the results structs to something that we can print to the terminal
string formatResults (vector<Result> results){
    string formattedResults = "";
    for (const auto& result : results){
        if (result.nival == -1){
            formattedResults.append("impossible\n");
        }
        else{
            string indicesList = "";
            for (const auto& index : result.indices){
                indicesList.append(to_string(index) + " ");
            }

            formattedResults.append(to_string(result.nival) + "\n" + indicesList + "\n");
        }
    }
    return formattedResults;
}

// This perfroms the actual interval cover algorithm 
Result intervalCover (TestCase testCase){

    Result result;
    result.nival = 0;

    long currentIndex = 0;

    // Init the low and max from the testCase.val that being the interval that we want to cover 
    double low = testCase.val.a;
    double max = testCase.val.b;

    // These will be the intervals that we will use to cover low-max
    vector<Pair> sortedIntervals = testCase.intervalsWi;

    while ( (low < max) || (result.indices.empty()) ) {
        double newLow = low;

        // init bestIndex to -1 as a placeholder
        long bestIndex = -1;

        // Keep iterating forward until we find some interval that dose NOT cover the low, meaning that we have check all the intervals that could 
        // cover low. For each one of these interval we will be trying to find the interval that has the furthest end point (i.e ival.b).
        // Note that we set i to currentIndex and iterate currentIndex along with i. These means that we will not be searching over interval already considered
        for (long i = currentIndex; i < (int)sortedIntervals.size(); i++) {

            // We have overshot the sought interval that covers low 
            if (sortedIntervals[i].ival.a > low) {
                break;
            }

            // We found a better interval 
            if (sortedIntervals[i].ival.b >= newLow) {
                newLow = sortedIntervals[i].ival.b;
                bestIndex = sortedIntervals[i].index;
            }

            currentIndex++;
        }


        // If the index is still set to -1 it means that we are unable to find any interval that cover low and as such there is no chose of interval that will work.
        if (bestIndex == -1) {
            result.indices.clear();
            result.nival = -1;
            return result;
        }

        low = newLow;

        result.indices.push_back(bestIndex);
    }

    result.nival = (int)result.indices.size();
    return result;
}


int main() {
    // Specify the file path here
    string filePath = "interval.in";

    vector<string> lines = readFile(filePath); // readInput()

    vector<TestCase> testCases = createTestCases(lines);

    testCases = sortIntervals(testCases);
    
    // Apply the intervalCover function to all the test cases
    vector<Result> allResults;
    for (const auto& testCase : testCases){ 
        allResults.push_back(intervalCover(testCase));
    }

    cout << formatResults(allResults);

    return 0;

}