#include <bits/stdc++.h>
#include <ostream>
using namespace std;

using ull = unsigned long long;
using TowerForm = pair<vector<ull>, long double>;

bool leo_cmp(TowerForm a, TowerForm b) {
    auto v1 = a.first;
    auto v2 = b.first;
    long double e1 = a.second;
    long double e2 = b.second;


    while (v1.size() < v2.size()) {
        ull top = v2.back(); v2.pop_back();
        e2 = powl((long double)top, e2);
    }
    while (v1.size() > v2.size()) {
        ull top = v1.back(); v1.pop_back();
        e1 = powl((long double)top, e1);
    }

    if (e1 != e2) {
        return e1 > e2;
    }

    for (int i = (int)v1.size() - 1; i >= 0; --i) {
        if (v1[i] != v2[i]) {
            return v1[i] > v2[i];
        }
    }

    return false;
}

TowerForm leo_tower_form(const vector<ull>& powers) {
    const long double THRESH = log2l(numeric_limits<long double>::max());
    long double c = 1.0L;
    int n = powers.size();

    for (int i = n - 1; i >= 0; --i) {
        long double b = (long double)powers[i];
        long double log2b = log2l(b);

        if (c * log2b > THRESH) {
            if (i - 1 >= 0) {
                long double a = (long double)powers[i - 1];
                long double rest_exp = log2l(log2l(a)) + log2b * c;
                vector<ull> rest(powers.begin(), powers.begin() + (i - 1));
                return { rest, rest_exp };
            } else {
                long double rest_exp = log2l(c) + log2l(log2b);
                vector<ull> rest;  
                return { rest, rest_exp };
            }
        }

        c = powl(b, c);
    }

    return { {}, log2l(log2l(c)) };
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    size_t n;
    if (!(cin >> n)) return 0;
    string line;
    getline(cin, line); 

    vector<pair<TowerForm, size_t>> towers;
    vector<string> original(n);

    for (size_t i = 0; i < n; ++i) {
        getline(cin, line);
        original[i] = line;

        vector<ull> exps;
        {
            stringstream ss(line);
            string part;
            while (getline(ss, part, '^')) {
                ull x = stoull(part);
                if (x == 1) break;
                exps.push_back(x);
            }
            if (exps.empty()) {
                exps.push_back(1);
            }
        }

        towers.emplace_back(leo_tower_form(exps), i);
    }

    stable_sort(towers.begin(), towers.end(),
        [&](auto& A, auto& B){
            return leo_cmp(B.first, A.first);
    });


    cout << "Case 1:\n";
    for (auto& p : towers) {
        cout << original[p.second] << "\n";
    }

    return 0;
}

