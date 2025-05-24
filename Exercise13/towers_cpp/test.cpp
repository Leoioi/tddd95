#include <iostream>
#include <limits>
#include <iomanip>

int main() {
    std::cout << std::scientific << std::setprecision(6);
    std::cout << "double max      = " 
              << std::numeric_limits<double>::max() << "\n";
    std::cout << "long double max = " 
              << std::numeric_limits<long double>::max() << "\n";
    return 0;
}
