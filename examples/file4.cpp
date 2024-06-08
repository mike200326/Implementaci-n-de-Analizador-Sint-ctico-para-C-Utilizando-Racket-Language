#include <iostream>
#include <vector>

void printNumbers(const std::vector<int>& numbers) {
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

void printSquares(const std::vector<int>& numbers) {
    for (int num : numbers) {
        std::cout << num * num << " ";
    }
    std::cout << std::endl;
}

void printCubes(const std::vector<int>& numbers) {
    for (int num : numbers) {
        std::cout << num * num * num << " ";
    }
    std::cout << std::endl;
}

int main() {
    std::vector<int> numbers;
    for (int i = 1; i <= 20; ++i) {
        numbers.push_back(i);
    }

    std::cout << "Numbers: ";
    printNumbers(numbers);

    std::cout << "Squares: ";
    printSquares(numbers);

    std::cout << "Cubes: ";
    printCubes(numbers);

    int a = 5;
    int b = 10;
    int c = 15;
    int d = 20;
    int sum = a + b + c + d;
    int product = a * b * c * d;
    std::cout << "Sum: " << sum << std::endl;
    std::cout << "Product: " << product << std::endl;

    if (sum > product) {
        std::cout << "Sum is greater than product." << std::endl;
    } else {
        std::cout << "Product is greater than or equal to sum." << std::endl;
    }

    for (int i = 1; i <= 5; ++i) {
        std::cout << "Number: " << i << ", Square: " << i * i << ", Cube: " << i * i * i << std::endl;
    }

    return 0;
}
