#include <iostream>
#include <iterator>

int main() {
  for (auto i = std::istream_iterator<char>(std::cin);
       i != std::istream_iterator<char>(); ++i) {
    std::cout << *i << "|";
  }
  std::cout << std::endl;
}
