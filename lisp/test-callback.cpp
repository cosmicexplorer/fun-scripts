/* testing callbacks from c++ code */

#include <stack>

std::stack<int> s;

extern "C" {
char easy_write(int j) {
  s.push(j);
  return s.size();
}

char return_your_return(char (*c)()) {
  s.push(static_cast<int>(c()));
  return s.size();
}

int get_top() { return s.top(); }
}

/* g++ test-callback.cpp -o test-callback++.so -shared -fPIC */
