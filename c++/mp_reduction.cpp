#include <omp.h>
#include <iostream>

struct Int {
  Int(int) = delete;
  explicit Int() : a(0)
  {
  }
  int a;
};

Int plus(Int a, Int b)
{
  Int res;
  std::cout << "ya" << std::endl;
  res.a = a.a + b.a;
  return res;
}
int main()
{
  size_t i;
  Int result;
#pragma omp declare reduction(plusReducer : Int : omp_out = \
                                  plus(omp_out, omp_in))    \
                                      initializer(omp_priv = Int())
#pragma omp parallel for private(i) reduction(plusReducer : result) \
    schedule(static, 5)
  for (i = 0; i < 5; ++i) {
    Int res;
    res.a  = result.a + i;
    result = res;
  }
  std::cout << result.a << std::endl;
}
