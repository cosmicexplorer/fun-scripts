#include <functional>
#include <type_traits>

/* TODO: figure out where libstdc++ optional is */
#include <boost/optional/optional.hpp>

namespace coro_iter {
template <typename V, typename S>
class result;

template <typename S, typename F>
struct FuncWrapper {
  using ResultType = typename std::result_of<F(S)>::type;
  using FunctionType = std::function<ResultType(S)>;

  FunctionType fun;

  FuncWrapper() : fun() {}
  FuncWrapper(F f) : fun(f) {}

  inline ResultType operator()(S state) const { return fun(state); }
};

template <typename S, typename F>
auto make_func_wrapper(F fun) {
  return FuncWrapper<S, F>(fun);
}

template <typename S, typename F>
class iter {
public:
  using StateType = S;
  /* result of F must have operator! and operator* defined
   * (e.g. optional) */
  using ResultType = typename std::result_of<F(S)>::type;
  using FunctionType = FuncWrapper<S, F>;

private:
  ResultType current;
  FunctionType obtain;

public:
  iter() : current(ResultType()), obtain() {}
  iter(ResultType cur, const FunctionType & fun) : current(cur), obtain(fun) {}

  bool operator!() const { return !is_end(); }
  auto operator*() { return current->value; };
  iter<S, F> next() { return make_iter_fun_ptr(current->state, obtain); }
  bool is_end() const { return !current; };
};

template <typename S, typename F>
inline iter<S, F> make_iter(const S & state, F obtain) {
  return make_iter_fun_ptr(state, make_func_wrapper<S, F>(obtain));
}

template <typename S, typename F>
inline iter<S, F> make_iter_fun_ptr(const S & state,
                                    const FuncWrapper<S, F> & obtain) {
  auto res = obtain(state);
  if (!res) {
    return iter<S, F>();
  } else {
    return iter<S, F>(res, obtain);
  }
}

template <typename V, typename S>
class result {
public:
  using ValueType = V;
  using StateType = S;

  ValueType value;
  StateType state;

public:
  result(ValueType val, StateType st) : value(val), state(st) {}
};

template <typename V, typename S>
inline auto make_result(const V & value, const S & state) {
  return boost::optional<result<V, S>>(result<V, S>(value, state));
}
}

#include <functional>
#include <iostream>
#include <vector>

int main() {
  using namespace coro_iter;
  auto vt = std::vector<int>{1, 2, 3};
  auto iter = make_iter(vt.begin(), [&](auto st) {
    if (st == vt.end()) {
      return boost::optional<result<int, std::vector<int>::iterator>>();
    } else {
      auto res = *st;
      return make_result(res, ++st);
    }
  });

  for (; !iter.is_end(); iter = iter.next()) {
    std::cerr << "el: " << *iter << std::endl;
  }
  std::cerr << "wow" << std::endl;
}

/* class Callable { */
/* private: */
/*   int x; */

/* public: */
/*   Callable(int num) : x(num) { */
/*   } */

/*   Callable(const Callable & rhs) : x(rhs.x) { */
/*     std::cerr << "copied!" << std::endl; */
/*   } */

/*   /\* Callable(const Callable &) = delete; *\/ */

/*   Callable(Callable && rhs) noexcept : x(std::move(rhs.x)) { */
/*     std::cerr << "moved!" << std::endl; */
/*   } */

/*   int operator()(int n) { */
/*     return x + n; */
/*   } */
/* }; */

/* template <typename F> */
/* class Test { */
/* public: */
/*   using ResultType = typename std::result_of<F(int)>::type; */
/*   using FunctionType = std::function<ResultType(int)>; */
/*   FunctionType fun; */
/*   /\* F fun; *\/ */

/*   Test(const F & f) : fun(f) { */
/*     std::cerr << "wow3" << std::endl; */
/*   } */
/* }; */

/* template <typename F> */
/* static Test<F> make_test(const F & f) { */
/*   /\* const auto & fref = f; *\/ */
/*   std::cerr << "wow2" << std::endl; */
/*   return Test<F>(f); */
/* } */

/* class ShowCopy { */
/* private: */
/*   int x; */

/* public: */
/*   ShowCopy(int n) : x(n) { */
/*   } */

/*   ShowCopy(const ShowCopy & rhs) : x(rhs.x) { */
/*     std::cerr << "copied ShowCopy!" << std::endl; */
/*   } */
/*   /\* ShowCopy(const ShowCopy &) = delete; *\/ */

/*   ShowCopy(ShowCopy && rhs) : x(std::move(rhs.x)) { */
/*     std::cerr << "moved ShowCopy!" << std::endl; */
/*   } */

/*   int get() const { */
/*     return x; */
/*   } */
/* }; */

/* auto get_test(int x) { */
/*   using namespace std::placeholders; */
/*   /\* const auto & fun = std::bind(std::plus<int>(), x, _1); *\/ */
/*   /\* return make_test(fun); *\/ */
/*   /\* return make_test(std::bind(std::plus<int>(), x, _1)); *\/ */
/*   /\* return make_test(Callable(x)); *\/ */
/*   ShowCopy x_show(x); */
/*   std::cerr << "here" << std::endl; */
/*   auto lam = [x_moved = std::move(x_show)](int a) { */
/*     std::cerr << "x_show.get() = " << x_moved.get() << std::endl; */
/*     return x_moved.get() + a; */
/*   }; */
/*   std::cerr << "here2" << std::endl; */
/*   return make_test(lam); */
/*   /\* return make_test([=](int a) { *\/ */
/*   /\*   std::cerr << "x_show.get() = " << x_show.get() << std::endl; *\/ */
/*   /\*   return x_show.get() + a; *\/ */
/*   /\* }); *\/ */
/* } */

/* int main() { */
/*   auto res = get_test(200); */
/*   std::cerr << "here3" << std::endl; */
/*   std::cerr << (res.fun)(3) << std::endl; */
/*   /\* using namespace std::placeholders; *\/ */
/*   /\* std::cerr << std::bind(std::plus<int>(), 200, _1)(3) << std::endl; *\/
 */
/*   std::cerr << "wow" << std::endl; */
/* } */
