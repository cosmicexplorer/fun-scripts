// following example from:
// http://www.boost.org/doc/libs/1_58_0/libs/spirit/example/qi/employee.cpp

// std includes
#include <iostream>
#include <string>
// boost includes
#include <boost/spirit/include/qi.hpp>

namespace frontend {
namespace spirit = boost::spirit;
namespace qi = spirit::qi;
namespace ascii = spirit::ascii;

struct cursor {
  std::string file;
  unsigned long long offset;
  unsigned long long line;
  unsigned long long col;
  // verify inputs using enum
  // decl/ref/defn/call
  std::string reference_type;
  // variable/function/scope/label/type
  std::string specifier;
  // if variable/function, then type
  std::string type;
  std::string language;
  std::string name;
  std::string scope;
};
}

// adapt struct to boost fusion
BOOST_FUSION_ADAPT_STRUCT(frontend::cursor, (std::string, file),
                          (unsigned long long, offset),
                          (unsigned long long, line), (unsigned long long, col),
                          (std::string, reference_type),
                          (std::string, specifier), (std::string, type),
                          (std::string, language), (std::string, name),
                          (std::string, scope));

namespace frontend {
template <typename Iterator>
struct cursor_parser : qi::grammar<Iterator, cursor(), ascii::space_type> {
  qi::rule<Iterator, std::string(), ascii::space_type> quoted_string;
  qi::rule<Iterator, cursor(), ascii::space_type> start;

  cursor_parser() : cursor_parser::base_type(start) {
    using qi::uint_;
    using qi::lit;
    using qi::double_;
    using qi::lexeme;
    using ascii::char_;

    quoted_string %= lexeme['"' >> +(char_ - '"') >> '"'];

    start %=
        // file
        quoted_string >> ',' >>
        // offset
        uint_ >> ',' >>
        // line
        uint_ >> ',' >>
        // col
        uint_ >> ',' >>
        // reference_type
        quoted_string >> ',' >>
        // specifier
        quoted_string >> ',' >>
        // type
        quoted_string >> ',' >>
        // language
        quoted_string >> ',' >>
        // name
        quoted_string >> ',' >>
        // scope
        quoted_string;
  }
};

template <typename Iterator>
bool parse_cursor_as_vector(Iterator first, Iterator last,
                            std::vector<cursor> &v) {
  using qi::phrase_parse;
  using qi::_1;
  bool r = phrase_parse(first, last, (*(cursor_parser<Iterator>())),
                        ascii::space, v);
  if (first != last) {
    return false;
  }
  return r;
}

template <typename T>
class cin_forward_iterator : std::iterator<std::forward_iterator_tag, T> {
private:
  std::istream_iterator<T> i;

public:
  cin_forward_iterator() : i(std::istream_iterator<T>()) {}
  cin_forward_iterator(std::istream &in) : i(std::istream_iterator<T>(in)) {}
  const T &operator*() { return *i; }
  cin_forward_iterator<T> operator++() {
    ++i;
    return *this;
  };
  cin_forward_iterator<T> operator++(int) {
    cin_forward_iterator<T> tmp = *this;
    i++;
    return tmp;
  };
  bool operator==(const cin_forward_iterator<T> &rhs) const {
    return i == rhs.i;
  }
  bool operator!=(const cin_forward_iterator<T> &rhs) const {
    return not (*this == rhs);
  }
};
}

namespace std {
template <typename T> class iterator_traits<frontend::cin_forward_iterator<T>> {
public:
  typedef typename std::istream_iterator<T>::value_type value_type;
  typedef typename std::istream_iterator<T>::difference_type difference_type;
  typedef typename std::istream_iterator<T>::reference reference;
  typedef typename std::istream_iterator<T>::pointer pointer;
  typedef std::forward_iterator_tag iterator_category;
};
}

int main() {
  // std::string str;
  // frontend::cursor_parser<std::string::const_iterator> cp;
  // std::vector<frontend::cursor> v;
  // while (getline(std::cin, str)) {
  //   frontend::cursor c;
  //   std::string::const_iterator start = str.begin();
  //   std::string::const_iterator end = str.end();
  //   bool res = phrase_parse(start, end, cp, boost::spirit::ascii::space, c);
  //   if (res and start == end) {
  //     v.push_back(c);
  //   } else {
  //     std::cerr << "failed!" << std::endl;
  //     return 1;
  //   }
  // }
  // std::stringstream buf;
  // buf << std::cin.rdbuf();
  // std::string s(buf.str());
  std::vector<frontend::cursor> v;
  if (frontend::parse_cursor_as_vector(
          frontend::cin_forward_iterator<char>(std::cin),
          frontend::cin_forward_iterator<char>(),
          // s.begin(), s.end(),
          // std::istream_iterator<char>(std::cin),
          // std::istream_iterator<char>(),
          v)) {
    for (auto &c : v) {
      std::cout << boost::fusion::as_vector(c) << std::endl;
    }
    std::cerr << "completed!" << std::endl;
    return 0;
  } else {
    std::cerr << "failed!" << std::endl;
    return 1;
  }
}
