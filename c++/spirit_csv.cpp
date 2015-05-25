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
}

int main() {
  std::string s;
  frontend::cursor c;
  while (getline(std::cin, s)) {
    c = {};                     // zero it out
    if (phrase_parse(s.cbegin(), s.cend(),
                     frontend::cursor_parser<std::string::const_iterator>(),
                     boost::spirit::ascii::space, c)) {
      std::cout << boost::fusion::as_vector(c) << std::endl;
      std::cerr << c.file << std::endl;
    } else {
      std::cerr << "failed!" << std::endl;
      return 1;
    }
  }
  std::cerr << "completed!" << std::endl;
  return 0;
}
