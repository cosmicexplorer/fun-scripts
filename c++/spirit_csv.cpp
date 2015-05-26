// following example from:
// http://www.boost.org/doc/libs/1_58_0/libs/spirit/example/qi/employee.cpp, and
// num_list4.cpp, and others

#define BOOST_SPIRIT_DEBUG 1
#define BOOST_SPIRIT_DEBUG_PRINT_SOME 200
#define BOOST_SPIRIT_DEBUG_OUT std::cerr

// std includes
#include <iostream>
#include <string>
// boost includes
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>

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
BOOST_FUSION_ADAPT_STRUCT(
    frontend::cursor,
    (std::string, file)(unsigned long long, offset)(unsigned long long, line)(
        unsigned long long, col)(std::string, reference_type)(std::string,
                                                              specifier)(
        std::string, type)(std::string, language)(std::string,
                                                  name)(std::string, scope));

// note: blank_type is so that newlines aren't counted as skippable, because
// they are significant for csv! however, typically you'll be wanting to use
// boost::spirit::ascii::space as your whitespace operator if you really do not
// care about whitespace
namespace frontend {
template <typename Iterator>
struct cursor_parser
    : public qi::grammar<Iterator, std::vector<cursor>(), qi::blank_type> {
  qi::rule<Iterator, std::string(), qi::blank_type> quoted_string;
  qi::rule<Iterator, cursor(), qi::blank_type> start;
  qi::rule<Iterator, std::vector<cursor>(), qi::blank_type> vec;

  cursor_parser() : cursor_parser::base_type(vec) {
    using qi::uint_;
    using qi::eol;
    using qi::lexeme;
    using qi::_1;
    using ascii::char_;
    using boost::phoenix::push_back;
    using boost::phoenix::ref;
    using boost::spirit::_val;

    quoted_string %= lexeme['"' >> *(char_ - '"') >> '"'];

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

    vec %= start % eol;

    quoted_string.name("qs");
    start.name("s");
    vec.name("v");
    BOOST_SPIRIT_DEBUG_NODES((quoted_string)(start)(vec))
  }
};
}

// try cat file | ./spirit_csv, or alternatively,
/*
echo \
  "\"f\",111,222,333,\"ref_type\",\"spc\",\"type\",\"lan\",\"name\",\"scop\"" \
  | ./spirit_csv
*/
int main() {
  boost::spirit::istream_iterator start(std::cin >> std::noskipws), end;
  std::vector<frontend::cursor> v;
  if (phrase_parse(start, end,
                   frontend::cursor_parser<boost::spirit::istream_iterator>(),
                   boost::spirit::qi::blank, v)) {
    for (auto &c : v) {
      std::cout << boost::fusion::as_vector(c) << std::endl;
    }
    // if you try cat file2 | ./spirit_csv, it won't parse all the way to the
    // end of the input, so this inequality will be true. if you do
    // cat file | ./spirit_csv, the cursor will be right before the end of the
    // input (it won't parse the final newline), so ++start == end
    if (++start != end) {
      std::cerr << "lol" << std::endl;
    }
    std::cerr << "success!" << std::endl;
    return 0;
  } else {
    if (++start == end) {
      std::cerr << "asdf" << std::endl;
    }
    std::cout << v.size() << std::endl;
    std::cerr << "failure!" << std::endl;
    return 1;
  }
}
