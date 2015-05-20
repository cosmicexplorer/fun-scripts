#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <algorithm>

namespace semantic_code_browser {
namespace utils {
template <class UnaryOperation, class InSeqType>
InSeqType map(InSeqType l, UnaryOperation f) {
  InSeqType out_seq;
  std::transform(l.begin(), l.end(), std::back_inserter(out_seq), f);
  return out_seq;
}

template <class InSeqType, class OutSeqType>
OutSeqType convert(InSeqType l) {
  OutSeqType o;
  for (auto e : l) {
    o.push_back(e);
  }
  return o;
}

template <class BinaryOperation, class InSeqType>
typename InSeqType::value_type foldr(InSeqType l, BinaryOperation f) {
  return std::accumulate(++l.begin(), l.end(), *l.begin(), f);
}

template <class BinaryOperation, class InSeqType>
typename InSeqType::value_type foldl(InSeqType l, BinaryOperation f) {
  return std::accumulate(++l.rbegin(), l.rend(), *l.rbegin(), f);
}
}
}

#endif /* UTILITIES_HPP */
