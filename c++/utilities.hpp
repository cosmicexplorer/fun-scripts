#ifndef UTILITIES_HPP
#define UTILITIES_HPP

// fun attempts at functional stuff in c++

#include <algorithm>

// i wish i could figure out the templates to generically convert a sequence of
// one type into another; currently it can only spit out a sequence of the same
// type as the input, which is kinda useless for most map operations, which
// typically change the value_type of the container (list<int> from
// list<string>, for example)
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

#endif /* UTILITIES_HPP */
