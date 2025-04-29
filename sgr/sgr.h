// "syntactic sugar"

#ifndef SGR_H
#define SGR_H

namespace sgr {

// see: https://en.cppreference.com/w/cpp/utility/variant/visit
template <typename... Ts> struct overloads : Ts... {
  using Ts::operator()...;
};

// deduction guide needed pre-c++20: https://stackoverflow.com/a/75699136
template <typename... Ts> overloads(Ts...) -> overloads<Ts...>;

} // namespace sgr

#endif
