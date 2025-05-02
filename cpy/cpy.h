// "c++ python"

#ifndef CPY_H
#define CPY_H

#include <functional>
#include <sstream>
#include <string>
#include <vector>

namespace cpy {

template <typename F, typename T>
inline auto map(F func, const std::vector<T> &list) {
  std::vector<decltype(func(std::declval<T>()))> ret;
  for (const auto &elem : list) {
    ret.push_back(func(elem));
  }
  return ret;
}

template <typename F> inline auto map(F func, const std::string &s) {
  std::vector<decltype(func(std::declval<char>()))> ret;
  for (const auto &c : s) {
    ret.push_back(func(c));
  }
  return ret;
}

template <typename T>
inline std::vector<std::pair<size_t, T>> enumerate(const std::vector<T> &list) {
  size_t i = 0;
  return map([&](T elem) { return std::make_pair(i++, elem); }, list);
}

inline std::vector<std::pair<size_t, char>> enumerate(const std::string &s) {
  size_t i = 0;
  return map([&](char c) { return std::make_pair(i++, c); }, s);
}

inline std::string join(std::string sep, const std::vector<std::string> &list) {
  std::stringstream s;
  for (auto const &[idx, item] : enumerate(list)) {
    if (idx) {
      s << sep;
    }
    s << item;
  }
  return s.str();
}

} // namespace cpy

#endif
