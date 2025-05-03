// "pythonic c++"

#ifndef CPY_H
#define CPY_H

#include <functional>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace cpy {

using uint = unsigned;
using String = std::string;
template <typename T> using List = std::vector<T>;
template <typename T> using Queue = std::queue<T>;
template <typename T> using Set = std::set<T>;
template <typename F, typename S> using Pair = std::pair<F, S>;
template <typename K, typename V> using Map = std::map<K, V>;
template <typename... Ts> using Tuple = std::tuple<Ts...>;

template <typename F, typename T> inline auto map(F func, const List<T> &list) {
  List<decltype(func(std::declval<T>()))> ret;
  for (const auto &elem : list) {
    ret.push_back(func(elem));
  }
  return ret;
}

template <typename F> inline auto map(F func, const String &s) {
  List<decltype(func(std::declval<char>()))> ret;
  for (const auto &c : s) {
    ret.push_back(func(c));
  }
  return ret;
}

template <typename T>
inline List<Pair<size_t, T>> enumerate(const List<T> &list) {
  size_t i = 0;
  return map([&](T elem) { return std::make_pair(i++, elem); }, list);
}

inline List<Pair<size_t, char>> enumerate(const String &s) {
  size_t i = 0;
  return map([&](char c) { return std::make_pair(i++, c); }, s);
}

inline String join(String sep, const List<String> &list) {
  std::stringstream s;
  for (auto const &[idx, item] : enumerate(list)) {
    if (idx) {
      s << sep;
    }
    s << item;
  }
  return s.str();
}

template <typename T> inline Set<T> union_(const Set<T> &s1, const Set<T> s2) {
  Set<T> s;
  for (const auto &e : s1) {
    s.insert(e);
  }
  for (const auto &e : s2) {
    s.insert(e);
  }
  return s;
}

template <typename T>
inline Set<T> intersection(const Set<T> &s1, const Set<T> s2) {
  Set<T> s;
  for (const auto &e : s1) {
    if (s2.count(e)) {
      s.insert(e);
    }
  }
  return s;
}

template <typename T> inline bool disjoint(const Set<T> &s1, const Set<T> s2) {
  return intersection(s1, s2).size() == 0;
}

template <typename T>
inline List<T> concat(const List<T> &l1, const List<T> &l2) {
  List<T> l;
  for (const auto &e : l1) {
    l.push_back(e);
  }
  for (const auto &e : l2) {
    l.push_back(e);
  }
  return l;
}

template <typename K, typename V> inline Set<K> keys(const Map<K, V> &m) {
  Set<K> s;
  for (const auto &[k, _] : m) {
    s.insert(k);
  }
  return s;
}

template <typename K, typename V> inline Set<V> values(const Map<K, V> &m) {
  Set<V> s;
  for (const auto &[_, v] : m) {
    s.insert(v);
  }
  return s;
}

template <typename T> inline List<T> repeat(size_t n, const T &value) {
  List<T> l;
  for (size_t i = 0; i < n; i++) {
    l.push_back(value);
  }
  return l;
}

template <size_t N>
inline List<String> repeat(size_t n, const char (&value)[N]) {
  List<String> l;
  for (size_t i = 0; i < n; i++) {
    l.push_back(String(value));
  }
  return l;
}

template <typename K, typename V>
inline Map<K, V> update(const Map<K, V> &m1, const Map<K, V> &m2) {
  Map<K, V> m;
  for (const auto &[k, v] : m1) {
    m[k] = v;
  }
  for (const auto &[k, v] : m2) {
    m[k] = v;
  }
  return m;
}

} // namespace cpy

#endif
