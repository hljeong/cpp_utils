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

template <typename F, typename C,
          typename = std::void_t<typename C::value_type>>
inline auto map(F f, const C &c) {
  using E = typename C::value_type;
  using R = decltype(f(std::declval<E>()));
  List<R> r;
  for (const auto &e : c) {
    r.push_back(f(e));
  }
  return r;
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

template <typename T> inline void assign(Set<T> &s1, const Set<T> &s2) {
  for (const auto &e : s2) {
    s1.insert(e);
  }
}

template <typename T> inline void assign(List<T> &l1, const List<T> &l2) {
  for (const auto &e : l2) {
    l1.push_back(e);
  }
}

template <typename K, typename V>
inline void assign(Map<K, V> &m1, const Map<K, V> &m2) {
  for (const auto &[k, v] : m2) {
    m1[k] = v;
  }
}

template <typename C> inline C combine(const C &c1, const C &c2) {
  C c = c1;
  assign(c, c2);
  return c;
}

template <typename T, typename... Ts>
inline T combine(const T &first, const T &second, const Ts &...rest) {
  return combine(combine(first, second), rest...);
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

template <typename K, typename V> struct Entry {
  K key;
  V value;

  bool operator==(const Entry &other) const {
    return (key == other.key) && (value == other.value);
  }
};

template <typename K, typename V>
inline List<Entry<K, V>> entries(const Map<K, V> &m) {
  return map([&](auto k) { return Entry<K, V>{k, m.at(k)}; }, keys(m));
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

inline bool in(const std::string &pattern, const std::string &s) {
  return s.find(pattern) != std::string::npos;
}

} // namespace cpy

#endif
