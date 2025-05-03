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
template <typename E> using List = std::vector<E>;
template <typename E> using Queue = std::queue<E>;
template <typename E> using Set = std::set<E>;
template <typename F, typename S> using Pair = std::pair<F, S>;
template <typename K, typename V> using Map = std::map<K, V>;
template <typename... Es> using Tuple = std::tuple<Es...>;

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

template <typename E> inline List<Pair<size_t, E>> enumerate(const List<E> &l) {
  size_t i = 0;
  return map([&](E e) { return std::make_pair(i++, e); }, l);
}

inline List<char> as_list(const String &s) {
  List<char> r;
  for (char c : s) {
    r.push_back(c);
  }
  return r;
}

inline List<Pair<size_t, char>> enumerate(const String &s) {
  return enumerate(as_list(s));
}

inline String join(String sep, const List<String> &l) {
  std::stringstream r;
  for (auto const &[i, s] : enumerate(l)) {
    if (i) {
      r << sep;
    }
    r << s;
  }
  return r.str();
}

template <typename E> inline void assign(Set<E> &s1, const Set<E> &s2) {
  for (const auto &e : s2) {
    s1.insert(e);
  }
}

template <typename E> inline void assign(List<E> &l1, const List<E> &l2) {
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
  C r = c1;
  assign(r, c2);
  return r;
}

template <typename C, typename... Cs>
inline C combine(const C &first, const C &second, const Cs &...rest) {
  return combine(combine(first, second), rest...);
}

template <typename E>
inline Set<E> intersection(const Set<E> &s1, const Set<E> s2) {
  Set<E> r;
  for (const auto &e : s1) {
    if (s2.count(e)) {
      r.insert(e);
    }
  }
  return r;
}

template <typename E> inline bool disjoint(const Set<E> &s1, const Set<E> s2) {
  return intersection(s1, s2).size() == 0;
}

template <typename K, typename V> inline Set<K> keys(const Map<K, V> &m) {
  Set<K> r;
  for (const auto &[k, _] : m) {
    r.insert(k);
  }
  return r;
}

template <typename K, typename V> inline Set<V> values(const Map<K, V> &m) {
  Set<V> r;
  for (const auto &[_, v] : m) {
    r.insert(v);
  }
  return r;
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

template <typename E> inline List<E> repeat(size_t n, const E &e) {
  List<E> r;
  for (size_t i = 0; i < n; i++) {
    r.push_back(e);
  }
  return r;
}

template <size_t N> inline List<String> repeat(size_t n, const char (&s)[N]) {
  List<String> r;
  for (size_t i = 0; i < n; i++) {
    r.push_back(String(s));
  }
  return r;
}

inline bool in(const std::string &p, const std::string &s) {
  return s.find(p) != std::string::npos;
}

} // namespace cpy

#endif
