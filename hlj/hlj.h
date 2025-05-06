#pragma once

#include <atomic>
#include <chrono>
#include <cxxabi.h>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <ratio>
#include <set>
#include <sstream>
#include <string>
#include <thread>

namespace hlj {

// template metaprogramming

template <size_t I, typename... Ts> struct nth_t;

template <typename T, typename... Ts> struct nth_t<0, T, Ts...> {
  using type = T;
};

template <size_t I, typename T, typename... Ts>
struct nth_t<I, T, Ts...> : nth_t<I - 1, Ts...> {};

template <size_t I, typename... Ts> using nth = typename nth_t<I, Ts...>::type;

// see: https://en.cppreference.com/w/cpp/utility/variant/visit
template <typename... Ts> struct overloads : Ts... {
  using Ts::operator()...;
};

// deduction guide needed pre-c++20: https://stackoverflow.com/a/75699136
template <typename... Ts> overloads(Ts...) -> overloads<Ts...>;

// aliases

using uint = unsigned int;
using String = std::string;
template <typename E> using List = std::vector<E>;
template <typename E> using Queue = std::queue<E>;
template <typename E> using Set = std::set<E>;
template <typename F, typename S> using Pair = std::pair<F, S>;
template <typename K, typename V> using Map = std::map<K, V>;
template <typename... Es> using Tuple = std::tuple<Es...>;
template <typename T> using Optional = std::optional<T>;
// template <typename R, typename... As> using Function =
// std::function<R(As...)>;
template <typename F> using Function = std::function<F>;
template <size_t... Is> using Seq = std::index_sequence<Is...>;
template <typename... Ts>
static constexpr auto SeqFor = std::index_sequence_for<Ts...>();
template <bool C> using EnableIf = std::enable_if_t<C, bool>;
template <size_t I, typename... Ts> nth<I, Ts...> Get(const Tuple<Ts...> &t);
using namespace std::chrono_literals;
using Nanoseconds = std::chrono::nanoseconds;
using Microseconds = std::chrono::microseconds;
using Milliseconds = std::chrono::milliseconds;
using Seconds = std::chrono::seconds;
using Minutes = std::chrono::minutes;
using Hours = std::chrono::hours;
using std::chrono::duration_cast;
using TimePoint = std::chrono::time_point<std::chrono::steady_clock>;
TimePoint now();
using std::move;
using std::this_thread::sleep_for;
using Thread = std::thread;

// type metadata

namespace detail {
template <typename T> struct Type {
  static const String name;
};
} // namespace detail

template <typename T> static const String type_name = detail::Type<T>::name;

// misc utils

template <typename K, typename V> struct Entry {
  K key;
  V value;

  bool operator==(const Entry &other) const;
};

bool starts_with(const String &s, const String &p);
bool ends_with(const String &s, const String &p);
template <typename F, typename C,
          typename = std::void_t<typename C::value_type>>
auto map(F f, const C &c);
List<char> as_list(const String &s);
template <typename E> List<Pair<size_t, E>> enumerate(const List<E> &l);
List<Pair<size_t, char>> enumerate(const String &s);
String join(String sep, const List<String> &l);
template <typename E> void assign(Set<E> &s1, const Set<E> &s2);
template <typename E> void assign(List<E> &l1, const List<E> &l2);
template <typename K, typename V>
void assign(Map<K, V> &m1, const Map<K, V> &m2);
template <typename C> C combine(const C &c1, const C &c2);
template <typename C, typename... Cs>
C combine(const C &first, const C &second, const Cs &...rest);
template <typename E> Set<E> intersection(const Set<E> &s1, const Set<E> &s2);
template <typename E> bool disjoint(const Set<E> &s1, const Set<E> &s2);
template <typename K, typename V> Set<K> keys(const Map<K, V> &m);
template <typename K, typename V> Set<V> values(const Map<K, V> &m);
template <typename K, typename V> List<Entry<K, V>> entries(const Map<K, V> &m);
template <typename E> List<E> repeat(size_t n, const E &e);
template <size_t N> List<String> repeat(size_t n, const char (&s)[N]);
bool in(const String &p, const String &s);

// string representation

String repr(char v);
String repr(signed char v);
String repr(short v);
String repr(int v);
String repr(long v);
String repr(long long v);
String repr(unsigned char v);
String repr(unsigned short v);
String repr(unsigned int v);
String repr(unsigned long v);
String repr(unsigned long long v);
String repr(float v);
String repr(double v);
String repr(long double v);
String repr(bool v);
String repr(const char *v);
template <size_t N> String repr(const char (&v)[N]);
String repr(const String &v);
String repr(const std::string_view &v);
String repr(const std::nullopt_t &);
template <typename T> String repr(const Optional<T> &v);
template <typename E> String repr(const List<E> &v);
template <typename E> String repr(const Set<E> &v);
template <typename K, typename V> String repr(const Map<K, V> &v);
template <typename... Ts> String repr(const Tuple<Ts...> &v);

template <typename T> String str(const T &v);
String str(char v);
String str(const char *v);
String str(const String &v);
String str(const std::string_view &v);

// string formatting

namespace detail {
void format(std::stringstream &ss, size_t &i, const String &s);
template <typename T, typename... Ts>
void format(std::stringstream &ss, size_t &i, const String &s, const T &v0,
            const Ts &...vs);

} // namespace detail

template <typename... Ts> String format(const String &s, const Ts &...vs);

// string manipulation

struct Indent {
  size_t stop = 1;
  size_t size = 2;

  String apply(const String &s) const;
  String operator+(const String &s) const;
  Indent operator+(size_t delta) const;
};

struct Bracket {
  enum class Kind {
    Brackets,
    Parentheses,
    Braces,
    AngleBrackets,
  };

  enum class Style {
    Plain,
    Spaced,
    Block,
  };

  Kind kind{Kind::Brackets};
  Style style{Style::Plain};
  Indent indent{};

  Bracket() {}
  Bracket(Style style_) : style(style_) {}
  Bracket(Kind kind_, Style style_) : kind(kind_), style(style_) {}
  Bracket(Style style_, Kind kind_) : kind(kind_), style(style_) {}
  Bracket(Kind kind_, Indent indent_)
      : kind(kind_), style(Style::Block), indent(indent_) {}
  String apply(const String &s) const;
};

String indent(const String &s, const Indent &ind = {});
String bracket(const String &s, const Bracket &brk = {});
String bracket(const String &s, const Indent &ind);
String parenthesize(const String &s,
                    const Bracket::Style &sty = Bracket::Style::Plain);
String parenthesize(const String &s, const Indent &ind);
String brace(const String &s,
             const Bracket::Style &sty = Bracket::Style::Plain);
String brace(const String &s, const Indent &ind);
String angle_bracket(const String &s,
                     const Bracket::Style &sty = Bracket::Style::Plain);
String angle_bracket(const String &s, const Indent &ind);

// pattern matching

static constexpr struct Any {
} any;

template <typename T> class Match {
public:
  Match(Function<bool(const T &)> pred_);
  Match();
  template <typename... Ts> Match(const T &match, const Ts &...rest);
  Match(Any);

  bool match(const T &value) const;

private:
  Function<bool(const T &)> pred;
};

template <typename... Ts> class Pattern {
public:
  Pattern(const Match<Ts> &...pattern_);
  template <typename... Us, EnableIf<sizeof...(Ts) == 1> = true>
  Pattern(const Us &...pattern_);
  Pattern(Any);

  bool match(const Ts &...values) const;

private:
  Tuple<Match<Ts>...> pattern;

  // todo: ugly
  bool match_any = false;

  template <size_t... Is> bool match(Seq<Is...>, const Ts &...args) const;
};

template <typename R, typename... Ts>
using Matcher = Function<R(const List<Pair<Pattern<Ts...>, R>> &)>;

template <typename R = void, typename... Ts>
Matcher<R, Ts...> match(const Ts &...values, const R &default_result);

template <typename R = void, typename... Ts>
Matcher<R, Ts...> match(const Ts &...values);

template <typename R = void, typename... Ts>
Matcher<Function<R()>, Ts...> match_do(const Ts &...values,
                                       const R &default_result);

template <typename R = void, typename... Ts>
Matcher<Function<R()>, Ts...> match_do(const Ts &...values);

// autodispatch

template <typename Dispatcher> class AutoDispatch {
public:
  AutoDispatch(Dispatcher &&dispatcher, Seconds timeout, Milliseconds interval)
      : dispatcher(move(dispatcher)) {
    const bool live_forever = (timeout == 0s);
    const auto end = now() + timeout;
    thread = std::thread([=]() {
      while (!stop_signaled && (live_forever || now() < end)) {
        dispatcher.dispatch();
        sleep_for(interval);
      }
    });
  }

  template <typename Timeout, typename Interval>
  AutoDispatch(Dispatcher &&dispatcher, Timeout timeout, Interval interval)
      : AutoDispatch(move(dispatcher), duration_cast<Seconds>(timeout),
                     duration_cast<Milliseconds>(interval)) {}

  template <typename Timeout>
  AutoDispatch(Dispatcher &&dispatcher, Timeout timeout)
      : AutoDispatch(move(dispatcher), timeout, 50ms) {}

  AutoDispatch(Dispatcher &&dispatcher) : AutoDispatch(move(dispatcher), 0s) {}

  virtual ~AutoDispatch() noexcept { stop(); }

  AutoDispatch(AutoDispatch &&other) noexcept = default;

  AutoDispatch &operator=(AutoDispatch &&other) noexcept = default;

  const Dispatcher &operator*() const { return dispatcher; }

  Dispatcher &operator*() { return dispatcher; }

  const Dispatcher *operator->() const { return &dispatcher; }

  Dispatcher *operator->() { return &dispatcher; }

  void signal_stop() { stop_signaled = true; }

  void stop() {
    signal_stop();
    join();
  }

  void join() {
    if (thread.joinable()) {
      thread.join();
    }
  }

private:
  Dispatcher dispatcher;
  Thread thread;
  std::atomic_bool stop_signaled = false;
};

} // namespace hlj

// aliases

template <size_t I, typename... Ts>
inline hlj::nth<I, Ts...> hlj::Get(const Tuple<Ts...> &t) {
  return std::get<I>(t);
}

inline hlj::TimePoint hlj::now() { return std::chrono::steady_clock::now(); }

// type metadata

// see: https://stackoverflow.com/a/20170989
template <typename T>
const hlj::String hlj::detail::Type<T>::name = []() {
  using TR = std::remove_reference_t<T>;

  std::unique_ptr<char, void (*)(void *)> own(
      abi::__cxa_demangle(typeid(TR).name(), nullptr, nullptr, nullptr),
      std::free);

  std::stringstream r;
  r << own.get();

  if (std::is_const_v<TR>) {
    r << " const";
  }

  if (std::is_volatile_v<TR>) {
    r << " volatile";
  }

  if (std::is_lvalue_reference_v<T>) {
    r << " &";
  } else if (std::is_rvalue_reference_v<T>) {
    r << " &&";
  }

  return r.str();
}();

// misc utils

inline bool hlj::starts_with(const String &s, const String &p) {
  return (s.size() >= p.size()) && (s.substr(0, p.size()) == p);
}

inline bool hlj::ends_with(const String &s, const String &p) {
  return (s.size() >= p.size()) && (s.substr(s.size() - p.size()) == p);
}

template <typename F, typename C, typename>
inline auto hlj::map(F f, const C &c) {
  using E = typename C::value_type;
  using R = decltype(f(std::declval<E>()));
  List<R> r;
  for (const auto &e : c) {
    r.push_back(f(e));
  }
  return r;
}

inline hlj::List<char> hlj::as_list(const String &s) {
  List<char> r;
  for (char c : s) {
    r.push_back(c);
  }
  return r;
}

template <typename E>
inline hlj::List<hlj::Pair<size_t, E>> hlj::enumerate(const List<E> &l) {
  size_t i = 0;
  return map([&](E e) { return std::make_pair(i++, e); }, l);
}

inline hlj::List<hlj::Pair<size_t, char>> hlj::enumerate(const String &s) {
  return enumerate(as_list(s));
}

inline hlj::String hlj::join(String sep, const List<String> &l) {
  std::stringstream r;
  for (auto const &[i, s] : enumerate(l)) {
    if (i) {
      r << sep;
    }
    r << s;
  }
  return r.str();
}

template <typename E> inline void hlj::assign(Set<E> &s1, const Set<E> &s2) {
  for (const auto &e : s2) {
    s1.insert(e);
  }
}

template <typename E> inline void hlj::assign(List<E> &l1, const List<E> &l2) {
  for (const auto &e : l2) {
    l1.push_back(l2);
  }
}

template <typename K, typename V>
inline void hlj::assign(Map<K, V> &m1, const Map<K, V> &m2) {
  for (const auto &[k, v] : m2) {
    m1[k] = v;
  }
}

template <typename C> inline C hlj::combine(const C &c1, const C &c2) {
  C r{c1};
  assign(r, c2);
  return r;
}

template <typename C, typename... Cs>
C hlj::combine(const C &first, const C &second, const Cs &...rest) {
  return combine(combine(first, second), rest...);
}

template <typename E>
inline hlj::Set<E> hlj::intersection(const Set<E> &s1, const Set<E> &s2) {
  Set<E> s;
  for (const auto &e : s1) {
    if (s2.count(e)) {
      s.insert(e);
    }
  }
  return s;
}

template <typename E>
inline bool hlj::disjoint(const Set<E> &s1, const Set<E> &s2) {
  return intersection(s1, s2).size() == 0;
}

template <typename K, typename V>
inline hlj::Set<K> hlj::keys(const Map<K, V> &m) {
  Set<K> r;
  for (const auto &[k, _] : m) {
    r.insert(k);
  }
  return r;
}

template <typename K, typename V>
inline hlj::Set<V> hlj::values(const Map<K, V> &m) {
  Set<V> r;
  for (const auto &[_, v] : m) {
    r.insert(v);
  }
  return r;
}

template <typename K, typename V>
inline hlj::List<hlj::Entry<K, V>> hlj::entries(const Map<K, V> &m) {
  return map([&](auto k) { return Entry{k, m.at(k)}; }, keys(m));
}

template <typename E> inline hlj::List<E> hlj::repeat(size_t n, const E &e) {
  List<E> r;
  for (size_t i = 0; i < n; i++) {
    r.push_back(e);
  }
  return r;
}

template <size_t N>
inline hlj::List<hlj::String> hlj::repeat(size_t n, const char (&s)[N]) {
  List<String> r;
  for (size_t i = 0; i < n; i++) {
    r.push_back(String(s));
  }
  return r;
}

inline bool hlj::in(const String &p, const String &s) {
  return s.find(p) != String::npos;
}

// string representation

inline hlj::String hlj::repr(char v) { return hlj::format("'{}'", String{v}); }

inline hlj::String hlj::repr(signed char v) { return std::to_string(v); }

inline hlj::String hlj::repr(short v) { return std::to_string(v); }

inline hlj::String hlj::repr(int v) { return std::to_string(v); }

inline hlj::String hlj::repr(long v) { return std::to_string(v); }

inline hlj::String hlj::repr(long long v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned char v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned short v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned int v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned long v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned long long v) { return std::to_string(v); }

inline hlj::String hlj::repr(float v) { return std::to_string(v); }

inline hlj::String hlj::repr(double v) { return std::to_string(v); }

inline hlj::String hlj::repr(long double v) { return std::to_string(v); }

inline hlj::String hlj::repr(bool v) { return v ? "true" : "false"; }

inline hlj::String hlj::repr(const char *v) { return "\"" + String(v) + "\""; }

inline hlj::String hlj::repr(const String &v) { return "\"" + v + "\""; }

template <size_t N> inline hlj::String hlj::repr(const char (&v)[N]) {
  return hlj::repr(String(v));
}

inline hlj::String hlj::repr(const std::string_view &v) {
  return hlj::repr(String(v));
}

inline hlj::String hlj::repr(const std::nullopt_t &) { return "std::nullopt"; }

template <typename T> inline hlj::String hlj::repr(const std::optional<T> &v) {
  return v ? repr(*v) : "std::nullopt";
}

template <typename E> inline hlj::String hlj::repr(const std::vector<E> &v) {
  return format("[{}]",
                join(", ", map([&](const E &e) { return repr(e); }, v)));
}

template <typename T> inline hlj::String hlj::repr(const std::set<T> &v) {
  return format("{{{}}}",
                join(", ", map([&](const auto &e) { return repr(e); }, v)));
}

template <typename K, typename V> hlj::String hlj::repr(const Map<K, V> &v) {
  return format("{{{}}}", join(", ", map(
                                         [&](const auto &e) {
                                           const auto &[k, v] = e;
                                           return format("{} => {}", k, v);
                                         },
                                         entries(v))));
}

template <typename... Ts>
inline hlj::String hlj::repr(const std::tuple<Ts...> &v) {
  const auto repr_ = [&](const Ts &...vs) {
    std::vector<String> list;
    (list.push_back(repr(vs)), ...);
    return format("({})", join(", ", list));
  };

  return std::apply(repr_, v);
}

template <typename T> inline hlj::String hlj::str(const T &v) {
  return repr(v);
}

inline hlj::String hlj::str(char v) { return String{v}; }

inline hlj::String hlj::str(const char *v) { return v; }

inline hlj::String hlj::str(const String &v) { return v; }

inline hlj::String hlj::str(const std::string_view &v) { return String(v); }

// string formatting

inline void hlj::detail::format(std::stringstream &ss, size_t &i,
                                const String &s) {
  const size_t n = s.size();
  while (i < n) {
    const char c = s[i++];
    switch (c) {
    case '{': {
      if (i < n && s[i++] == '{') {
        ss << '{';
      } else {
        // todo: this could be caused by not enough values supplied
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '{{')", s));
      }
      break;
    }
    case '}': {
      if (i < n && s[i++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      ss << c;
    }
    }
  }
}

template <typename T, typename... Ts>
inline void hlj::detail::format(std::stringstream &ss, size_t &i,
                                const String &s, const T &v0, const Ts &...vs) {
  const size_t n = s.size();
  bool flag = false;
  while (i < n) {
    const char c = s[i++];
    switch (c) {
    case '{': {
      if (i < n && s[i] == '{') {
        i++;
        ss << '{';
      } else {
        flag = !flag;
      }
      break;
    }
    case '}': {
      if (flag) {
        ss << hlj::str(v0);
        format(ss, i, s, vs...);
        return;
      } else if (i < n && s[i++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      if (flag) {
        throw std::invalid_argument(hlj::format(
            "invalid format string: \"{}\" (format specifiers not supported)",
            s));
      } else {
        ss << c;
      }
      break;
    }
    }
  }
  throw std::invalid_argument(hlj::format("invalid format string: \"{}\"", s));
}

template <typename... Ts>
inline hlj::String hlj::format(const String &s, const Ts &...vs) {
  std::stringstream ss;
  size_t i = 0;
  detail::format(ss, i, s, vs...);
  return ss.str();
}

// string manipulation

inline hlj::String hlj::Indent::apply(const String &s) const {
  const std::string tab(stop * size, ' ');
  std::stringstream r;
  r << tab;
  const size_t n = s.size();
  for (const auto &[i, c] : enumerate(s)) {
    switch (c) {
    case '\n':
      r << c;
      if (i + 1 < n && s[i + 1] != '\n') {
        r << tab;
      }
      break;
    default:
      r << c;
      break;
    }
  }
  return r.str();
}

inline hlj::String hlj::Indent::operator+(const String &s) const {
  return apply(s);
}

inline hlj::Indent hlj::Indent::operator+(size_t delta) const {
  return {stop + delta, size};
}

inline std::string hlj::Bracket::apply(const std::string &s) const {
  std::string open;
  std::string close;
  switch (kind) {
  case Kind::Parentheses:
    open = "(";
    close = ")";
    break;
  case Kind::Brackets:
    open = "[";
    close = "]";
    break;
  case Kind::Braces:
    open = "{";
    close = "}";
    break;
  case Kind::AngleBrackets:
    open = "<";
    close = ">";
    break;
  }

  if (s == "") {
    return join("", {open, close});
  }

  switch (style) {
  case Style::Plain:
    return join("", {open, s, close});

  case Style::Spaced:
    return join(" ", {open, s, close});

  case Style::Block:
    return join("\n", {open, indent + s, close});
  }
  return s;
}

inline hlj::String hlj::indent(const String &s, const Indent &ind) {
  return ind + s;
}

inline hlj::String hlj::bracket(const std::string &s, const Bracket &bracket) {
  return bracket.apply(s);
}

inline hlj::String hlj::bracket(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Brackets, indent}.apply(s);
}

inline hlj::String hlj::parenthesize(const std::string &s,
                                     const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Parentheses, style}.apply(s);
}

inline hlj::String hlj::parenthesize(const std::string &s,
                                     const Indent &indent) {
  return Bracket{Bracket::Kind::Parentheses, indent}.apply(s);
}

inline hlj::String hlj::brace(const std::string &s,
                              const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Braces, style}.apply(s);
}

inline hlj::String hlj::brace(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Braces, indent}.apply(s);
}

inline hlj::String hlj::angle_bracket(const std::string &s,
                                      const Bracket::Style &style) {
  return Bracket{Bracket::Kind::AngleBrackets, style}.apply(s);
}

inline hlj::String hlj::angle_bracket(const std::string &s,
                                      const Indent &indent) {
  return Bracket{Bracket::Kind::AngleBrackets, indent}.apply(s);
}

// pattern matching

template <typename T>
hlj::Match<T>::Match(Function<bool(const T &)> pred_) : pred(pred_) {}

template <typename T>
hlj::Match<T>::Match() : Match([](auto) { return false; }) {}

template <typename T>
template <typename... Ts>
hlj::Match<T>::Match(const T &match, const Ts &...rest)
    : Match([match, rest_match = Match(rest...)](const T &value) {
        return (value == match) || rest_match.match(value);
      }) {}

template <typename T>
hlj::Match<T>::Match(Any) : Match([](auto) { return true; }) {}

template <typename T> bool hlj::Match<T>::match(const T &value) const {
  return pred(value);
}

template <typename... Ts>
hlj::Pattern<Ts...>::Pattern(const Match<Ts> &...pattern_)
    : pattern(std::make_tuple(pattern_...)) {}

template <typename... Ts>
template <typename... Us, hlj::EnableIf<sizeof...(Ts) == 1>>
hlj::Pattern<Ts...>::Pattern(const Us &...pattern_)
    : pattern(Match<nth<0, Ts...>>(pattern_...)) {}

template <typename... Ts> hlj::Pattern<Ts...>::Pattern(Any) : match_any(true) {}

template <typename... Ts>
bool hlj::Pattern<Ts...>::match(const Ts &...values) const {
  return match(SeqFor<Ts...>, values...);
}

template <typename... Ts>
template <size_t... Is>
bool hlj::Pattern<Ts...>::match(Seq<Is...>, const Ts &...args) const {
  return match_any || (Get<Is>(pattern).match(args) && ...);
}

template <typename R, typename... Ts>
hlj::Matcher<R, Ts...> hlj::match(const Ts &...values,
                                  const R &default_result) {
  return [&](const auto &pattern_results) {
    for (const auto &[pattern, result] : pattern_results) {
      if (pattern.match(values...)) {
        return result;
      }
    }
    return default_result;
  };
}

template <typename R, typename... Ts>
hlj::Matcher<R, Ts...> hlj::match(const Ts &...values) {
  return [&](const auto &pattern_results) {
    for (const auto &[pattern, result] : pattern_results) {
      if (pattern.match(values...)) {
        return result;
      }
    }
    throw std::invalid_argument("no match");
  };
}

template <typename R, typename... Ts>
hlj::Matcher<hlj::Function<R()>, Ts...> hlj::match_do(const Ts &...values,
                                                      const R &default_result) {
  return [&](const auto &pattern_actions) {
    return match<Function<R()>>(values...)(pattern_actions, default_result)();
  };
}

template <typename R, typename... Ts>
hlj::Matcher<hlj::Function<R()>, Ts...> hlj::match_do(const Ts &...values) {
  return [&](const auto &pattern_actions) {
    return match<Function<R()>>(values...)(pattern_actions)();
  };
}
