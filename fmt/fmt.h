// "format"

#ifndef FMT_H
#define FMT_H

#include <algorithm>
#include <cxxabi.h>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

#include "../cpy/cpy.h"

namespace fmt {

// todo: move this out
// see: https://stackoverflow.com/a/20170989
template <typename T> std::string type_name() {
  using TR = std::remove_reference_t<T>;

  std::unique_ptr<char, void (*)(void *)> own(
      abi::__cxa_demangle(typeid(TR).name(), nullptr, nullptr, nullptr),
      std::free);

  std::string r = own.get();

  if (std::is_const_v<TR>) {
    r += " const";
  }

  if (std::is_volatile_v<TR>) {
    r += " volatile";
  }

  if (std::is_lvalue_reference_v<T>) {
    r += " &";
  } else if (std::is_rvalue_reference_v<T>) {
    r += " &&";
  }

  return r;
}

// template <typename T> inline std::string repr(const T &);

// template <typename T> inline std::string str(const T &value);

// inline std::string repr(const char (&value)[]) {
//   return "\"" + std::string(value) + "\"";
// }

// inline std::string repr(const char *value) {
//   return "\"" + std::string(value) + "\"";
// }

// template <std::size_t N> inline std::string repr(const char (&value)[N]) {
//   return "\"" + std::string(value) + "\"";
// }

// template <std::size_t N> inline std::string str(const char (&value)[N]) {
//   return value;
// }
//
// inline void prints(const std::string &s) { printf("%s\n", s.c_str()); }

template <typename... Ts>
std::string format(const std::string &s, const Ts &...values);

namespace detail {

void format(std::stringstream &ss, size_t &idx, const std::string &s);

template <typename T, typename... Ts>
void format(std::stringstream &ss, size_t &idx, const std::string &s,
            const T &first, const Ts &...rest);

} // namespace detail

template <typename... Ts>
std::string format(const std::string &s, const Ts &...values);

// inline void print() {
// prints(""); }
//
// template <typename...
// Ts> inline void
// print(const std::string
// &s, const Ts &...values)
// {
//   prints(format(s,
//   values...));
// }
//
// template <typename...
// Ts> inline void
// printo(const Ts
// &...values) {
//   prints(format(cpy::join("
//   ",
//   cpy::repeat(sizeof...(Ts),
//   "{}")), values...));
// }

std::string repr(const char &value);
std::string repr(const signed char &value);
std::string repr(const short &value);
std::string repr(const int &value);
std::string repr(const long &value);
std::string repr(const long long &value);
std::string repr(const unsigned char &value);
std::string repr(const unsigned short &value);
std::string repr(const unsigned int &value);
std::string repr(const unsigned long &value);
std::string repr(const unsigned long long &value);
std::string repr(const float &value);
std::string repr(const double &value);
std::string repr(const long double &value);
std::string repr(const bool &value);
std::string repr(const char *const &value);
template <size_t N> std::string repr(const char (&value)[N]);
std::string repr(const std::string &value);
std::string repr(const std::string_view &value);
std::string repr(const std::nullopt_t &);
template <typename T> std::string repr(const std::optional<T> &value);
template <typename E> std::string repr(const std::vector<E> &value);
template <typename E> std::string repr(const std::set<E> &value);
template <typename K, typename V> std::string repr(const std::map<K, V> &value);
template <typename... Ts> std::string repr(const std::tuple<Ts...> &value);

template <typename T> std::string str(const T &value);
std::string str(const char &value);
std::string str(const char *const &value);
std::string str(const std::string &value);
std::string str(const std::string_view &value);

// // todo: move this out to extra_type_traits.h or smth
// template <typename T> struct is_optional : std::false_type {};
//
// template <typename T> struct is_optional<std::optional<T>> : std::true_type
// {};
//
// template <typename T>
// struct is_optional<const std::optional<T>> : std::true_type {};
//
// template <typename T>
// struct is_optional<const std::optional<T> &> : std::true_type {};
//
// template <typename T> constexpr bool is_optional_v = is_optional<T>::value;
//
// // not sure whats wrong with (const std::optional &) but repr(const
// // std::optional &) inside repr(const std::tuple<std::optional<T>> &) would
// not
// // work without this patch...
// template <typename T> std::string patch_repr(const T &value) {
//   if constexpr (is_optional_v<T>) {
//     return value ? repr(*value) : "std::nullopt";
//   }
//   return repr(value);
// }
//
// template <typename... Ts> std::string repr(const std::tuple<Ts...> &value) {
//   const auto repr_ = [&](const Ts &...value) {
//     std::vector<std::string> list;
//     (list.push_back(repr(value)), ...);
//     return format("({})", cpy::join(", ", list));
//   };
//
//   return std::apply(repr_, value);
// }

// todo:
// repr<std::variant<Ts...>>

struct Indent {
  size_t stop = 1;
  size_t size = 2;

  std::string apply(const std::string &s) const;
  std::string operator+(const std::string &s) const { return apply(s); }
  Indent operator+(size_t delta) const { return {stop + delta, size}; }
};

inline std::string indent(const std::string &s, const Indent &indent = {}) {
  return indent + s;
}

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
  std::string apply(const std::string &s) const;
};

std::string bracket(const std::string &s, const Bracket &bracket = {});
std::string bracket(const std::string &s, const Indent &indent);
inline std::string
parenthesize(const std::string &s,
             const Bracket::Style &style = Bracket::Style::Plain);
std::string parenthesize(const std::string &s, const Indent &indent);
std::string brace(const std::string &s,
                  const Bracket::Style &style = Bracket::Style::Plain);
std::string brace(const std::string &s, const Indent &indent);
std::string angle_bracket(const std::string &s,
                          const Bracket::Style &style = Bracket::Style::Plain);
std::string angle_bracket(const std::string &s, const Indent &indent);

} // namespace fmt

inline std::string fmt::repr(const char &value) {
  return "'" + std::string{value} + "'";
}

inline std::string fmt::repr(const signed char &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const short &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const int &value) { return std::to_string(value); }

inline std::string fmt::repr(const long &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const long long &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const unsigned char &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const unsigned short &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const unsigned int &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const unsigned long &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const unsigned long long &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const float &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const double &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const long double &value) {
  return std::to_string(value);
}

inline std::string fmt::repr(const bool &value) {
  return value ? "true" : "false";
}

inline std::string fmt::repr(const char *const &value) {
  return "\"" + std::string(value) + "\"";
}

inline std::string fmt::repr(const std::string &value) {
  return "\"" + value + "\"";
}

template <size_t N> inline std::string fmt::repr(const char (&value)[N]) {
  return fmt::repr(std::string(value));
}

inline std::string fmt::repr(const std::string_view &value) {
  return fmt::repr(std::string(value));
}

inline std::string fmt::repr(const std::nullopt_t &) { return "std::nullopt"; }

template <typename T>
inline std::string fmt::repr(const std::optional<T> &value) {
  return value ? repr(*value) : "std::nullopt";
}

template <typename E>
inline std::string fmt::repr(const std::vector<E> &value) {
  return format(
      "[{}]",
      cpy::join(", ", cpy::map([&](const E &e) { return repr(e); }, value)));
}

template <typename T> inline std::string fmt::repr(const std::set<T> &value) {
  return format(
      "{{{}}}",
      cpy::join(", ", cpy::map([&](const auto &e) { return repr(e); }, value)));
}

template <typename K, typename V>
std::string fmt::repr(const std::map<K, V> &value) {
  return format("{{{}}}", cpy::join(", ", cpy::map(
                                              [&](const auto &e) {
                                                const auto &[k, v] = e;
                                                return format("{} => {}", k, v);
                                              },
                                              cpy::entries(value))));
}

template <typename... Ts>
inline std::string fmt::repr(const std::tuple<Ts...> &value) {
  const auto repr_ = [&](const Ts &...value) {
    std::vector<std::string> list;
    (list.push_back(repr(value)), ...);
    return format("({})", cpy::join(", ", list));
  };

  return std::apply(repr_, value);
}

template <typename T> inline std::string fmt::str(const T &value) {
  return repr(value);
}

inline std::string fmt::str(const char &value) { return std::string{value}; }

inline std::string fmt::str(const char *const &value) { return value; }

inline std::string fmt::str(const std::string &value) { return value; }

inline std::string fmt::str(const std::string_view &value) {
  return std::string(value);
}

inline void fmt::detail::format(std::stringstream &ss, size_t &idx,
                                const std::string &s) {
  const size_t n = s.size();
  while (idx < n) {
    const char c = s[idx++];
    switch (c) {
    case '{': {
      if (idx < n && s[idx++] == '{') {
        ss << '{';
      } else {
        // todo: this could be caused by not enough values supplied
        throw std::invalid_argument(
            fmt::format("invalid format string: \"{}\" (dangling '{{')", s));
      }
      break;
    }
    case '}': {
      if (idx < n && s[idx++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            fmt::format("invalid format string: \"{}\" (dangling '}}')", s));
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
inline void fmt::detail::format(std::stringstream &ss, size_t &idx,
                                const std::string &s, const T &first,
                                const Ts &...rest) {
  const size_t n = s.size();
  bool flag = false;
  while (idx < n) {
    const char c = s[idx++];
    switch (c) {
    case '{': {
      if (idx < n && s[idx] == '{') {
        idx++;
        ss << '{';
      } else {
        flag = !flag;
      }
      break;
    }
    case '}': {
      if (flag) {
        ss << str(first);
        format(ss, idx, s, rest...);
        return;
      } else if (idx < n && s[idx++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            fmt::format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      if (flag) {
        throw std::invalid_argument(fmt::format(
            "invalid format string: \"{}\" (format specifiers not supported)",
            s));
      } else {
        ss << c;
      }
      break;
    }
    }
  }
  throw std::invalid_argument(fmt::format("invalid format string: \"{}\"", s));
}

template <typename... Ts>
inline std::string fmt::format(const std::string &s, const Ts &...values) {
  std::stringstream ss;
  size_t idx = 0;
  detail::format(ss, idx, s, values...);
  return ss.str();
}

inline std::string fmt::Indent::apply(const std::string &s) const {
  const std::string tab(stop * size, ' ');
  std::stringstream ss;
  ss << tab;
  const size_t n = s.size();
  for (const auto &[i, c] : cpy::enumerate(s)) {
    switch (c) {
    case '\n':
      ss << c;
      if (i + 1 < n && s[i + 1] != '\n') {
        ss << tab;
      }
      break;
    default:
      ss << c;
      break;
    }
  }
  return ss.str();
}
inline std::string fmt::Bracket::apply(const std::string &s) const {
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
    return cpy::join("", {open, close});
  }

  switch (style) {
  case Style::Plain:
    return cpy::join("", {open, s, close});

  case Style::Spaced:
    return cpy::join(" ", {open, s, close});

  case Style::Block:
    return cpy::join("\n", {open, indent + s, close});
  }
  return s;
}

inline std::string fmt::bracket(const std::string &s, const Bracket &bracket) {
  return bracket.apply(s);
}

inline std::string fmt::bracket(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Brackets, indent}.apply(s);
}

inline std::string fmt::parenthesize(const std::string &s,
                                     const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Parentheses, style}.apply(s);
}

inline std::string fmt::parenthesize(const std::string &s,
                                     const Indent &indent) {
  return Bracket{Bracket::Kind::Parentheses, indent}.apply(s);
}

inline std::string fmt::brace(const std::string &s,
                              const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Braces, style}.apply(s);
}

inline std::string fmt::brace(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Braces, indent}.apply(s);
}

inline std::string fmt::angle_bracket(const std::string &s,
                                      const Bracket::Style &style) {
  return Bracket{Bracket::Kind::AngleBrackets, style}.apply(s);
}

inline std::string fmt::angle_bracket(const std::string &s,
                                      const Indent &indent) {
  return Bracket{Bracket::Kind::AngleBrackets, indent}.apply(s);
}

#define type_name_of(o) type_name<std::declval(o)>()

#endif
