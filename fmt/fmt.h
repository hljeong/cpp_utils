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
template <typename T> inline std::string repr(const T &);

template <typename T> inline std::string str(const T &value);

inline std::string repr(const char (&value)[]) {
  return "\"" + std::string(value) + "\"";
}

inline std::string str(const char (&value)[]) { return value; }

template <std::size_t N> inline std::string repr(const char (&value)[N]) {
  return "\"" + std::string(value) + "\"";
}

template <std::size_t N> inline std::string str(const char (&value)[N]) {
  return value;
}

template <typename... Ts>
std::string format(const std::string &s, const Ts &...values);

// todo: these repr() overloads need to be defined before the definitions of
// format() below. figure out why (the compiler instantly resolves the function
// call with only the known declarations?)

template <typename T> std::string repr(const std::vector<T> &value) {
  // need to explicitly spell out lambda instead of using repr<T>
  // since overloads such as repr(const std::vector<?> &) are not
  // template specializations
  return format(
      "[{}]",
      cpy::join(", ", cpy::map([&](const auto &e) { return repr(e); }, value)));
}

template <typename T> std::string repr(const std::set<T> &value) {
  return format(
      "{{{}}}",
      cpy::join(", ", cpy::map([&](const auto &e) { return repr(e); }, value)));
}

template <typename K, typename V>
std::string repr(const std::map<K, V> &value) {
  return format("{{{}}}", cpy::join(", ", cpy::map(
                                              [&](const auto &e) {
                                                const auto &[k, v] = e;
                                                return format("{} => {}", k, v);
                                              },
                                              cpy::entries(value))));
}

inline void prints(const std::string &s) { printf("%s\n", s.c_str()); }

inline void format(std::stringstream &ss, size_t &idx, const std::string &s) {
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
            format("invalid format string: \"{}\" (dangling '{{')", s));
      }
      break;
    }
    case '}': {
      if (idx < n && s[idx++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            format("invalid format string: \"{}\" (dangling '}}')", s));
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
inline void format(std::stringstream &ss, size_t &idx, const std::string &s,
                   const T &first, const Ts &...rest) {
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
            format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      if (flag) {
        throw std::invalid_argument(format(
            "invalid format string: \"{}\" (format specifiers not supported)",
            s));
      } else {
        ss << c;
      }
      break;
    }
    }
  }
  throw std::invalid_argument(format("invalid format string: \"{}\"", s));
}

template <typename... Ts>
inline std::string format(const std::string &s, const Ts &...values) {
  std::stringstream ss;
  size_t idx = 0;
  format(ss, idx, s, values...);
  return ss.str();
}

inline void print() { prints(""); }

template <typename... Ts>
inline void print(const std::string &s, const Ts &...values) {
  prints(format(s, values...));
}

template <typename... Ts> inline void printo(const Ts &...values) {
  prints(format(cpy::join(" ", cpy::repeat(sizeof...(Ts), "{}")), values...));
}

template <typename T> inline std::string repr(const T &) {
  return format("<unimplemented: repr({})>", type_name<T>());
}

template <typename T> inline std::string str(const T &value) {
  return repr(value);
}

template <> inline std::string repr<char>(const char &value) {
  return "'" + std::string{value} + "'";
}

template <> inline std::string str<char>(const char &value) {
  return std::string{value};
}

template <> inline std::string repr<signed char>(const signed char &value) {
  return std::to_string(value);
}

template <> inline std::string repr<short>(const short &value) {
  return std::to_string(value);
}

template <> inline std::string repr<int>(const int &value) {
  return std::to_string(value);
}

template <> inline std::string repr<long>(const long &value) {
  return std::to_string(value);
}

template <> inline std::string repr<long long>(const long long &value) {
  return std::to_string(value);
}

template <> inline std::string repr<unsigned char>(const unsigned char &value) {
  return std::to_string(value);
}

template <>
inline std::string repr<unsigned short>(const unsigned short &value) {
  return std::to_string(value);
}

template <> inline std::string repr<unsigned int>(const unsigned int &value) {
  return std::to_string(value);
}

template <> inline std::string repr<unsigned long>(const unsigned long &value) {
  return std::to_string(value);
}

template <>
inline std::string repr<unsigned long long>(const unsigned long long &value) {
  return std::to_string(value);
}

template <> inline std::string repr<float>(const float &value) {
  return std::to_string(value);
}

template <> inline std::string repr<double>(const double &value) {
  return std::to_string(value);
}

template <> inline std::string repr<long double>(const long double &value) {
  return std::to_string(value);
}

template <> inline std::string repr<bool>(const bool &value) {
  return value ? "true" : "false";
}

template <> inline std::string repr<const char *>(const char *const &value) {
  return "\"" + std::string(value) + "\"";
}

template <> inline std::string str<const char *>(const char *const &value) {
  return value;
}

template <> inline std::string repr<std::string>(const std::string &value) {
  return "\"" + value + "\"";
}

template <> inline std::string str<std::string>(const std::string &value) {
  return value;
}

template <>
inline std::string repr<std::string_view>(const std::string_view &value) {
  return repr(std::string(value));
}

template <>
inline std::string str<std::string_view>(const std::string_view &value) {
  return std::string(value);
}

// todo: move this out to extra_type_traits.h or smth
template <typename T> struct is_optional : std::false_type {};

template <typename T> struct is_optional<std::optional<T>> : std::true_type {};

template <typename T>
struct is_optional<const std::optional<T>> : std::true_type {};

template <typename T>
struct is_optional<const std::optional<T> &> : std::true_type {};

template <typename T> constexpr bool is_optional_v = is_optional<T>::value;

// not sure whats wrong with (const std::optional &) but repr(const
// std::optional &) inside repr(const std::tuple<std::optional<T>> &) would
// not work without this patch...
template <typename T> std::string patch_repr(const T &value) {
  if constexpr (is_optional_v<T>) {
    return value ? repr(*value) : "std::nullopt";
  }
  return repr(value);
}

template <typename... Ts> std::string repr(const std::tuple<Ts...> &value) {
  const auto repr_ = [](const Ts &...value) {
    std::vector<std::string> list;
    (list.push_back(patch_repr(value)), ...);
    return format("({})", cpy::join(", ", list));
  };

  return std::apply(repr_, value);
}

template <> inline std::string repr<std::nullopt_t>(const std::nullopt_t &) {
  return "std::nullopt";
}

template <typename T> std::string repr(const std::optional<T> &value) {
  return value ? repr(*value) : "std::nullopt";
}

// todo: repr<std::variant<Ts...>>

struct Indent {
  size_t stop = 1;
  size_t size = 2;

  std::string apply(const std::string &s) const {
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

  std::string operator+(const std::string &s) const { return apply(s); }

  Indent operator+(int delta) const {
    return {static_cast<size_t>(std::max(0, static_cast<int>(stop) + delta)),
            size};
  }
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

  std::string apply(const std::string &s) const {
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
};

inline std::string bracket(const std::string &s, const Bracket &bracket = {}) {
  return bracket.apply(s);
}

inline std::string bracket(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Brackets, indent}.apply(s);
}

inline std::string
parenthesize(const std::string &s,
             const Bracket::Style &style = Bracket::Style::Plain) {
  return Bracket{Bracket::Kind::Parentheses, style}.apply(s);
}

inline std::string parenthesize(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Parentheses, indent}.apply(s);
}

inline std::string brace(const std::string &s,
                         const Bracket::Style &style = Bracket::Style::Plain) {
  return Bracket{Bracket::Kind::Braces, style}.apply(s);
}

inline std::string brace(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Braces, indent}.apply(s);
}

inline std::string
angle_bracket(const std::string &s,
              const Bracket::Style &style = Bracket::Style::Plain) {
  return Bracket{Bracket::Kind::AngleBrackets, style}.apply(s);
}

inline std::string angle_bracket(const std::string &s, const Indent &indent) {
  return Bracket{Bracket::Kind::AngleBrackets, indent}.apply(s);
}

} // namespace fmt

#define type_name_of(o) type_name<std::declval(o)>()

#endif
