// "format"

#ifndef FMT_H
#define FMT_H

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

inline void prints(const std::string &s) { printf("%s", s.c_str()); }

inline std::string format(const std::string &s) { return s; }

template <typename T, typename... Ts>
inline std::string format(const std::string &s, const T &value,
                          const Ts &...rest) {
  bool flag = false;
  for (const auto &[i, c] : cpy::enumerate(s)) {
    switch (c) {
    case '{': {
      flag = !flag;
      break;
    }
    case '}': {
      if (flag) {
        // todo: this implementation is wrong. escaped braces are not resolved
        auto s_ = s.substr(0, i - 1) + str(value) + s.substr(i + 1);
        return format(s_, rest...);
      } else if (!(i + 1 < s.size() && s[i + 1] == '}')) {
        throw std::invalid_argument(format("invalid format string: \"{}\"", s));
      }
      break;
    }
    default: {
      if (flag) {
        throw std::invalid_argument(format("invalid format string: \"{}\"", s));
      }
      break;
    }
    }
  }
  throw std::invalid_argument(format("invalid format string: \"{}\"", s));
}

template <typename... Ts>
inline void print(const std::string &s, const Ts &...values) {
  prints(format(s, values...));
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

template <std::size_t N> inline std::string repr(const char (&value)[N]) {
  return "\"" + std::string(value) + "\"";
}

template <std::size_t N> inline std::string str(const char (&value)[N]) {
  return value;
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

template <typename T> std::string repr(const std::vector<T> &value) {
  return format("[{}]", cpy::join(", ", cpy::map(repr<T>, value)));
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
// std::optional &) inside repr(const std::tuple<std::optional<T>> &) would not
// work without this patch...
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

} // namespace fmt

#define type_name_of(o) type_name<std::declval(o)>()

#endif
