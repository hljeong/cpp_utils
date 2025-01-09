#pragma once

#include <optional>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace fmt {

template <typename T,
          std::enable_if_t<std::is_arithmetic_v<T> && !std::is_same_v<T, bool>,
                           bool> = true>
std::string repr(T value) {
  return std::to_string(value);
}

// workaround for string literals decaying into bool
template <typename T, std::enable_if_t<std::is_same_v<T, bool>, bool> = true>
std::string repr(T value) {
  return value ? "true" : "false";
}

inline std::string repr(const std::string &value) {
  std::stringstream s;
  s << "\"" << value << "\"";
  return s.str();
}

template <std::size_t N> inline std::string repr(const char (&value)[N]) {
  return repr(std::string(value));
}

template <typename T> std::string repr(const std::vector<T> &value) {
  const std::size_t n = value.size();
  std::stringstream s;
  s << "[";
  for (std::size_t i = 0; i < n; ++i) {
    if (i) {
      s << ", ";
    }
    s << repr(value[i]);
  }
  s << "]";
  return s.str();
}

template <typename... Ts, std::size_t... Idx>
std::string repr(const std::tuple<Ts...> &value, std::index_sequence<Idx...>) {
  std::stringstream s;
  s << "(";
  ((s << (Idx ? ", " : "") << repr(std::get<Idx>(value))), ...);
  s << ")";
  return s.str();
}

template <typename... Ts> std::string repr(const std::tuple<Ts...> &value) {
  return repr(value, std::index_sequence_for<Ts...>());
}

inline std::string repr(const std::nullopt_t &) { return "std::nullopt"; }

template <typename T> std::string repr(const std::optional<T> &value) {
  return value ? repr(*value) : "std::nullopt";
}

}; // namespace fmt
