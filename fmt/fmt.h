#pragma once

#include <cxxabi.h>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace fmt {

// // this is cheating, also floods compile errors
// template <typename T> std::string repr(const T &value) {
//   std::stringstream s;
//
//   s << value;
//
//   return s.str();
// }

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
    r += "&";
  } else if (std::is_rvalue_reference_v<T>) {
    r += "&&";
  }

  return r;
}

// todo: move this out to extra_type_traits.h or smth
template <typename T>
struct remove_cvref : std::remove_cv<std::remove_reference_t<T>> {};

template <typename T> using remove_cvref_t = typename remove_cvref<T>::type;

template <typename T> std::string repr(const T &) {
  return "<could not repr: " + type_name<T>() + ">";
}

template <> inline std::string repr<char>(const char &value) {
  return "'" + std::string{value} + "'";
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

template <> inline std::string repr<const char *>(const char *const &value) {
  return "\"" + std::string(value) + "\"";
}

template <> inline std::string repr<std::string>(const std::string &value) {
  return "\"" + value + "\"";
}

template <typename T> std::string repr(const std::vector<T> &value) {
  std::stringstream s;

  s << "[";
  // todo: implement enumerate()?
  for (std::size_t i = 0; i < value.size(); ++i) {
    if (i) {
      s << ", ";
    }
    s << repr(value[i]);
  }
  s << "]";

  return s.str();
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
    std::stringstream s;

    s << "(";

    // std::size_t idx_ = 0;
    // (printf("decltype(value[%lu]) = %s\n", idx_++,
    //         type_name<decltype(value)>().c_str()),
    //  ...);

    std::size_t idx = 0;
    ((s << (idx++ ? ", " : ""), s << patch_repr(value)), ...);

    s << ")";
    return s.str();
  };

  return std::apply(repr_, value);
}

template <> inline std::string repr<std::nullopt_t>(const std::nullopt_t &) {
  return "std::nullopt";
}

template <typename T> std::string repr(const std::optional<T> &value) {
  // printf("calling repr<std::optional<%s>>()\n", type_name<T>().c_str());
  return value ? repr(*value) : "std::nullopt";
}

}; // namespace fmt
