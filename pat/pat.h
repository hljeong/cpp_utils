// "pattern"

#ifndef PAT_H
#define PAT_H

#include <cstddef>
#include <functional>
#include <stdexcept>
#include <tuple>

namespace pat {

static constexpr struct Any {
} any;

template <typename T> class Match {
public:
  // Match with predicate
  Match(std::function<bool(const T &)> match) : m_match(match) {}

  // Match nothing (used as base case for "match any of")
  Match() : Match([](auto) { return false; }) {}

  // Match any of
  template <typename... Ts,
            std::enable_if_t<(std::is_same_v<T, Ts> && ...), bool> = true>
  Match(const T &value, const Ts &...rest)
      : Match([value, rest_match = Match<T>(rest...)](const T &value_) {
          return (value == value_) || rest_match.match(value_);
        }) {}

  // Match anything
  Match(Any) : Match([](auto) { return true; }) {}

  bool match(const T &value) const { return m_match(value); }

private:
  std::function<bool(const T &)> m_match;
};

template <typename... Ts> class Pattern {
public:
  using T0 = std::tuple_element_t<0, std::tuple<Ts...>>;

  Pattern(const Match<Ts> &...pattern)
      : m_pattern(std::make_tuple(pattern...)) {}

  // Shortcut for pattern of 1
  template <
      typename... Ts_,
      std::enable_if_t<(sizeof...(Ts) == 1) && (std::is_same_v<T0, Ts_> && ...),
                       bool> = true>
  Pattern(const Ts_ &...values) : Pattern(Match<T0>(values...)) {}

  // Match anything
  Pattern(Any) : m_is_any(true) {}

  bool match(const Ts &...args) const {
    return match(std::index_sequence_for<Ts...>(), args...);
  }

private:
  std::tuple<Match<Ts>...> m_pattern;

  // todo: ugly
  bool m_is_any = false;

  template <size_t... Is>
  bool match(std::index_sequence<Is...>, const Ts &...args) const {
    return m_is_any || (std::get<Is>(m_pattern).match(args) && ...);
  }
};

template <typename V, typename... Ts> struct PatternValue {
  const Pattern<Ts...> pattern;
  const V value;
};

template <typename V = void, typename... Ts>
std::function<V(const std::vector<PatternValue<V, Ts...>> &)>
match(const Ts &...args, const V &default_value) {
  return [&](const auto &pattern_values) {
    for (const auto &[pattern, value] : pattern_values) {
      if (pattern.match(args...)) {
        return value;
      }
    }
    return default_value;
  };
}

template <typename V = void, typename... Ts>
std::function<V(const std::vector<PatternValue<V, Ts...>> &)>
match(const Ts &...args) {
  return [&](const auto &pattern_values) {
    for (const auto &[pattern, value] : pattern_values) {
      if (pattern.match(args...)) {
        return value;
      }
    }
    throw std::invalid_argument("no match");
  };
}

template <typename V = void, typename... Ts>
auto match_do(const Ts &...args, const V &default_value) {
  return [&](const auto &pattern_actions) {
    return match<std::function<V()>>(args...)(pattern_actions, default_value)();
  };
}

template <typename V = void, typename... Ts> auto match_do(const Ts &...args) {
  return [&](const auto &pattern_actions) {
    return match<std::function<V()>>(args...)(pattern_actions)();
  };
}

} // namespace pat

#endif
