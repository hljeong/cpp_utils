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
  Match(const T &match, const Ts &...rest)
      : Match([match, rest_match = Match<T>(rest...)](const T &value) {
          return (value == match) || rest_match.match(value);
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
  Pattern(const Ts_ &...pattern) : Pattern(Match<T0>(pattern...)) {}

  // Match anything
  Pattern(Any) : m_is_any(true) {}

  bool match(const Ts &...values) const {
    return match(std::index_sequence_for<Ts...>(), values...);
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

template <typename R, typename... Ts> struct PatternResult {
  const Pattern<Ts...> pattern;
  const R result;
};

template <typename R = void, typename... Ts>
std::function<R(const std::vector<PatternResult<R, Ts...>> &)>
match(const Ts &...values, const R &default_result) {
  return [&](const auto &pattern_results) {
    for (const auto &[pattern, result] : pattern_results) {
      if (pattern.match(values...)) {
        return result;
      }
    }
    return default_result;
  };
}

template <typename R = void, typename... Ts>
std::function<R(const std::vector<PatternResult<R, Ts...>> &)>
match(const Ts &...values) {
  return [&](const auto &pattern_results) {
    for (const auto &[pattern, result] : pattern_results) {
      if (pattern.match(values...)) {
        return result;
      }
    }
    throw std::invalid_argument("no match");
  };
}

template <typename R = void, typename... Ts>
auto match_do(const Ts &...values, const R &default_result) {
  return [&](const auto &pattern_actions) {
    return match<std::function<R()>>(values...)(pattern_actions,
                                                default_result)();
  };
}

template <typename R = void, typename... Ts>
auto match_do(const Ts &...values) {
  return [&](const auto &pattern_actions) {
    return match<std::function<R()>>(values...)(pattern_actions)();
  };
}

} // namespace pat

#endif
