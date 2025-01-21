#ifndef RES_H
#define RES_H

#include <algorithm>
#include <variant>

namespace res {

template <typename T> class Wraps {
public:
  explicit constexpr Wraps(T value) : value(std::move(value)) {};

  constexpr T &&move() { return static_cast<T &&>(value); }

  T value;
};

template <typename T> class Ok : public Wraps<T> {
public:
  explicit constexpr Ok(T value) : Wraps<T>(std::move(value)) {}
};

template <typename T> class Err : public Wraps<T> {
public:
  explicit constexpr Err(T value) : Wraps<T>(std::move(value)) {}
};

template <typename T, typename E> class Result {
public:
  using Variant = std::variant<Ok<T>, Err<E>>;

  constexpr Result(Ok<T> value) : m_value(std::move(value)) {}

  constexpr Result(Err<E> value) : m_value(std::move(value)) {}

  constexpr bool is_ok() const {
    return std::holds_alternative<Ok<T>>(m_value);
  }

  constexpr bool is_err() const {
    return std::holds_alternative<Err<E>>(m_value);
  }

  constexpr T ok() const { return std::get<Ok<T>>(m_value).value; }

  constexpr E err() const { return std::get<Err<E>>(m_value).value; }

  constexpr T &&move_ok() { return std::get<Ok<T>>(m_value).move(); }

  constexpr E &&move_err() { return std::get<Err<E>>(m_value).move(); }

  // for potentially better syntax, see: https://github.com/BowenFu/matchit.cpp
  template <typename R>
  R match(std::function<R(T)> f_ok, std::function<R(E)> f_err) {
    if (is_ok()) {
      return f_ok(move_ok());
    } else {
      return f_err(move_err());
    }
  }

  void match_do(std::function<void(T)> f_ok, std::function<void(E)> f_err) {
    match<void>(f_ok, f_err);
  }

private:
  Variant m_value;
};

}; // namespace res

#endif
