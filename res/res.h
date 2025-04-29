// "result"

#ifndef RES_H
#define RES_H

#include <algorithm>
#include <variant>

#include "../sum/sum.h"

namespace res {

template <typename T> class Wraps {
public:
  constexpr Wraps(T value) : value(std::move(value)) {};

  constexpr Wraps(Wraps<T> &&) = default;

  constexpr Wraps &operator=(Wraps<T> &&) = default;

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

template <typename T, typename E>
class Result : public sum::OneOf<Ok<T>, Err<E>> {
public:
  using Base = sum::OneOf<Ok<T>, Err<E>>;

  constexpr Result(Ok<T> value) : Base(std::move(value)) {}

  constexpr Result(Err<E> value) : Base(std::move(value)) {}

  constexpr bool is_ok() const { return Base::template is<Ok<T>>(); }

  constexpr bool is_err() const { return Base::template is<Err<E>>(); }

  constexpr T ok() const { return Base::template get<Ok<T>>(); }

  constexpr E err() const { return Base::template get<Err<E>>(); }

  // todo: figure out why get<Ok<T>>().move() doesnt work
  constexpr T &&move_ok() { return Base::template move<Ok<T>>().move(); }

  constexpr E &&move_err() { return Base::template move<Err<E>>().move(); }

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
};

} // namespace res

#endif
