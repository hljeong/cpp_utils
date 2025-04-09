#ifndef SUM_H
#define SUM_H

#include <variant>

namespace sum {

template <typename... Ts> class OneOf {
public:
  using Variant = std::variant<Ts...>;

  template <typename T>
  constexpr OneOf(T &&value) : m_value(std::forward<T>(value)){};

  // see: https://en.cppreference.com/w/cpp/utility/variant/visit
  template <typename Visitor> auto visit(Visitor &&visitor) const {
    return std::visit(std::forward<Visitor>(visitor), m_value);
  }

  template <typename Visitor> auto visit(Visitor &&visitor) {
    return std::visit(std::forward<Visitor>(visitor), m_value);
  }

  template <typename T> constexpr T get() const { return std::get<T>(m_value); }

  template <typename T> constexpr T &&move() {
    return std::move(std::get<T>(m_value));
  }

  template <typename T> constexpr bool is() const {
    return std::holds_alternative<T>(m_value);
  }

private:
  Variant m_value;
};

} // namespace sum

#endif
