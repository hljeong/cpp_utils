#include <cassert>
#include <stdexcept>

#include "hlj.h"

using namespace hlj;

// todo: use a real test framework

static constexpr struct Unreprable {
} unreprable;

// todo: search second pass with `is_constructible_v<>`?
template <typename T, typename... Ts> struct index_t;

template <typename T, typename... Ts> struct index_t<T, T, Ts...> {
  static constexpr size_t value = 0;
};

template <typename T, typename U, typename... Ts> struct index_t<T, U, Ts...> {
  static constexpr size_t value = 1 + index_t<T, Ts...>::value;
};

template <typename T, typename... Ts>
static constexpr size_t index = index_t<T, Ts...>::value;

template <typename... Ts> class OneOf {
public:
  template <typename T, size_t I = index<T, Ts...>>
  OneOf(const T &value) : store{value}, which{I} {}

  template <typename T, size_t I = index<T, Ts...>> bool is() const {
    return which == I;
  }

  template <typename T, size_t = index<T, Ts...>> const T &as() const {
    return store.as<T>();
  }

  String repr() const { return store.repr(); }

private:
  Whatever store;
  size_t which;
};

// todo: move OneOf into hlj.h
// todo: un-inline Whatever (and OneOf) impl
// todo: custom struct + repr defined in/outside namespace
// todo: custom struct + repr method defined in/outside namespace

static constexpr struct Nil {
  String repr() const { return "nil"; }
} nil;

template <typename T> struct ConsCell;
template <typename T> struct ConsList : OneOf<ConsCell<T>, Nil> {};
template <typename T> struct ConsCell {
  T value;
  ConsList<T> rest;
};

template <typename T>
ConsList<T> cons(const T &value, const ConsList<T> &rest = {nil}) {
  return ConsList{ConsCell{value, rest}};
}

template <typename T> ConsCell(const T &, const ConsList<T> &) -> ConsCell<T>;

template <typename T> ConsList(const ConsCell<T> &) -> ConsList<T>;

template <typename T> const T &car(const ConsList<T> &list) {
  if (list.template is<Nil>()) {
    throw std::invalid_argument("empty list");
  } else {
    return list.template as<ConsCell<T>>().value;
  }
}

template <typename T> const ConsList<T> &cdr(const ConsList<T> &list) {
  if (list.template is<Nil>()) {
    throw std::invalid_argument("empty list");
  } else {
    return list.template as<ConsCell<T>>().rest;
  }
}

template <typename T> String repr(const ConsList<T> &value) {
  if (value.template is<ConsCell<T>>()) {
    return format("cons({}, {})", repr(car(value)), cdr(value));
  } else {
    return "nil";
  }
}

int main() {
  assert(starts_with("hi", "h"));
  assert(!starts_with("hi", "i"));
  assert(!starts_with("h", "hi"));

  assert(type_name<int> == "int");
  assert(type_name<String> == "String");
  assert(type_name<List<int>> == "List<int>");

  using Abomination =
      Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>;

  assert(type_name<Abomination> ==
         "Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>");

  assert(formato(3, "hello", repr('w'), 'd') == "3 hello 'w' d");

  Whatever x = Abomination{
      17, 'g', {{{{"lorem", {}}, {"ipsum", {}}, {"pachamama", {}}}, nullopt}}};
  // note the orderedness
  assert(repr(x) == format("{} {}", type_name<Abomination>,
                           "(17, 'g', [({\"ipsum\" => (), \"lorem\" => (), "
                           "\"pachamama\" => ()}, nullopt)])"));

  x = 3;
  assert(repr(x) == "int 3");

  Whatever y = std::move(x);
  assert(repr(x) == "nothing");
  assert(repr(y) == "int 3");

  x = y;
  assert(repr(x) == "int 3");
  assert(repr(y) == "int 3");

  y = String("hi");
  assert(repr(y) == "String \"hi\"");

  assert(always_repr(unreprable) == "<unreprable Unreprable>");
  assert(repr(Whatever{unreprable}) == "Unreprable <unreprable Unreprable>");

  assert(repr(cons(1, cons(2, {nil}))) == "cons(1, cons(2, nil))");

  assert(repr(cons(String{"hello"}, cons(String{"world"}, {nil}))) ==
         "cons(\"hello\", cons(\"world\", nil))");
}
