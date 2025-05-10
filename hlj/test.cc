#include <cassert>
#include <stdexcept>

#include "hlj.h"

using namespace hlj;

// todo: Whatever::match() and OneOf::match()
// todo: has_signature<F, R(As...)>
// todo: use a real test framework

static constexpr struct Unreprable {
} unreprable;

struct S1 {
  int x;
};

String repr(S1 value) { return format("S1(x={})", value.x); };

namespace ns1 {

struct S2 {
  S1 s;
  char c;
};

String repr(S2 value) {
  return format("S2(s={}, c={})", repr(value.s), hlj::repr(value.c));
};

} // namespace ns1

namespace ns2 {

struct S3 {
  S1 s1;
  ns1::S2 s2;
  float f;
};

String repr(S3 value) {
  return format("S3(s1={}, s2={}, f={})", value.s1, value.s2, value.f);
}

} // namespace ns2

struct S4 {
  int y;

  String repr() const { return format("S4(y={})", y); }
};

namespace ns3 {

struct S5 {
  S4 s;
  char h;

  String repr() const {
    // using namespace hlj; // this would not resolve the repr()'s
    using hlj::repr;
    return format("S5(s={}, h={})", repr(s), repr(h));
  }
};

} // namespace ns3

static constexpr struct Nil {
  String repr() const { return "nil"; }
} nil;

template <typename T> struct Cons;
template <typename T> using ConsList = OneOf<Cons<T>, Nil>;
template <typename T> struct Cons {
  T value;
  ConsList<T> rest;
};

template <typename T>
ConsList<T> cons(const T &value, const ConsList<T> &rest) {
  return Cons{value, rest};
}

template <typename T> Cons(const T &, const ConsList<T> &) -> Cons<T>;

template <typename T> const T &car(const ConsList<T> &list) {
  if (list.template is<Nil>()) {
    throw std::invalid_argument("empty list");
  } else {
    return list.template as<Cons<T>>().value;
  }
}

template <typename T> const ConsList<T> &cdr(const ConsList<T> &list) {
  if (list.template is<Nil>()) {
    throw std::invalid_argument("empty list");
  } else {
    return list.template as<Cons<T>>().rest;
  }
}

template <typename T> String repr(const ConsList<T> &value) {
  if (value.template is<Cons<T>>()) {
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

  Whatever x = List<int>{1};
  assert(repr(x) == "List<int> [1]");

  x = Abomination{
      17, 'g', {{{{"lorem", {}}, {"ipsum", {}}, {"pachamama", {}}}, nullopt}}};
  // note the orderedness
  assert(repr(x) == formato(type_name<Abomination>,
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

  assert(always_repr(unreprable) == "<Unreprable>");
  assert(repr(Whatever{unreprable}) == "Unreprable <Unreprable>");

  assert(repr(cons(1, cons(2, {nil}))) == "cons(1, cons(2, nil))");

  assert(repr(cons(String{"hello"}, cons(String{"world"}, {nil}))) ==
         "cons(\"hello\", cons(\"world\", nil))");

  OneOf<int, char> p = 3;
  auto q = p;
  assert(repr(q) == "int 3");

  auto r = std::move(p);
  assert(repr(q) == "int 3");
  assert(repr(r) == "int 3");
  assert(repr(p) == "nothing");
  assert((!p.is<int>()) && (!p.is<char>()));

  assert(repr(ns2::S3{.s1 = S1{1}, .s2 = ns1::S2{S1{2}, '3'}, .f = 3.14f}) ==
         "S3(s1=S1(x=1), s2=S2(s=S1(x=2), c='3'), f=3.140000)");

  assert(repr(ns3::S5{S4{4}, 'x'}) == "S5(s=S4(y=4), h='x')");

  Whatever::Matcher<int> matcher = {
      {type<int>, 0},
      {type<char>, 1},
      {type<String>, 2},
  };

  x = 3;
  assert(x.match(matcher) == 0);

  x = 'c';
  assert(x.match(matcher) == 1);

  x = String{"hello"};
  assert(x.match(matcher) == 2);

  Whatever::LazyMatcher<String> lazy_matcher = {
      {type<int>, [&]() { return repr(x); }},
      {type<char>, [&]() { return repr(y); }},
      {type<String>, []() { return "bye"; }},
      {any, []() { return "trash"; }},
  };

  assert(x.lazy_match(lazy_matcher) == "bye");

  x = 6;
  assert(x.lazy_match(lazy_matcher) == "int 6");

  x = 'f';
  y = String{"stolen"};
  assert(x.lazy_match(lazy_matcher) == "String \"stolen\"");

  x = 2.3;
  assert(x.lazy_match(lazy_matcher) == "trash");
}
