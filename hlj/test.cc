#include <cassert>

#include "hlj.h"

using namespace hlj;

// todo: has_signature<F, R(As...)>
// todo: use a real test framework

String format_string(const String &s) {
  if (in("\n", s)) {
    return "\n---\n" + s + "\n---\n";
  } else {
    return "\"" + s + "\"";
  }
}

void assert_eq(String value, const String &expected) {
  if (value != expected) {
    printf("expected: %s, got: %s\n", format_string(expected).c_str(),
           format_string(value).c_str());
  }
  assert(value == expected);
}

template <typename T> void test_repr(T value, const String &expected) {
  assert_eq(repr(value), expected);
}

template <size_t N>
void test_repr(const char (&value)[N], const String &expected) {
  assert_eq(repr(value), expected);
}

struct S {
  struct T {
    int x;
  };

  int y;
  T t;
};

String repr(const S::T &value) { return format("{{.x = {}}}", value.x); }

String repr(const S &value) {
  return format("{{.y = {}, .t = {}}}", value.y, value.t);
}

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

  bool operator==(Nil) const { return true; }
} nil;

template <typename T> struct Cons;
template <typename T> using ConsList = OneOf<Cons<T>, Nil>;
template <typename T> struct Cons {
  T car;
  ConsList<T> cdr;

  bool operator==(const Cons<T> &other) const {
    return (car == other.car) && (cdr == other.cdr);
  }
};

template <typename T> ConsList<T> cons(const T &car, const ConsList<T> &cdr) {
  return Cons{car, cdr};
}

template <typename T> Cons(const T &, const ConsList<T> &) -> Cons<T>;

template <typename T> String repr(const ConsList<T> &value) {
  return value.template match<String>({
      [](const Cons<T> &cons) {
        return format("cons({}, {})", repr(cons.car), cons.cdr);
      },
      [](Nil) { return "nil"; },
  });
}

template <typename T> Optional<T> last(const ConsList<T> &list) {
  return list.template match<Optional<T>>({
      [](const Cons<T> &cons) {
        return match<Optional<T>>(cons.car, cons.cdr)({
            {{any, {nil}}, [&cons]() { return cons.car; }},
            {any, [&cons]() { return last(cons.cdr); }},
        });
      },
      [](Nil) { return nullopt; },
  });
}

int main() {
  assert(starts_with("hi", "h"));
  assert(!starts_with("hi", "i"));
  assert(!starts_with("h", "hi"));

  test_repr(3, "3");

  test_repr('x', "'x'");

  test_repr(static_cast<unsigned char>(2), "2");

  test_repr(static_cast<signed char>(-2), "-2");

  test_repr(static_cast<unsigned short>(300), "300");

  test_repr(static_cast<short>(-300), "-300");

  test_repr(true, "true");

  test_repr(false, "false");

  test_repr("hello literal", "\"hello literal\"");

  test_repr(static_cast<const char *>("hello"), "\"hello\"");

  test_repr(String{"world"}, "\"world\"");

  test_repr(List<int>{2, 5, 0}, "[2, 5, 0]");

  test_repr(List<String>{"a", "b", "c"}, "[\"a\", \"b\", \"c\"]");

  test_repr(Tuple<>{}, "()");

  test_repr(Tuple<u32, String>(2, "hi"), "(2, \"hi\")");

  test_repr(nullopt, "nullopt");

  test_repr<Optional<bool>>(nullopt, "nullopt");

  test_repr(Tuple<Optional<u32>>(nullopt), "(nullopt)");

  test_repr(Tuple<List<u32>>({{2}}), "([2])");

  test_repr(
      Tuple<List<i8>, Optional<bool>, String, u32>{
          {-1, -2, 3, 4}, nullopt, "hi", 12},
      "([-1, -2, 3, 4], nullopt, \"hi\", 12)");

  test_repr(StringView("view"), "\"view\"");

  test_repr(S{3, {4}}, "{.y = 3, .t = {.x = 4}}");

  test_repr(Set<int>({1, 2, 3}), "{1, 2, 3}");

  test_repr(Map<int, bool>({{1, true}, {2, false}, {3, true}}),
            "{1 => true, 2 => false, 3 => true}");

  test_repr(
      List<List<char>>({{'a', 'b'}, {'c', 'd', 'e', 'f'}, {}, {'x', 'y', 'z'}}),
      "[['a', 'b'], ['c', 'd', 'e', 'f'], [], ['x', 'y', 'z']]");

  test_repr(List<S>({{2}, {.t = {4}}}),
            "[{.y = 2, .t = {.x = 0}}, {.y = 0, .t = {.x = 4}}]");

  {
    List<S> value{{2}, {.t = {4}}};
    assert_eq(format("{}", value), repr(value));
  }

  assert_eq(indent("hi"), "  hi");

  assert_eq(indent("hi", {2}), "    hi");

  // ...but why?
  assert_eq(indent("hi", {.size = 3}), "   hi");

  assert_eq(indent("lorem\nipsum\n"), "  lorem\n  ipsum\n");

  assert_eq(bracket("1, 2, 3"), "[1, 2, 3]");

  assert_eq(bracket("3, 4, 5", Bracket::Style::Spaced), "[ 3, 4, 5 ]");

  assert_eq(angle_bracket("bra | ket", Bracket::Style::Spaced),
            "< bra | ket >");

  assert_eq(parenthesize("parenthesize(...)"), "(parenthesize(...))");

  assert_eq(brace("printf(\"hello world\");", Bracket::Style::Block),
            "{\n  printf(\"hello world\");\n}");

  assert_eq(brace("printf(\"hello world\");", {.size = 3}),
            "{\n   printf(\"hello world\");\n}");

  assert_eq(brace("", {.size = 3}), "{}");

  assert(join(", ", {"a", "b", "c"}) == "a, b, c");

  assert(combine(Set<int>{1, 2, 3}, Set<int>{3, 4, 5}, Set<int>{5, 6, 7}) ==
         Set<int>({1, 2, 3, 4, 5, 6, 7}));

  assert(combine(List<int>{1, 2, 3}, List<int>{3, 4, 5}, List<int>{5, 6, 7}) ==
         List<int>({1, 2, 3, 3, 4, 5, 5, 6, 7}));

  assert(entries(Map<int, int>{{1, 2}, {3, 4}}) ==
         (List<Entry<int, int>>{{1, 2}, {3, 4}}));

  assert(combine(List<int>{1, 2, 3}, List<int>{3, 4, 5}) ==
         List<int>({1, 2, 3, 3, 4, 5}));

  assert(type_name<int> == "int");
  assert(type_name<String> == "String");
  assert(type_name<List<int>> == "List<int>");

  using Abomination =
      Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>;

  assert(type_name<Abomination> ==
         "Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>");

  assert(oformat(3, "hello", repr('w'), 'd') == "3 hello 'w' d");

  Whatever x = List<int>{1};
  assert(repr(x) == "List<int> [1]");

  x = Abomination{
      17, 'g', {{{{"lorem", {}}, {"ipsum", {}}, {"pachamama", {}}}, nullopt}}};
  // note the orderedness
  assert(repr(x) == oformat(type_name<Abomination>,
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

  auto le = last(cons(1, cons(2, {nil})));
  assert(le.has_value());
  assert(*le == 2);

  assert(!last<int>({nil}));

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

  assert(eager_match<bool>(0)({{0, true}, {{1}, false}, {any, false}}) == true);

  assert(eager_match<bool>(1)({{{0, 1, 2}, true}, {{3, 4, 5}, false}}) == true);

  assert(eager_match<bool>(3)({{{0, 1, 2}, true}, {{3, 4, 5}, false}}) ==
         false);

  assert(eager_match<int>('b', 2)(
             {{{'a', any}, 0}, {{'b', {0, 1}}, 1}, {{any, 2}, 2}}) == 2);

  assert(eager_match<int>(String{"b"}, 2)({{{String{"a"}, any}, 0},
                                           {{String{"b"}, {0, 1}}, 1},
                                           {{any, 2}, 2}}) == 2);

  assert(eager_match<int>(3, 'c')(
             {{{1, 'a'}, 0}, {{1, 'b'}, 1}, {{2, any}, 2}, {{any, 'c'}, 3}}) ==
         3);

  Whatever::PatternMap<String> map = {
      {type<int>, [&]() { return repr(x); }},
      {type<char>, [&]() { return repr(y); }},
      {type<String>, []() { return "bye"; }},
      {any, []() { return "trash"; }},
  };

  x = 6;
  assert(x.match(map) == "int 6");

  x = 'f';
  y = String{"stolen"};
  assert(x.match(map) == "String \"stolen\"");

  x = 2.3;
  assert(x.match(map) == "trash");

  Whatever::EagerPatternMap<int> eager_map = {
      {type<int>, 0},
      {type<char>, 1},
      {type<String>, 2},
  };

  x = 3;
  assert(x.eager_match(eager_map) == 0);

  x = 'c';
  assert(x.eager_match(eager_map) == 1);

  x = String{"hello"};
  assert(x.eager_match(eager_map) == 2);

  // comparable OneOf
  Map<OneOf<int, char>, int> m;
  m[3] = 2;
  m['x'] = 3;
  assert(m[3] == 2);
  assert(m['x'] == 3);
}
