#include <cassert>

#include "fmt.h"

using namespace cpy;
using namespace fmt;

std::string format_string(const std::string &s) {
  if (in("\n", s)) {
    return "\n---\n" + s + "\n---\n";
  } else {
    return "\"" + s + "\"";
  }
}

void assert_eq(std::string value, const std::string &expected) {
  if (value != expected) {
    printf("expected: %s, got: %s\n", format_string(expected).c_str(),
           format_string(value).c_str());
  }
  assert(value == expected);
}

template <typename T> void test_repr(T value, const std::string &expected) {
  assert_eq(repr(value), expected);
}

template <size_t N>
void test_repr(const char (&value)[N], const std::string &expected) {
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

int main() {
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

  test_repr(std::string("world"), "\"world\"");

  test_repr(std::vector<int>{2, 5, 0}, "[2, 5, 0]");

  test_repr(std::vector<std::string>{"a", "b", "c"}, "[\"a\", \"b\", \"c\"]");

  test_repr(std::tuple<>(), "()");

  test_repr(std::tuple<uint32_t, std::string>(2, "hi"), "(2, \"hi\")");

  test_repr(std::nullopt, "std::nullopt");

  test_repr<std::optional<bool>>(std::nullopt, "std::nullopt");

  test_repr(std::tuple<std::optional<uint32_t>>(std::nullopt),
            "(std::nullopt)");

  test_repr(std::tuple<std::vector<uint32_t>>({{2}}), "([2])");

  test_repr(std::tuple<std::vector<int8_t>, std::optional<bool>, std::string,
                       uint32_t>{{-1, -2, 3, 4}, std::nullopt, "hi", 12},
            "([-1, -2, 3, 4], std::nullopt, \"hi\", 12)");

  test_repr(std::string_view("view"), "\"view\"");

  test_repr(S{3, {4}}, "{.y = 3, .t = {.x = 4}}");

  test_repr(std::set<int>({1, 2, 3}), "{1, 2, 3}");

  test_repr(std::map<int, bool>({{1, true}, {2, false}, {3, true}}),
            "{1 => true, 2 => false, 3 => true}");

  test_repr(std::vector<std::vector<char>>(
                {{'a', 'b'}, {'c', 'd', 'e', 'f'}, {}, {'x', 'y', 'z'}}),
            "[['a', 'b'], ['c', 'd', 'e', 'f'], [], ['x', 'y', 'z']]");

  test_repr(std::vector<S>({{2}, {.t = {4}}}),
            "[{.y = 2, .t = {.x = 0}}, {.y = 0, .t = {.x = 4}}]");

  {
    std::vector<S> value{{2}, {.t = {4}}};
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
}
