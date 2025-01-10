#include <cassert>

#include "fmt.h"

using namespace fmt;

void assert_eq(std::string value, const char *expected) {
  if (value != expected) {
    printf("expected: %s, got: %s\n", expected, value.c_str());
  }
  assert(value == expected);
}

template <typename T> void test(T value, const char *expected) {
  assert_eq(repr(value), expected);
}

template <std::size_t N>
void test(const char (&value)[N], const char *expected) {
  assert_eq(repr(value), expected);
}

int main() {
  test(3, "3");

  test('x', "'x'");

  test(static_cast<unsigned char>(2), "2");

  test(static_cast<signed char>(-2), "-2");

  test(static_cast<unsigned short>(300), "300");

  test(static_cast<short>(-300), "-300");

  test(true, "true");

  test(false, "false");

  test("hello literal", "\"hello literal\"");

  test(static_cast<const char *>("hello"), "\"hello\"");

  test(std::string("world"), "\"world\"");

  test(std::vector<std::string>{"a", "b", "c"}, "[\"a\", \"b\", \"c\"]");

  test(std::tuple<>(), "()");

  test(std::tuple<uint32_t, std::string>(2, "hi"), "(2, \"hi\")");

  test(std::nullopt, "std::nullopt");

  test<std::optional<bool>>(std::nullopt, "std::nullopt");

  test(std::tuple<std::optional<uint32_t>>(std::nullopt), "(std::nullopt)");

  test(std::tuple<std::vector<uint32_t>>({{2}}), "([2])");

  test(std::tuple<std::vector<int8_t>, std::optional<bool>, std::string,
                  uint32_t>{{-1, -2, 3, 4}, std::nullopt, "hi", 12},
       "([-1, -2, 3, 4], std::nullopt, \"hi\", 12)");
}
