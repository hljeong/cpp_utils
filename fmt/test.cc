#include <cassert>

#include "fmt.h"

using namespace fmt;

int main() {
  assert(repr(3) == "3");

  assert(repr(true) == "true");

  assert(repr(false) == "false");

  assert(repr("hello") == "\"hello\"");

  assert(repr(std::string("world")) == "\"world\"");

  assert(repr(std::vector<std::string>{"a", "b", "c"}) ==
         "[\"a\", \"b\", \"c\"]");

  assert(repr(std::make_tuple()) == "()");

  assert(repr(std::make_tuple(2, "hi")) == "(2, \"hi\")");

  assert(repr(std::nullopt) == "std::nullopt");

  assert(repr(std::optional<int8_t>(-2)) == "-2");
}
