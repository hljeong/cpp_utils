#include <cassert>
#include <iostream>

#include "fmt.h"

int main() {
  using namespace fmt;

  assert(repr(3) == "3");

  assert(repr(true) == "true");

  assert(repr(false) == "false");

  assert(repr("hello") == "\"hello\"");

  assert(repr(std::string("world")) == "\"world\"");

  assert(repr(std::vector<std::string>{"a", "b", "c"}) ==
         "[\"a\", \"b\", \"c\"]");

  assert(repr(std::make_tuple()) == "()");

  assert(repr(std::make_tuple(2, "hi")) == "(2, \"hi\")");
}
