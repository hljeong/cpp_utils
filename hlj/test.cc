#include <cassert>

#include "hlj.h"

using namespace hlj;

int main() {
  assert(starts_with("hi", "h"));
  assert(!starts_with("hi", "i"));
  assert(!starts_with("h", "hi"));

  assert(type_name<int> == "int");
  assert(type_name<String> == "String");
  assert(type_name<List<int>> == "List<int>");

  auto a = type_name<
      Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>>;
  assert(a ==
         "Tuple<int, char, List<Pair<Map<String, Tuple<>>, Optional<bool>>>>");
}
