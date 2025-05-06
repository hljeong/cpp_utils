#include <cassert>

#include "hlj.h"

using namespace hlj;

// todo: delete
#include "../fmt/fmt.h"

int main() {
  assert(starts_with("hi", "h"));
  assert(!starts_with("hi", "i"));
  assert(!starts_with("h", "hi"));

  assert(type_name<int> == "int");
  assert(starts_with(type_name<std::vector<int>>, "std::vector<int"));
}
