#include <cassert>

#include "cpy.h"

using namespace cpy;

int main() {
  assert(join(", ", {"a", "b", "c"}) == "a, b, c");

  return 0;
}
