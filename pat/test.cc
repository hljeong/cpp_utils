#include <cassert>

#include "pat.h"

using namespace pat;

int main() {
  assert(match<bool>(1)({{{0, 1, 2}, true}, {{3, 4, 5}, false}}) == true);
  assert(match<bool>(3)({{{0, 1, 2}, true}, {{3, 4, 5}, false}}) == false);
  assert(match<int>('b', 2)(
             {{{'a', any}, 0}, {{'b', {0, 1}}, 1}, {{any, 2}, 2}}) == 2);

  return 0;
}
