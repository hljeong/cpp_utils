#include <cassert>

#include "cpy.h"

using namespace cpy;

int main() {
  assert(join(", ", {"a", "b", "c"}) == "a, b, c");

  assert(combine(Set<int>{1, 2, 3}, Set<int>{3, 4, 5}, Set<int>{5, 6, 7}) ==
         Set<int>({1, 2, 3, 4, 5, 6, 7}));

  assert(combine(List<int>{1, 2, 3}, List<int>{3, 4, 5}, List<int>{5, 6, 7}) ==
         List<int>({1, 2, 3, 3, 4, 5, 5, 6, 7}));

  assert(entries(Map<int, int>{{1, 2}, {3, 4}}) ==
         (List<Entry<int, int>>{{1, 2}, {3, 4}}));

  return 0;
}
