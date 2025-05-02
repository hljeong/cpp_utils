#include "../fmt/fmt.h"
#include "ref.h"

struct S {
  int a;
  bool b;
};

template <> const std::string ref::Reflect<S>::repr(const S &value) {
  std::stringstream s;
  s << "{" << fmt::repr(value.a) << ", " << fmt::repr(value.b) << "}";
  return s.str();
}

int main() {
  // S s = {3, true};
  // printf("%s\n", ref::repr(s).c_str());
  // printf("{%s, %s}\n", fmt::repr(s.a).c_str(), fmt::repr(s.b).c_str());
  // Reflect<S>::set_a(s, 5);
  // printf("{%s, %s}\n", repr(s.a).c_str(), repr(s.b).c_str());
}
