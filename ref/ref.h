// "static reflection"

#ifndef REF_H
#define REF_H

#include <string>

namespace ref {

template <typename T> struct Reflect {
  static const std::string repr(const T &value);
};

template <typename T> static const std::string repr(const T &value) {
  return Reflect<T>::repr(value);
}

} // namespace ref

#endif
