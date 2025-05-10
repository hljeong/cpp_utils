#pragma once

#include <atomic>
#include <chrono>
#include <cxxabi.h>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <ratio>
#include <set>
#include <sstream>
#include <string>
#include <thread>
#include <typeinfo>

namespace hlj {

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using byte = u8;
using uint = u32;
using Size = size_t;
using Index = size_t;
using String = std::string;
using StringView = std::string_view;
using SStream = std::stringstream;
using True = std::true_type;
using False = std::false_type;
template <typename T, typename U> using Same = std::is_same<T, U>;
template <typename T> using Void = std::void_t<T>;
using std::declval;
template <typename E> using List = std::vector<E>;
template <typename E> using Queue = std::queue<E>;
template <typename E> using Set = std::set<E>;
template <typename F, typename S> using Pair = std::pair<F, S>;
using std::make_pair;
template <typename K, typename V> using Map = std::map<K, V>;
template <typename... Es> using Tuple = std::tuple<Es...>;
template <typename T> using Optional = std::optional<T>;
using NullOpt = std::nullopt_t;
using std::nullopt;
template <typename F> using Function = std::function<F>;
template <typename T> using Decay = std::decay<T>;
template <typename T> using decay = typename Decay<T>::type;
template <Index... Is> using Seq = std::index_sequence<Is...>;
template <typename... Ts>
static constexpr auto SeqFor = std::index_sequence_for<Ts...>();
template <bool C> using EnableIf = std::enable_if_t<C, bool>;
template <typename T> using UPtr = std::unique_ptr<T>;
using std::make_unique;
using TypeId = std::type_info;
using namespace std::chrono_literals;
using Nanoseconds = std::chrono::nanoseconds;
using Microseconds = std::chrono::microseconds;
using Milliseconds = std::chrono::milliseconds;
using Seconds = std::chrono::seconds;
using Minutes = std::chrono::minutes;
using Hours = std::chrono::hours;
using std::chrono::duration_cast;
using TimePoint = std::chrono::time_point<std::chrono::steady_clock>;
inline TimePoint now() { return std::chrono::steady_clock::now(); }
using std::this_thread::sleep_for;
using Thread = std::thread;

struct TypeInfo {
  virtual const TypeId &get_type_id() const = 0;
  virtual const String &get_name() const = 0;
  virtual String repr() const = 0;

  bool operator==(const TypeInfo &other) const {
    return get_type_id() == other.get_type_id();
  }
};

template <typename T> struct TypeName {
  static const String name;
};

template <typename T> struct Type : TypeInfo {
  using type = T;
  static constexpr const TypeId &type_id{typeid(T)};
  inline static const String name = TypeName<T>::name;

  const TypeId &get_type_id() const final { return type_id; }
  const String &get_name() const final { return name; }
  String repr() const final;
};
template <> inline const String TypeName<String>::name = "String";

template <typename T> static const Type<T> type = Type<T>{};
template <typename T> static const TypeInfo &type_info = type<T>;
template <typename T> static const TypeId &type_id = Type<T>::type_id;
template <typename T> static const String type_name = Type<T>::name;

// todo: search second pass with `is_constructible_v<>`?
inline namespace detail {
template <typename T, typename... Ts> struct IsIn : False {};
template <typename T, typename... Ts> struct IsIn<T, T, Ts...> : True {};
template <typename T, typename U, typename... Ts>
struct IsIn<T, U, Ts...> : IsIn<T, Ts...> {};
} // namespace detail
template <typename T, typename... Ts>
static constexpr bool is_in = IsIn<T, Ts...>::value;

inline namespace detail {
template <Index I, typename... Ts> struct Nth;
template <typename T, typename... Ts> struct Nth<0, T, Ts...> : Type<T> {};
template <Index I, typename T, typename... Ts>
struct Nth<I, T, Ts...> : Nth<I - 1, Ts...> {};
} // namespace detail
template <Index I, typename... Ts> using nth = typename Nth<I, Ts...>::type;

struct NotFound {
  static NotFound value;
};

struct Undefined {
  static Undefined value;
};

// todo: search second pass with `is_constructible_v<>`?
template <typename T, typename... Ts> struct IndexOf : NotFound {};

template <typename T, typename... Ts> struct IndexOf<T, T, Ts...> {
  static constexpr Index value = 0;
};

template <typename T, typename U, typename... Ts> struct IndexOf<T, U, Ts...> {
  static constexpr Index value = 1 + IndexOf<T, Ts...>::value;
};

template <typename T, typename... Ts>
static constexpr Index index = IndexOf<T, Ts...>::value;

inline namespace detail {
template <typename T, typename> struct HasRepr : std::false_type {};
template <typename T>
struct HasRepr<T, Void<decltype(declval<T>().repr())>>
    : Same<decltype(declval<T>().repr()), String> {};
} // namespace detail
template <typename T> constexpr bool has_repr = detail::HasRepr<T, void>::value;

// see: https://en.cppreference.com/w/cpp/utility/variant/visit
template <typename... Ts> struct overloads : Ts... {
  using Ts::operator()...;
};

// deduction guide needed pre-c++20: https://stackoverflow.com/a/75699136
template <typename... Ts> overloads(Ts...) -> overloads<Ts...>;

template <Index I, typename... Ts>
inline nth<I, Ts...> Get(const Tuple<Ts...> &t) {
  return std::get<I>(t);
}

template <typename K, typename V> struct Entry {
  K key;
  V value;

  inline bool operator==(const Entry &other) const {
    return (key == other.key) && (value == other.value);
  }
};

bool starts_with(const String &s, const String &p);
bool ends_with(const String &s, const String &p);
template <typename F, typename C, typename = Void<typename C::value_type>>
auto map(F f, const C &c);
List<char> as_list(const String &s);
template <typename E> List<Pair<Index, E>> enumerate(const List<E> &l);
List<Pair<Index, char>> enumerate(const String &s);
String join(String sep, const List<String> &l);
template <typename E> void assign(Set<E> &s1, const Set<E> &s2);
template <typename E> void assign(List<E> &l1, const List<E> &l2);
template <typename K, typename V>
void assign(Map<K, V> &m1, const Map<K, V> &m2);
template <typename C> C combine(const C &c1, const C &c2);
template <typename C, typename... Cs>
C combine(const C &first, const C &second, const Cs &...rest);
template <typename E> Set<E> intersection(const Set<E> &s1, const Set<E> &s2);
template <typename E> bool disjoint(const Set<E> &s1, const Set<E> &s2);
template <typename K, typename V> Set<K> keys(const Map<K, V> &m);
template <typename K, typename V> Set<V> values(const Map<K, V> &m);
template <typename K, typename V> List<Entry<K, V>> entries(const Map<K, V> &m);
template <typename E> List<E> repeat(Size n, const E &e);
template <Size N> List<String> repeat(Size n, const char (&s)[N]);
bool in(const String &p, const String &s);

template <typename T> String always_repr(const T &v);
template <typename T, EnableIf<has_repr<T>> = true>
inline String repr(const T &v) {
  // clangd is not happy with defining this out-of-line
  return v.repr();
}
String repr(char v);
String repr(signed char v);
String repr(short v);
String repr(int v);
String repr(long v);
String repr(long long v);
String repr(unsigned char v);
String repr(unsigned short v);
String repr(unsigned int v);
String repr(unsigned long v);
String repr(unsigned long long v);
String repr(float v);
String repr(double v);
String repr(long double v);
String repr(bool v);
String repr(const char *v);
template <Size N> String repr(const char (&v)[N]);
String repr(const String &v);
String repr(const StringView &v);
String repr(const NullOpt &);
template <typename T> String repr(const Optional<T> &v);
template <typename E> String repr(const List<E> &v);
template <typename E> String repr(const Set<E> &v);
template <typename F, typename S> String repr(const Pair<F, S> &v);
template <typename K, typename V> String repr(const Map<K, V> &v);
template <typename... Ts> String repr(const Tuple<Ts...> &v);

inline namespace detail {
using hlj::repr;

template <typename T, typename> struct Reprable : False {};
template <typename T>
struct Reprable<T, Void<decltype(repr(declval<T>()))>>
    : Same<decltype(repr(declval<T>())), String> {};
} // namespace detail
template <typename T>
constexpr bool reprable = detail::Reprable<T, void>::value;

template <typename T> String str(const T &v);
String str(char v);
String str(const char *v);
String str(const String &v);
String str(const StringView &v);

static constexpr struct Any {
} any;

template <typename T> class Match {
public:
  Match(Function<bool(const T &)> pred_);
  Match();
  template <typename... Ts> Match(const T &match, const Ts &...rest);
  Match(Any);

  bool match(const T &value) const;

private:
  Function<bool(const T &)> pred;
};

template <typename... Ts> class Pattern {
public:
  Pattern(const Match<Ts> &...pattern_);
  template <typename... Us, EnableIf<sizeof...(Ts) == 1> = true>
  Pattern(const Us &...pattern_);
  Pattern(Any);

  bool match(const Ts &...values) const;

private:
  Tuple<Match<Ts>...> pattern;

  // todo: ugly
  bool match_any = false;

  template <Index... Is> bool match(Seq<Is...>, const Ts &...args) const;
};

template <typename R, typename... Ts>
using Matcher = Function<R(const List<Pair<Pattern<Ts...>, R>> &)>;

template <typename R = void, typename... Ts>
Matcher<R, Ts...> match(const Ts &...values, const R &default_result);

template <typename R = void, typename... Ts>
Matcher<R, Ts...> match(const Ts &...values);

template <typename R = void, typename... Ts>
Matcher<Function<R()>, Ts...> match_do(const Ts &...values,
                                       const R &default_result);

template <typename R = void, typename... Ts>
Matcher<Function<R()>, Ts...> lazy_match(const Ts &...values);

// inspiration: https://gist.github.com/shoooe/9202235
class Whatever {
public:
  Whatever() : store{nullptr} {}

  template <typename T>
  Whatever(const T &value) : store{make_unique<Concrete<decay<T>>>(value)} {}

  Whatever(const Whatever &other)
      : store{other.store ? other.store->copy() : nullptr} {}

  Whatever &operator=(const Whatever &other) {
    store = other.store ? other.store->copy() : nullptr;
    return *this;
  }

  Whatever(Whatever &&other) : store{std::move(other.store)} {}

  Whatever &operator=(Whatever &&other) {
    store = std::move(other.store);
    return *this;
  }

  template <typename T> const T &as() const {
    return reinterpret_cast<Concrete<T> *>(store.get())->value;
  }

  String repr() const;

  template <typename R>
  using Matcher = List<Pair<Pattern<const TypeInfo &>, R>>;
  template <typename R> R match(const Matcher<R> &type_results);

  template <typename R> using LazyMatcher = Matcher<Function<R()>>;
  template <typename R> R lazy_match(const LazyMatcher<R> &type_results);

private:
  struct Store {
    virtual ~Store() = 0;
    virtual UPtr<Store> copy() const = 0;
    virtual const TypeInfo &get_type_info() const = 0;
    virtual String repr() const = 0;
  };

  template <typename T> struct Concrete : Store {
    const T value;

    Concrete(const T &value_) : value{value_} {}
    ~Concrete() override = default;
    UPtr<Store> copy() const final { return make_unique<Concrete<T>>(value); }
    const TypeInfo &get_type_info() const final { return type_info<T>; }
    String repr() const final { return always_repr(value); }
  };

  UPtr<Store> store;
};

template <typename... Ts> class OneOf {
public:
  template <typename T, EnableIf<is_in<T, Ts...>> = true,
            Index I = index<T, Ts...>>
  OneOf(const T &value) : store{value}, which{I} {}

  OneOf(const OneOf &other) = default;
  OneOf &operator=(const OneOf &other) = default;

  OneOf(OneOf &&other) : store{std::move(other.store)}, which{other.which} {
    other.which = sizeof...(Ts);
  }

  OneOf &operator=(OneOf &&other) {
    store = std::move(other.store);
    which = other.which;
    other.which = sizeof...(Ts);
  }

  template <typename T, size_t I = index<T, Ts...>> bool is() const {
    return which == I;
  }

  template <typename T, size_t = index<T, Ts...>> const T &as() const {
    return store.as<T>();
  }

  String repr() const { return store.repr(); }

private:
  Whatever store;
  size_t which;
};

inline namespace detail {
void format(SStream &ss, Index &i, const String &s);
template <typename T, typename... Ts>
void format(SStream &ss, Index &i, const String &s, const T &v0,
            const Ts &...vs);
} // namespace detail
template <typename... Ts> String format(const String &s, const Ts &...vs);
// todo: format(const Ts &...) possible?
template <typename... Ts> String formato(const Ts &...vs);

void print();
template <typename... Ts> void print(const String &s, const Ts &...vs);
template <typename... Ts> void printo(const Ts &...vs);

struct Indent {
  Index stop = 1;
  Size size = 2;

  String apply(const String &s) const;
  String operator+(const String &s) const;
  Indent operator+(Size delta) const;
};

struct Bracket {
  enum class Kind {
    Brackets,
    Parentheses,
    Braces,
    AngleBrackets,
  };

  enum class Style {
    Plain,
    Spaced,
    Block,
  };

  Kind kind{Kind::Brackets};
  Style style{Style::Plain};
  Indent indent{};

  Bracket() {}
  Bracket(Style style_) : style(style_) {}
  Bracket(Kind kind_, Style style_) : kind(kind_), style(style_) {}
  Bracket(Style style_, Kind kind_) : kind(kind_), style(style_) {}
  Bracket(Kind kind_, Indent indent_)
      : kind(kind_), style(Style::Block), indent(indent_) {}
  String apply(const String &s) const;
};

String indent(const String &s, const Indent &ind = {});
String bracket(const String &s, const Bracket &brk = {});
String bracket(const String &s, const Indent &ind);
String parenthesize(const String &s,
                    const Bracket::Style &sty = Bracket::Style::Plain);
String parenthesize(const String &s, const Indent &ind);
String brace(const String &s,
             const Bracket::Style &sty = Bracket::Style::Plain);
String brace(const String &s, const Indent &ind);
String angle_bracket(const String &s,
                     const Bracket::Style &sty = Bracket::Style::Plain);
String angle_bracket(const String &s, const Indent &ind);

template <typename Dispatcher> class AutoDispatch {
public:
  AutoDispatch(Dispatcher &&dispatcher, Seconds timeout, Milliseconds interval)
      : dispatcher(move(dispatcher)) {
    const bool live_forever = (timeout == 0s);
    const auto end = now() + timeout;
    thread = Thread([=]() {
      while (!stop_signaled && (live_forever || now() < end)) {
        dispatcher.dispatch();
        sleep_for(interval);
      }
    });
  }

  template <typename Timeout, typename Interval>
  AutoDispatch(Dispatcher &&dispatcher, Timeout timeout, Interval interval)
      : AutoDispatch(std::move(dispatcher), duration_cast<Seconds>(timeout),
                     duration_cast<Milliseconds>(interval)) {}

  template <typename Timeout>
  AutoDispatch(Dispatcher &&dispatcher, Timeout timeout)
      : AutoDispatch(std::move(dispatcher), timeout, 50ms) {}

  AutoDispatch(Dispatcher &&dispatcher)
      : AutoDispatch(std::move(dispatcher), 0s) {}

  virtual ~AutoDispatch() noexcept { stop(); }

  AutoDispatch(AutoDispatch &&other) noexcept = default;

  AutoDispatch &operator=(AutoDispatch &&other) noexcept = default;

  const Dispatcher &operator*() const { return dispatcher; }

  Dispatcher &operator*() { return dispatcher; }

  const Dispatcher *operator->() const { return &dispatcher; }

  Dispatcher *operator->() { return &dispatcher; }

  void signal_stop() { stop_signaled = true; }

  void stop() {
    signal_stop();
    join();
  }

  void join() {
    if (thread.joinable()) {
      thread.join();
    }
  }

private:
  Dispatcher dispatcher;
  Thread thread;
  std::atomic_bool stop_signaled = false;
};

} // namespace hlj

// see: https://stackoverflow.com/a/20170989
template <typename T>
inline const hlj::String hlj::TypeName<T>::name = []() {
  using TR = std::remove_reference_t<T>;

  std::unique_ptr<char, void (*)(void *)> own(
      abi::__cxa_demangle(typeid(TR).name(), nullptr, nullptr, nullptr),
      std::free);

  SStream r;
  r << own.get();

  if (std::is_const_v<TR>) {
    r << " const";
  }

  if (std::is_volatile_v<TR>) {
    r << " volatile";
  }

  if (std::is_lvalue_reference_v<T>) {
    r << " &";
  } else if (std::is_rvalue_reference_v<T>) {
    r << " &&";
  }

  return r.str();
}();

template <typename E> struct hlj::TypeName<hlj::List<E>> {
  static const String name;
};
template <typename E>
inline const hlj::String hlj::TypeName<hlj::List<E>>::name =
    format("List<{}>", type_name<E>);

template <typename E> struct hlj::TypeName<hlj::Queue<E>> {
  static const String name;
};
template <typename E>
inline const hlj::String hlj::TypeName<hlj::Queue<E>>::name =
    format("Queue<{}>", type_name<E>);

template <typename E> struct hlj::TypeName<hlj::Set<E>> {
  static const String name;
};
template <typename E>
inline const hlj::String hlj::TypeName<hlj::Set<E>>::name =
    format("Set<{}>", type_name<E>);

template <typename F, typename S> struct hlj::TypeName<hlj::Pair<F, S>> {
  static const String name;
};
template <typename F, typename S>
inline const hlj::String hlj::TypeName<hlj::Pair<F, S>>::name =
    format("Pair<{}, {}>", type_name<F>, type_name<S>);

template <typename K, typename V> struct hlj::TypeName<hlj::Map<K, V>> {
  static const String name;
};
template <typename K, typename V>
inline const hlj::String hlj::TypeName<hlj::Map<K, V>>::name =
    format("Map<{}, {}>", type_name<K>, type_name<V>);

template <typename... Ts> struct hlj::TypeName<hlj::Tuple<Ts...>> {
  static const String name;
};
template <typename... Ts>
inline const hlj::String hlj::TypeName<hlj::Tuple<Ts...>>::name =
    format("Tuple<{}>", join(", ", {type_name<Ts>...}));

template <typename T> struct hlj::TypeName<hlj::Optional<T>> {
  static const String name;
};
template <typename T>
inline const hlj::String hlj::TypeName<hlj::Optional<T>>::name =
    format("Optional<{}>", type_name<T>);

inline bool hlj::starts_with(const String &s, const String &p) {
  return (s.size() >= p.size()) && (s.substr(0, p.size()) == p);
}

inline bool hlj::ends_with(const String &s, const String &p) {
  return (s.size() >= p.size()) && (s.substr(s.size() - p.size()) == p);
}

template <typename T> inline hlj::String hlj::Type<T>::repr() const {
  return format("type {}", name);
}

template <typename F, typename C, typename>
inline auto hlj::map(F f, const C &c) {
  using E = typename C::value_type;
  using R = decltype(f(std::declval<E>()));
  List<R> r;
  for (const auto &e : c) {
    r.push_back(f(e));
  }
  return r;
}

inline hlj::List<char> hlj::as_list(const String &s) {
  List<char> r;
  for (char c : s) {
    r.push_back(c);
  }
  return r;
}

template <typename E>
inline hlj::List<hlj::Pair<hlj::Index, E>> hlj::enumerate(const List<E> &l) {
  Index i = 0;
  return map([&](E e) { return make_pair(i++, e); }, l);
}

inline hlj::List<hlj::Pair<hlj::Index, char>> hlj::enumerate(const String &s) {
  return enumerate(as_list(s));
}

inline hlj::String hlj::join(String sep, const List<String> &l) {
  SStream r;
  for (auto const &[i, s] : enumerate(l)) {
    if (i) {
      r << sep;
    }
    r << s;
  }
  return r.str();
}

template <typename E> inline void hlj::assign(Set<E> &s1, const Set<E> &s2) {
  for (const auto &e : s2) {
    s1.insert(e);
  }
}

template <typename E> inline void hlj::assign(List<E> &l1, const List<E> &l2) {
  for (const auto &e : l2) {
    l1.push_back(l2);
  }
}

template <typename K, typename V>
inline void hlj::assign(Map<K, V> &m1, const Map<K, V> &m2) {
  for (const auto &[k, v] : m2) {
    m1[k] = v;
  }
}

template <typename C> inline C hlj::combine(const C &c1, const C &c2) {
  C r{c1};
  assign(r, c2);
  return r;
}

template <typename C, typename... Cs>
C hlj::combine(const C &first, const C &second, const Cs &...rest) {
  return combine(combine(first, second), rest...);
}

template <typename E>
inline hlj::Set<E> hlj::intersection(const Set<E> &s1, const Set<E> &s2) {
  Set<E> s;
  for (const auto &e : s1) {
    if (s2.count(e)) {
      s.insert(e);
    }
  }
  return s;
}

template <typename E>
inline bool hlj::disjoint(const Set<E> &s1, const Set<E> &s2) {
  return intersection(s1, s2).size() == 0;
}

template <typename K, typename V>
inline hlj::Set<K> hlj::keys(const Map<K, V> &m) {
  Set<K> r;
  for (const auto &[k, _] : m) {
    r.insert(k);
  }
  return r;
}

template <typename K, typename V>
inline hlj::Set<V> hlj::values(const Map<K, V> &m) {
  Set<V> r;
  for (const auto &[_, v] : m) {
    r.insert(v);
  }
  return r;
}

template <typename K, typename V>
inline hlj::List<hlj::Entry<K, V>> hlj::entries(const Map<K, V> &m) {
  return map([&](auto k) { return Entry<K, V>{k, m.at(k)}; }, keys(m));
}

template <typename E> inline hlj::List<E> hlj::repeat(Size n, const E &e) {
  List<E> r;
  for (Index i = 0; i < n; i++) {
    r.push_back(e);
  }
  return r;
}

template <hlj::Size N>
inline hlj::List<hlj::String> hlj::repeat(Size n, const char (&s)[N]) {
  List<String> r;
  for (Index i = 0; i < n; i++) {
    r.push_back(String(s));
  }
  return r;
}

inline bool hlj::in(const String &p, const String &s) {
  return s.find(p) != String::npos;
}

template <typename T> inline hlj::String hlj::always_repr(const T &v) {
  if constexpr (reprable<T>) {
    return repr(v);
  } else {
    return format("<{}>", type_name<T>);
  }
}

inline hlj::String hlj::repr(char v) { return hlj::format("'{}'", String{v}); }

inline hlj::String hlj::repr(signed char v) { return std::to_string(v); }

inline hlj::String hlj::repr(short v) { return std::to_string(v); }

inline hlj::String hlj::repr(int v) { return std::to_string(v); }

inline hlj::String hlj::repr(long v) { return std::to_string(v); }

inline hlj::String hlj::repr(long long v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned char v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned short v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned int v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned long v) { return std::to_string(v); }

inline hlj::String hlj::repr(unsigned long long v) { return std::to_string(v); }

inline hlj::String hlj::repr(float v) { return std::to_string(v); }

inline hlj::String hlj::repr(double v) { return std::to_string(v); }

inline hlj::String hlj::repr(long double v) { return std::to_string(v); }

inline hlj::String hlj::repr(bool v) { return v ? "true" : "false"; }

inline hlj::String hlj::repr(const char *v) { return "\"" + String(v) + "\""; }

inline hlj::String hlj::repr(const String &v) { return "\"" + v + "\""; }

template <hlj::Size N> inline hlj::String hlj::repr(const char (&v)[N]) {
  return hlj::repr(String(v));
}

inline hlj::String hlj::repr(const StringView &v) {
  return hlj::repr(String(v));
}

inline hlj::String hlj::repr(const NullOpt &) { return "nullopt"; }

template <typename T> inline hlj::String hlj::repr(const Optional<T> &v) {
  return v ? repr(*v) : "nullopt";
}

template <typename E> inline hlj::String hlj::repr(const List<E> &v) {
  return format("[{}]",
                join(", ", map([&](const E &e) { return repr(e); }, v)));
}

template <typename T> inline hlj::String hlj::repr(const Set<T> &v) {
  return format("{{{}}}",
                join(", ", map([&](const auto &e) { return repr(e); }, v)));
}

template <typename F, typename S> hlj::String hlj::repr(const Pair<F, S> &v) {
  return format("({}, {})", v.first, v.second);
}

template <typename K, typename V> hlj::String hlj::repr(const Map<K, V> &v) {
  return format("{{{}}}", join(", ", map(
                                         [&](const auto &e) {
                                           return format("{} => {}",
                                                         repr(e.key),
                                                         repr(e.value));
                                         },
                                         entries(v))));
}

template <typename... Ts> inline hlj::String hlj::repr(const Tuple<Ts...> &v) {
  const auto repr_ = [&](const Ts &...vs) {
    std::vector<String> list;
    (list.push_back(repr(vs)), ...);
    return format("({})", join(", ", list));
  };

  return std::apply(repr_, v);
}

template <typename T> inline hlj::String hlj::str(const T &v) {
  return repr(v);
}

inline hlj::String hlj::str(char v) { return String{v}; }

inline hlj::String hlj::str(const char *v) { return v; }

inline hlj::String hlj::str(const String &v) { return v; }

inline hlj::String hlj::str(const StringView &v) { return String(v); }

template <typename R>
inline R hlj::Whatever::match(const Matcher<R> &type_results) {
  return hlj::match<R, const TypeInfo &>(store->get_type_info())(type_results);
}

template <typename R>
inline R hlj::Whatever::lazy_match(const LazyMatcher<R> &type_results) {
  return match(type_results)();
}

inline hlj::Whatever::Store::~Store() {};

inline hlj::String hlj::Whatever::repr() const {
  return store
             ? format("{} {}", store->get_type_info().get_name(), store->repr())
             : "nothing";
}

inline void hlj::detail::format(SStream &ss, Index &i, const String &s) {
  const Size n = s.size();
  while (i < n) {
    const char c = s[i++];
    switch (c) {
    case '{': {
      if (i < n && s[i++] == '{') {
        ss << '{';
      } else {
        // todo: this could be caused by not enough values supplied
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '{{')", s));
      }
      break;
    }
    case '}': {
      if (i < n && s[i++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      ss << c;
    }
    }
  }
}

template <typename T, typename... Ts>
inline void hlj::detail::format(SStream &ss, Index &i, const String &s,
                                const T &v0, const Ts &...vs) {
  const Size n = s.size();
  bool flag = false;
  while (i < n) {
    const char c = s[i++];
    switch (c) {
    case '{': {
      if (i < n && s[i] == '{') {
        i++;
        ss << '{';
      } else {
        flag = !flag;
      }
      break;
    }
    case '}': {
      if (flag) {
        ss << hlj::str(v0);
        format(ss, i, s, vs...);
        return;
      } else if (i < n && s[i++] == '}') {
        ss << '}';
      } else {
        throw std::invalid_argument(
            hlj::format("invalid format string: \"{}\" (dangling '}}')", s));
      }
      break;
    }
    default: {
      if (flag) {
        throw std::invalid_argument(hlj::format(
            "invalid format string: \"{}\" (format specifiers not supported)",
            s));
      } else {
        ss << c;
      }
      break;
    }
    }
  }
  throw std::invalid_argument(hlj::format("invalid format string: \"{}\"", s));
}

template <typename... Ts>
inline hlj::String hlj::format(const String &s, const Ts &...vs) {
  SStream ss;
  Index i = 0;
  detail::format(ss, i, s, vs...);
  return ss.str();
}

template <typename... Ts> inline hlj::String hlj::formato(const Ts &...vs) {
  return format(join(" ", repeat(sizeof...(Ts), "{}")), vs...);
}

inline void hlj::print() { printf("\n"); }

template <typename... Ts>
inline void hlj::print(const String &s, const Ts &...vs) {
  printf("%s\n", format(s, vs...).c_str());
}

template <typename... Ts> inline void hlj::printo(const Ts &...vs) {
  printf("%s\n", formato(vs...).c_str());
}

inline hlj::String hlj::Indent::apply(const String &s) const {
  const String tab(stop * size, ' ');
  SStream r;
  r << tab;
  const Size n = s.size();
  for (const auto &[i, c] : enumerate(s)) {
    switch (c) {
    case '\n':
      r << c;
      if (i + 1 < n && s[i + 1] != '\n') {
        r << tab;
      }
      break;
    default:
      r << c;
      break;
    }
  }
  return r.str();
}

inline hlj::String hlj::Indent::operator+(const String &s) const {
  return apply(s);
}

inline hlj::Indent hlj::Indent::operator+(Size delta) const {
  return {stop + delta, size};
}

inline hlj::String hlj::Bracket::apply(const std::string &s) const {
  String open;
  String close;
  switch (kind) {
  case Kind::Parentheses:
    open = "(";
    close = ")";
    break;
  case Kind::Brackets:
    open = "[";
    close = "]";
    break;
  case Kind::Braces:
    open = "{";
    close = "}";
    break;
  case Kind::AngleBrackets:
    open = "<";
    close = ">";
    break;
  }

  if (s == "") {
    return join("", {open, close});
  }

  switch (style) {
  case Style::Plain:
    return join("", {open, s, close});

  case Style::Spaced:
    return join(" ", {open, s, close});

  case Style::Block:
    return join("\n", {open, indent + s, close});
  }
  return s;
}

inline hlj::String hlj::indent(const String &s, const Indent &ind) {
  return ind + s;
}

inline hlj::String hlj::bracket(const String &s, const Bracket &bracket) {
  return bracket.apply(s);
}

inline hlj::String hlj::bracket(const String &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Brackets, indent}.apply(s);
}

inline hlj::String hlj::parenthesize(const String &s,
                                     const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Parentheses, style}.apply(s);
}

inline hlj::String hlj::parenthesize(const String &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Parentheses, indent}.apply(s);
}

inline hlj::String hlj::brace(const String &s, const Bracket::Style &style) {
  return Bracket{Bracket::Kind::Braces, style}.apply(s);
}

inline hlj::String hlj::brace(const String &s, const Indent &indent) {
  return Bracket{Bracket::Kind::Braces, indent}.apply(s);
}

inline hlj::String hlj::angle_bracket(const String &s,
                                      const Bracket::Style &style) {
  return Bracket{Bracket::Kind::AngleBrackets, style}.apply(s);
}

inline hlj::String hlj::angle_bracket(const String &s, const Indent &indent) {
  return Bracket{Bracket::Kind::AngleBrackets, indent}.apply(s);
}

template <typename T>
hlj::Match<T>::Match(Function<bool(const T &)> pred_) : pred(pred_) {}

template <typename T>
hlj::Match<T>::Match() : Match([](const auto &) { return false; }) {}

template <typename T>
template <typename... Ts>
hlj::Match<T>::Match(const T &match, const Ts &...rest)
    : Match([&match, rest_match = Match(rest...)](const T &value) {
        return (value == match) || rest_match.match(value);
      }) {}

template <typename T>
hlj::Match<T>::Match(Any) : Match([](const auto &) { return true; }) {}

template <typename T> bool hlj::Match<T>::match(const T &value) const {
  return pred(value);
}

template <typename... Ts>
hlj::Pattern<Ts...>::Pattern(const Match<Ts> &...pattern_)
    : pattern(std::make_tuple(pattern_...)) {}

template <typename... Ts>
template <typename... Us, hlj::EnableIf<sizeof...(Ts) == 1>>
hlj::Pattern<Ts...>::Pattern(const Us &...pattern_)
    : pattern(Match<nth<0, Ts...>>(pattern_...)) {}

template <typename... Ts> hlj::Pattern<Ts...>::Pattern(Any) : match_any(true) {}

template <typename... Ts>
bool hlj::Pattern<Ts...>::match(const Ts &...values) const {
  return match(SeqFor<Ts...>, values...);
}

template <typename... Ts>
template <hlj::Index... Is>
bool hlj::Pattern<Ts...>::match(Seq<Is...>, const Ts &...args) const {
  return match_any || (Get<Is>(pattern).match(args) && ...);
}

template <typename R, typename... Ts>
hlj::Matcher<R, Ts...> hlj::match(const Ts &...values) {
  return [&](const auto &pattern_results) {
    for (const auto &[pattern, result] : pattern_results) {
      if (pattern.match(values...)) {
        return result;
      }
    }
    throw std::invalid_argument("no match");
  };
}

template <typename R, typename... Ts>
hlj::Matcher<hlj::Function<R()>, Ts...> hlj::lazy_match(const Ts &...values) {
  return [&](const auto &pattern_results) {
    return match<Function<R()>>(values...)(pattern_results)();
  };
}
