#pragma GCC diagnostic ignored "-Wsign-compare"

/// \file
///
/// Test for inline_vector
///
/// Most of the tests below are adapted from libc++: https://libcxx.llvm.org
/// under the following license:
//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <beman/inplace_vector/inplace_vector.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#define CHECK(...)                                                             \
  static_cast<void>((__VA_ARGS__)                                              \
                        ? void(0)                                              \
                        : ::beman::__iv_detail::__assert_failure(              \
                              static_cast<const char *>(__FILE__), __LINE__,   \
                              "assertion failed: " #__VA_ARGS__))

#define CHECK_THROWS(EXPR, EXCEPT)                                             \
  if (auto e =                                                                 \
          [&] {                                                                \
            try {                                                              \
              (EXPR);                                                          \
              return false;                                                    \
            } catch (const EXCEPT &) {                                         \
              return true;                                                     \
            } catch (...) {                                                    \
              return false;                                                    \
            }                                                                  \
          }();                                                                 \
      !e) {                                                                    \
    ::beman::__iv_detail::__assert_failure(                                    \
        static_cast<const char *>(__FILE__), __LINE__,                         \
        "expression failed to throw " #EXCEPT ": " #EXPR);                     \
  }

template struct beman::__iv_detail::__storage::__zero_sized<int>;
template struct beman::__iv_detail::__storage::__trivial<int, 10>;
template struct beman::__iv_detail::__storage::__non_trivial<
    std::unique_ptr<int>, 10>;

template struct beman::__iv_detail::__storage::__zero_sized<const int>;
template struct beman::__iv_detail::__storage::__trivial<const int, 10>;
template struct beman::__iv_detail::__storage::__non_trivial<
    const std::unique_ptr<int>, 10>;

// empty:
template struct beman::inplace_vector<int, 0>;

// trivial non-empty:
template struct beman::inplace_vector<int, 1>;
template struct beman::inplace_vector<int, 2>;
template struct beman::inplace_vector<const int, 3>;

// non-trivial
template struct beman::inplace_vector<std::string, 3>;
template struct beman::inplace_vector<const std::string, 3>;

// move-only:
template struct beman::inplace_vector<std::unique_ptr<int>, 3>;
template struct beman::inplace_vector<const std::unique_ptr<int>, 3>;

struct tint {
  std::size_t i;
  tint() = default;
  constexpr tint(tint const &) = default;
  constexpr tint(tint &&) = default;
  constexpr tint &operator=(tint const &) = default;
  constexpr tint &operator=(tint &&) = default;
  // FIXME: ~tint() = default;
  //        ^^^ adding this makes the class non-trivial in clang

  explicit constexpr tint(std::size_t j) : i(j) {}
  explicit operator std::size_t() { return i; }
};

static_assert(std::is_trivial<tint>{} && std::is_copy_constructible<tint>{} &&
                  std::is_move_constructible<tint>{},
              "");

// Explicit instantiations
template struct beman::inplace_vector<tint, 0>; // trivial empty
template struct beman::inplace_vector<tint, 1>; // trivial non-empty
template struct beman::inplace_vector<tint, 2>; // trivial non-empty
template struct beman::inplace_vector<tint, 3>; // trivial non-empty

struct moint final {
  std::size_t i = 0;
  moint() = default;
  moint(moint const &) = delete;
  moint &operator=(moint const &) = delete;
  moint(moint &&) = default;
  moint &operator=(moint &&) = default;
  ~moint() = default;
  explicit operator std::size_t() { return i; }
  explicit constexpr moint(std::size_t j) : i(j) {}
  // it seems that deleting the copy constructor is not enough to make
  // this non-trivial using libstdc++:
  virtual void foo() {}
  bool operator==(moint b) { return i == b.i; }
};

static_assert(!std::is_trivial<moint>{} and
                  !std::is_copy_constructible<moint>{} and
                  std::is_move_constructible<moint>{},
              "");

// cannot explicitly instantiate the type for some types
// // non-trivial empty:
// template struct std::inplace_vector<moint, 0>;
// // non-trivial non-empty:
// template struct std::inplace_vector<moint, 1>;
// template struct std::inplace_vector<moint, 2>;
// template struct std::inplace_vector<moint, 3>;

template <typename T, std::size_t N> using vector = beman::inplace_vector<T, N>;

class non_copyable {
  int i_;
  double d_;

public:
  non_copyable(const non_copyable &) = delete;
  non_copyable &operator=(const non_copyable &) = delete;

  non_copyable(int i, double d) : i_(i), d_(d) {}

  non_copyable(non_copyable &&a) noexcept : i_(a.i_), d_(a.d_) {
    a.i_ = 0;
    a.d_ = 0;
  }

  non_copyable &operator=(non_copyable &&a) noexcept {
    i_ = a.i_;
    d_ = a.d_;
    a.i_ = 0;
    a.d_ = 0;
    return *this;
  }

  int geti() const { return i_; }
  double getd() const { return d_; }
};

template <typename T, int N> struct vec {
  vec() = default;
  vec(std::initializer_list<T> /*il*/) {}
};

template <typename T, std::size_t N> constexpr void test_il_constructor() {
  auto v = [] {
    switch (N) {
    case 0: {
      const vector<T, N> x{};
      return x;
    }
    case 1: {
      const vector<T, N> x({0});
      return x;
    }
    case 10: {
      const vector<T, N> x({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
      return x;
    }
    default:
      CHECK(false);
    }
  }();
  CHECK(v.size() == N);
  for (size_t i = 0; i < N; ++i)
    CHECK(v[i] == T(i));
  CHECK(11 > N);
  if !consteval {
    CHECK_THROWS(
        ([&] { const vector<T, N> x({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}); }()),
        std::bad_alloc);
  }
}

template <typename T, std::size_t N> constexpr void test_il_assignment() {
  auto v = [] {
    switch (N) {
    case 0: {
      const vector<T, N> x = {};
      return x;
    }
    case 1: {
      const vector<T, N> x = {0};
      return x;
    }
    case 10: {
      const vector<T, N> x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
      return x;
    }
    default:
      CHECK(false);
    }
  }();
  CHECK(v.size() == N);
  for (size_t i = 0; i < N; ++i)
    CHECK(v[i] == T(i));
  if !consteval {
    CHECK_THROWS(([&] {
                   [[maybe_unused]] const vector<T, N> x = {0, 1, 2, 3, 4, 5,
                                                            6, 7, 8, 9, 10};
                 }()),
                 std::bad_alloc);
  }
}

template <typename T, std::size_t N> constexpr void test_default_constructor() {
  vector<T, N> v;
  CHECK(v.size() == 0);
  CHECK(v.empty());
  CHECK(v.capacity() == N);
}

template <typename T, std::size_t N>
constexpr void
test_default_constructor_bounds_and_contiguous_iterators(std::size_t sz) {
  CHECK(sz <= N);
  vector<T, N> v(sz);
  CHECK(v.size() == sz);
  CHECK(v.max_size() == N);
  CHECK(v.capacity() == N);
  for (std::size_t i = 0; i != sz; ++i) {
    CHECK(v[i] == T{});
  }
  for (std::size_t i = 0; i < v.size(); ++i) { // contiguous
    CHECK(*(v.begin() + i) == *(std::addressof(*v.begin()) + i));
    CHECK(*(v.cbegin() + i) == *(std::addressof(*v.cbegin()) + i));
    CHECK(*(v.rbegin() + i) == *(std::addressof(*v.rbegin()) - i));
    CHECK(*(v.crbegin() + i) == *(std::addressof(*v.crbegin()) - i));
  }
  // iterators
  if (v.size() == 0) {
    CHECK(v.empty());
    CHECK(v.begin() == v.end());
    CHECK(v.cbegin() == v.cend());
    CHECK(v.rbegin() == v.rend());
    CHECK(v.crbegin() == v.crend());
  } else {
    CHECK(!v.empty());
    CHECK(v.begin() != v.end());
    CHECK(v.cbegin() != v.cend());
    CHECK(v.rbegin() != v.rend());
    CHECK(v.crbegin() != v.crend());
    CHECK(std::distance(v.begin(), v.end()) == v.size());
    CHECK(std::distance(v.cbegin(), v.cend()) == v.size());
    CHECK(std::distance(v.rbegin(), v.rend()) == v.size());
    CHECK(std::distance(v.crbegin(), v.crend()) == v.size());
    CHECK(v.back() == T{});
    CHECK(v.front() == T{});
  }
}

template <typename T, std::size_t N> constexpr void test_iterators() {
  { // N3644 testing
    using C = vector<T, N>;
    typename C::iterator ii1{}, ii2{};
    typename C::iterator ii4 = ii1;
    typename C::const_iterator cii{};
    CHECK(ii1 == ii2);
    CHECK(ii1 == ii4);

    CHECK(!(ii1 != ii2));

    CHECK((ii1 == cii));
    CHECK((cii == ii1));
    CHECK(!(ii1 != cii));
    CHECK(!(cii != ii1));
    CHECK(!(ii1 < cii));
    CHECK(!(cii < ii1));
    CHECK((ii1 <= cii));
    CHECK((cii <= ii1));
    CHECK(!(ii1 > cii));
    CHECK(!(cii > ii1));
    CHECK((ii1 >= cii));
    CHECK((cii >= ii1));
    CHECK((cii - ii1) == 0);
    CHECK((ii1 - cii) == 0);
  }
}

template <typename T, std::size_t N>
constexpr void test_constructor_input_iterators() {
  CHECK(N < 11);
  const T t[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  using C = vector<T, N>;
  C c(std::begin(t), std::begin(t) + N);
  CHECK(c.size() == N);
  CHECK(
      std::equal(std::begin(t), std::begin(t) + N, std::begin(c), std::end(c)));

  for (typename C::iterator i = c.begin(); i != c.end(); ++i) {
    auto idx = i - c.begin();
    auto o = t[idx];
    CHECK(*i == o);
    *i = 2 * o;
    CHECK(*i == (2 * o));
    CHECK(std::distance(c.begin(), i) == idx);
    CHECK(std::distance(i, c.end()) == c.size() - idx);
  }
}

template <typename T, std::size_t N> constexpr bool test_all_() {
  test_il_constructor<T, N>();
  test_il_assignment<T, N>();
  test_default_constructor<T, N>();
  for (size_t i = 0; i < N; ++i)
    test_default_constructor_bounds_and_contiguous_iterators<T, N>(i);
  test_iterators<T, N>();
  test_constructor_input_iterators<T, N>();
  return true;
}

template <typename T, std::size_t N> void test_all() {
  constexpr bool ct = test_all_<T, N>();
  static_assert(ct, "CONSTEXPR TESTS FAILED");
  test_all_<T, N>();
}

int main() {
  { // storage
    using beman::__iv_detail::__storage::__non_trivial;
    using beman::__iv_detail::__storage::__trivial;
    using beman::__iv_detail::__storage::__zero_sized;
    using beman::__iv_detail::__storage::_t;

    static_assert(std::is_same<_t<int, 0>, __zero_sized<int>>{});
    static_assert(std::is_same<_t<int, 10>, __trivial<int, 10>>{});
    static_assert(std::is_same<_t<std::unique_ptr<int>, 10>,
                               __non_trivial<std::unique_ptr<int>, 10>>{},
                  "");
  }

  test_all<int, 0>();
  test_all<int, 1>();
  test_all<int, 10>();

  // test_all<const int, 0>();

  { // capacity
    vector<int, 10> a;
    static_assert(a.capacity() == std::size_t(10));
    CHECK(a.empty());
    for (std::size_t i = 0; i != 10; ++i) {
      a.push_back(0);
    }
    static_assert(a.capacity() == std::size_t(10));
    CHECK(a.size() == std::size_t(10));
    CHECK(!a.empty());
    CHECK_THROWS(a.push_back(0), std::bad_alloc);
    CHECK((uintptr_t) nullptr == (uintptr_t)a.try_push_back(0));
  }

  { // resize copyable
    using Copyable = int;
    vector<Copyable, 10> a(std::size_t(10), 5);
    CHECK(a.size() == std::size_t(10));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    for (std::size_t i = 0; i != 10; ++i) {
      CHECK(a[i] == 5);
    }
    a.resize(5);
    CHECK(a.size() == std::size_t(5));

    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    a.resize(9);
    CHECK(a[4] == 5);
    for (std::size_t i = 5; i != 9; ++i) {
      CHECK(a[i] == 0);
    }
    CHECK(a.size() == std::size_t(9));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    a.resize(10, 3);
    CHECK(a[4] == 5);
    CHECK(a[8] == 0);
    CHECK(a[9] == 3);
    CHECK(a.size() == std::size_t(10));
    static_assert(a.capacity() == std::size_t(10));
    a.resize(5, 2);
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(a[i] == 5);
    }
    // test_contiguous(a);
    CHECK_THROWS(a.resize(12), std::bad_alloc);
  }
  { // resize move-only
    using MoveOnly = std::unique_ptr<int>;
    vector<MoveOnly, 10> a(10);
    CHECK(a.size() == std::size_t(10));
    static_assert(a.capacity() == std::size_t(10));
    a.resize(5);
    // test_contiguous(a);
    CHECK(a.size() == std::size_t(5));
    static_assert(a.capacity() == std::size_t(10));
    a.resize(9);
    CHECK(a.size() == std::size_t(9));
    static_assert(a.capacity() == std::size_t(10));
  }

  { // resize value:
    using Copyable = int;
    vector<Copyable, 10> a(std::size_t(10));
    CHECK(a.size() == std::size_t(10));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    for (std::size_t i = 0; i != 10; ++i) {
      CHECK(a[i] == 0);
    }
    a.resize(5);
    CHECK(a.size() == std::size_t(5));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(a[i] == 0);
    }
    a.resize(9, 5);
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(a[i] == 0);
    }
    for (std::size_t i = 5; i != 9; ++i) {
      CHECK(a[i] == 5);
    }
    CHECK(a.size() == std::size_t(9));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
    a.resize(10, 3);
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(a[i] == 0);
    }
    for (std::size_t i = 5; i != 9; ++i) {
      CHECK(a[i] == 5);
    }
    CHECK(a[9] == 3);
    CHECK(a.size() == std::size_t(10));
    static_assert(a.capacity() == std::size_t(10));
    // test_contiguous(a);
  }

  { // assign copy
    vector<int, 3> z(3, 5);
    vector<int, 3> a = {0, 1, 2};
    CHECK(a.size() == std::size_t{3});
    vector<int, 3> b;
    CHECK(b.size() == std::size_t{0});
    b = a;
    CHECK(b.size() == std::size_t{3});
    CHECK(std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b)));
  }

  { // copy construct
    vector<int, 3> a = {0, 1, 2};
    CHECK(a.size() == std::size_t{3});
    vector<int, 3> b(a);
    CHECK(b.size() == std::size_t{3});

    CHECK(std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b)));
  }

  { // assign move
    using MoveOnly = std::unique_ptr<int>;
    vector<MoveOnly, 3> a(3);
    CHECK(a.size() == std::size_t{3});
    vector<MoveOnly, 3> b;
    CHECK(b.size() == std::size_t{0});
    b = std::move(a);
    CHECK(b.size() == std::size_t{3});
    CHECK(a.size() == std::size_t{3});
  }

  { // move construct
    using MoveOnly = std::unique_ptr<int>;
    vector<MoveOnly, 3> a(3);
    CHECK(a.size() == std::size_t{3});
    vector<MoveOnly, 3> b(std::move(a));
    CHECK(b.size() == std::size_t{3});
    CHECK(a.size() == std::size_t{3});
  }

  { // old tests
    using vec_t = vector<int, 5>;
    vec_t vec1(5);
    vec1[0] = 0;
    vec1[1] = 1;
    vec1[2] = 2;
    vec1[3] = 3;
    vec1[4] = 4;
    {
      vec_t vec2;
      vec2.push_back(5);
      vec2.push_back(6);
      vec2.push_back(7);
      vec2.push_back(8);
      vec2.push_back(9);
      CHECK(vec1[0] == 0);
      CHECK(vec1[4] == 4);
      CHECK(vec2[0] == 5);
      CHECK(vec2[4] == 9);
    }
    {
      vec_t vec2;
      vec2.try_push_back(5);
      vec2.try_push_back(6);
      vec2.try_push_back(7);
      vec2.try_push_back(8);
      vec2.try_push_back(9);
      CHECK(vec1[0] == 0);
      CHECK(vec1[4] == 4);
      CHECK(vec2[0] == 5);
      CHECK(vec2[4] == 9);
    }
    {
      vec_t vec2;
      vec2.unchecked_push_back(5);
      vec2.unchecked_push_back(6);
      vec2.unchecked_push_back(7);
      vec2.unchecked_push_back(8);
      vec2.unchecked_push_back(9);
      CHECK(vec1[0] == 0);
      CHECK(vec1[4] == 4);
      CHECK(vec2[0] == 5);
      CHECK(vec2[4] == 9);
    }
    {
      auto vec2 = vec1;
      CHECK(vec2[0] == 0);
      CHECK(vec2[4] == 4);
      CHECK(vec1[0] == 0);
      CHECK(vec1[4] == 4);
    }
    {
      int count_ = 0;
      for (auto i : vec1) {
        CHECK(i == count_);
        count_++;
      }
    }

    {
      std::vector<int> vec2(5);
      vec2[0] = 4;
      vec2[1] = 3;
      vec2[2] = 2;
      vec2[3] = 1;
      vec2[4] = 0;
      vec_t vec(vec2.size());
      copy(std::begin(vec2), std::end(vec2), std::begin(vec));
      int count_ = 4;
      for (auto i : vec) {
        CHECK(i == count_);
        count_--;
      }
    }
  }
  {
    using vec_t = vector<int, 0>;
    static_assert(sizeof(vec_t) == 1, "");

    constexpr auto a = vec_t{};
    static_assert(a.size() == std::size_t{0}, "");
  }

  { // back and front:
    using C = vector<int, 2>;
    C c(1);
    CHECK(c.back() == 0);
    CHECK(c.front() == 0);
    CHECK(c[0] == 0);
    c.clear();
    int one = 1;
    c.push_back(one);
    CHECK(c.back() == 1);
    CHECK(c.front() == 1);
    CHECK(c[0] == 1);
    CHECK(c.size() == 1);
    c.push_back(2);
    CHECK(c.back() == 2);
    CHECK(c.front() == 1);
    CHECK(c[0] == 1);
    CHECK(c[1] == 2);
    CHECK(c.size() == 2);
    c.pop_back();
    CHECK(c.front() == 1);
    CHECK(c[0] == 1);
    CHECK(c.back() == 1);
    c.pop_back();
    CHECK(c.empty());
  }

  { // const back:
    using C = vector<int, 2>;
    constexpr C c(1);
    static_assert(c.back() == 0);
    static_assert(c.front() == 0);
    static_assert(c[0] == 0);
    static_assert(c.size() == 1);
  }

  { // swap: same type
    using C = vector<int, 5>;
    C c0(3, 5);
    C c1(5, 1);
    C c2(0);
    CHECK(c0.size() == std::size_t(3));
    CHECK(c1.size() == std::size_t(5));
    CHECK(c2.size() == std::size_t(0));
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c0[i] == 5);
    }
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(c1[i] == 1);
    }
    c0.swap(c1);
    CHECK(c0.size() == std::size_t(5));
    CHECK(c1.size() == std::size_t(3));
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(c0[i] == 1);
    }
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c1[i] == 5);
    }
    c2.swap(c1);
    CHECK(c1.size() == std::size_t(0));
    CHECK(c2.size() == std::size_t(3));
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c2[i] == 5);
    }
  }

  { // std::swap: same type
    using C = vector<int, 5>;
    C c0(3, 5);
    C c1(5, 1);
    C c2(0);
    CHECK(c0.size() == std::size_t(3));
    CHECK(c1.size() == std::size_t(5));
    CHECK(c2.size() == std::size_t(0));
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c0[i] == 5);
    }
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(c1[i] == 1);
    }
    std::swap(c0, c1);
    CHECK(c0.size() == std::size_t(5));
    CHECK(c1.size() == std::size_t(3));
    for (std::size_t i = 0; i != 5; ++i) {
      CHECK(c0[i] == 1);
    }
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c1[i] == 5);
    }
    std::swap(c2, c1);
    CHECK(c1.size() == std::size_t(0));
    CHECK(c2.size() == std::size_t(3));
    for (std::size_t i = 0; i != 3; ++i) {
      CHECK(c2[i] == 5);
    }
  }

  {
    constexpr vector<int, 5> v;
    static_assert(v.data() != nullptr);

    constexpr vector<int, 0> v0;
    static_assert(v0.data() == nullptr);
  }

  { // emplace:
    {
      vector<non_copyable, 3> c;
      vector<non_copyable, 3>::iterator i = c.emplace(c.cbegin(), 2, 3.5);
      CHECK(i == c.begin());
      CHECK(c.size() == 1);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      i = c.emplace(c.cend(), 3, 4.5);
      CHECK(i == c.end() - 1);
      CHECK(c.size() == 2);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      CHECK(c.back().geti() == 3);
      CHECK(c.back().getd() == 4.5);
      i = c.emplace(c.cbegin() + 1, 4, 6.5);
      CHECK(i == c.begin() + 1);
      CHECK(c.size() == 3);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      CHECK(c[1].geti() == 4);
      CHECK(c[1].getd() == 6.5);
      CHECK(c.back().geti() == 3);
      CHECK(c.back().getd() == 4.5);
      CHECK_THROWS(c.emplace(c.cbegin(), 2, 3.5), std::bad_alloc);
    }
    {
      vector<non_copyable, 3> c;
      vector<non_copyable, 3>::iterator i = c.emplace(c.cbegin(), 2, 3.5);
      CHECK(i == c.begin());
      CHECK(c.size() == 1);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      i = c.emplace(c.cend(), 3, 4.5);
      CHECK(i == c.end() - 1);
      CHECK(c.size() == 2);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      CHECK(c.back().geti() == 3);
      CHECK(c.back().getd() == 4.5);
      i = c.emplace(c.cbegin() + 1, 4, 6.5);
      CHECK(i == c.begin() + 1);
      CHECK(c.size() == 3);
      CHECK(c.front().geti() == 2);
      CHECK(c.front().getd() == 3.5);
      CHECK(c[1].geti() == 4);
      CHECK(c[1].getd() == 6.5);
      CHECK(c.back().geti() == 3);
      CHECK(c.back().getd() == 4.5);
    }
  }

  { // emplace_back
    vector<non_copyable, 2> c;
    c.emplace_back(2, 3.5);
    CHECK(c.size() == 1);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    c.emplace_back(3, 4.5);
    CHECK(c.size() == 2);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    CHECK(c.back().geti() == 3);
    CHECK(c.back().getd() == 4.5);
    CHECK_THROWS(c.emplace_back(2, 3.5), std::bad_alloc);
  }
  { // try_emplace_back
    vector<non_copyable, 2> c;
    CHECK((uintptr_t)c.begin() == (uintptr_t)c.try_emplace_back(2, 3.5));
    CHECK(c.size() == 1);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    CHECK((uintptr_t)(c.begin() + 1) == (uintptr_t)c.try_emplace_back(3, 4.5));
    CHECK(c.size() == 2);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    CHECK(c.back().geti() == 3);
    CHECK(c.back().getd() == 4.5);
    CHECK((uintptr_t) nullptr == (uintptr_t)c.try_emplace_back(2, 3.5));
  }
  { // unchecked_emplace_back
    vector<non_copyable, 2> c;
    CHECK((uintptr_t)c.begin() == (uintptr_t)&c.unchecked_emplace_back(2, 3.5));
    CHECK(c.size() == 1);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    CHECK((uintptr_t)(c.begin() + 1) ==
          (uintptr_t)&c.unchecked_emplace_back(3, 4.5));
    CHECK(c.size() == 2);
    CHECK(c.front().geti() == 2);
    CHECK(c.front().getd() == 3.5);
    CHECK(c.back().geti() == 3);
    CHECK(c.back().getd() == 4.5);
  }

  {   // emplace extra:
    { //
      vector<int, 4> v;
      v = {1, 2, 3};

      v.emplace(v.begin(), v.back());
      CHECK(v[0] == 3);
    }
    {
      vector<int, 4> v;
      v = {1, 2, 3};
      v.emplace(v.begin(), v.back());
      CHECK(v[0] == 3);
    }
  }

  { // erase
    {
      int a1[] = {1, 2, 3};
      vector<int, 4> l1(a1, a1 + 3);
      CHECK(l1.size() == 3);
      vector<int, 4>::const_iterator i = l1.begin();
      ++i;
      vector<int, 4>::iterator j = l1.erase(i);
      CHECK(l1.size() == 2);
      CHECK(std::distance(l1.begin(), l1.end()) == 2);
      CHECK(*j == 3);
      CHECK(*l1.begin() == 1);
      CHECK(*std::next(l1.begin()) == 3);
      j = l1.erase(j);
      CHECK(j == l1.end());
      CHECK(l1.size() == 1);
      CHECK(std::distance(l1.begin(), l1.end()) == 1);
      CHECK(*l1.begin() == 1);
      j = l1.erase(l1.begin());
      CHECK(j == l1.end());
      CHECK(l1.empty());
      CHECK(std::distance(l1.begin(), l1.end()) == 0);
    }
  }

  { // erase iter iter
    int a1[] = {1, 2, 3};
    using vec_t = vector<int, 5>;
    {
      vec_t l1(a1, a1 + 3);
      vec_t::iterator i = l1.erase(l1.cbegin(), l1.cbegin());
      CHECK(l1.size() == 3);
      CHECK(std::distance(l1.cbegin(), l1.cend()) == 3);
      CHECK(i == l1.begin());
    }
    {
      vec_t l1(a1, a1 + 3);
      vec_t::iterator i = l1.erase(l1.cbegin(), std::next(l1.cbegin()));
      CHECK(l1.size() == 2);
      CHECK(std::distance(l1.cbegin(), l1.cend()) == 2);
      CHECK(i == l1.begin());
      CHECK(l1 == vec_t(a1 + 1, a1 + 3));
    }
    {
      vec_t l1(a1, a1 + 3);
      vec_t::iterator i = l1.erase(l1.cbegin(), std::next(l1.cbegin(), 2));
      CHECK(l1.size() == 1);
      CHECK(std::distance(l1.cbegin(), l1.cend()) == 1);
      CHECK(i == l1.begin());
      CHECK(l1 == vec_t(a1 + 2, a1 + 3));
    }
    {
      vec_t l1(a1, a1 + 3);
      vec_t::iterator i = l1.erase(l1.cbegin(), std::next(l1.cbegin(), 3));
      CHECK(l1.empty());
      CHECK(std::distance(l1.cbegin(), l1.cend()) == 0);
      CHECK(i == l1.begin());
    }
    {
      vector<vec_t, 3> outer(2, vec_t(1));
      outer.erase(outer.begin(), outer.begin());
      CHECK(outer.size() == 2);
      CHECK(outer[0].size() == 1);
      CHECK(outer[1].size() == 1);
    }
  }

  { // insert init list
    {
      vector<int, 15> d(10, 1);
      vector<int, 15>::iterator i = d.insert(d.cbegin() + 2, {3, 4, 5, 6});
      CHECK(d.size() == 14);
      CHECK(i == d.begin() + 2);
      CHECK(d[0] == 1);
      CHECK(d[1] == 1);
      CHECK(d[2] == 3);
      CHECK(d[3] == 4);
      CHECK(d[4] == 5);
      CHECK(d[5] == 6);
      CHECK(d[6] == 1);
      CHECK(d[7] == 1);
      CHECK(d[8] == 1);
      CHECK(d[9] == 1);
      CHECK(d[10] == 1);
      CHECK(d[11] == 1);
      CHECK(d[12] == 1);
      CHECK(d[13] == 1);
      CHECK_THROWS(d.insert(d.cbegin(), {2, 3, 4, 5}), std::bad_alloc);
    }
  }

  { // insert iter iter
    {
      vector<int, 120> v(100);
      int a[] = {1, 2, 3, 4, 5};
      const std::size_t n = sizeof(a) / sizeof(a[0]);
      vector<int, 120>::iterator i =
          v.insert(v.cbegin() + 10, (a + 0), (a + n));
      CHECK(v.size() == 100 + n);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      for (std::size_t k = 0; k < n; ++j, ++k) {
        CHECK(v[j] == a[k]);
      }
      for (; j < 105; ++j) {
        CHECK(v[j] == 0);
      }
    }
    {
      vector<int, 120> v(100);
      size_t sz = v.size();
      int a[] = {1, 2, 3, 4, 5};
      const unsigned n = sizeof(a) / sizeof(a[0]);
      vector<int, 120>::iterator i =
          v.insert(v.cbegin() + 10, (a + 0), (a + n));
      CHECK(v.size() == sz + n);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      for (std::size_t k = 0; k < n; ++j, ++k) {
        CHECK(v[j] == a[k]);
      }
      for (; j < v.size(); ++j) {
        CHECK(v[j] == 0);
      }
    }
  }

  { // insert iter rvalue
    {
      vector<moint, 103> v(100);
      vector<moint, 103>::iterator i = v.insert(v.cbegin() + 10, moint(3));
      CHECK(v.size() == 101);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == moint());
      }
      CHECK(v[j] == moint(3));
      for (++j; j < 101; ++j) {
        CHECK(v[j] == moint());
      }
    }
  }

  { // insert iter size
    {
      vector<int, 130> v(100);
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
      CHECK(v.size() == 105);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      for (; j < 15; ++j) {
        CHECK(v[j] == 1);
      }
      for (++j; j < 105; ++j) {
        CHECK(v[j] == 0);
      }
    }
    {
      vector<int, 130> v(100);
      size_t sz = v.size();
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
      CHECK(v.size() == sz + 5);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      for (; j < 15; ++j) {
        CHECK(v[j] == 1);
      }
      for (++j; j < v.size(); ++j) {
        CHECK(v[j] == 0);
      }
    }
    {
      vector<int, 130> v(100);
      size_t sz = v.size();
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
      CHECK(v.size() == sz + 5);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      for (; j < 15; ++j) {
        CHECK(v[j] == 1);
      }
      for (++j; j < v.size(); ++j) {
        CHECK(v[j] == 0);
      }
    }
  }

  { // iter value:
    {
      vector<int, 130> v(100);
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 1);
      CHECK(v.size() == 101);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      CHECK(v[j] == 1);
      for (++j; j < 101; ++j) {
        CHECK(v[j] == 0);
      }
    }
    {
      vector<int, 130> v(100);
      size_t sz = v.size();
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 1);
      CHECK(v.size() == sz + 1);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      CHECK(v[j] == 1);
      for (++j; j < v.size(); ++j) {
        CHECK(v[j] == 0);
      }
    }
    {
      vector<int, 130> v(100);
      v.pop_back();
      v.pop_back(); // force no reallocation
      size_t sz = v.size();
      vector<int, 130>::iterator i = v.insert(v.cbegin() + 10, 1);
      CHECK(v.size() == sz + 1);
      CHECK(i == v.begin() + 10);
      std::size_t j;
      for (j = 0; j < 10; ++j) {
        CHECK(v[j] == 0);
      }
      CHECK(v[j] == 1);
      for (++j; j < v.size(); ++j) {
        CHECK(v[j] == 0);
      }
    }
  }

  { // push back move only
    {
      vector<moint, 6> c;
      c.push_back(moint(0));
      CHECK(c.size() == 1);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      c.push_back(moint(1));
      CHECK(c.size() == 2);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      c.push_back(moint(2));
      CHECK(c.size() == 3);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      c.push_back(moint(3));
      CHECK(c.size() == 4);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      c.push_back(moint(4));
      CHECK(c.size() == 5);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
    }
    {
      vector<moint, 6> c;
      auto i = c.try_push_back(moint(0));
      CHECK(c.size() == 1);
      CHECK((uintptr_t)(c.begin() + 0) == (uintptr_t)i);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = c.try_push_back(moint(1));
      CHECK(c.size() == 2);
      CHECK((uintptr_t)(c.begin() + 1) == (uintptr_t)i);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = c.try_push_back(moint(2));
      CHECK((uintptr_t)(c.begin() + 2) == (uintptr_t)i);
      CHECK(c.size() == 3);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = c.try_push_back(moint(3));
      CHECK((uintptr_t)(c.begin() + 3) == (uintptr_t)i);
      CHECK(c.size() == 4);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = c.try_push_back(moint(4));
      CHECK((uintptr_t)(c.begin() + 4) == (uintptr_t)i);
      CHECK(c.size() == 5);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
    }
    {
      vector<moint, 6> c;
      auto i = &c.unchecked_push_back(moint(0));
      CHECK(c.size() == 1);
      CHECK((uintptr_t)(c.begin() + 0) == (uintptr_t)i);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = &c.unchecked_push_back(moint(1));
      CHECK(c.size() == 2);
      CHECK((uintptr_t)(c.begin() + 1) == (uintptr_t)i);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = &c.unchecked_push_back(moint(2));
      CHECK((uintptr_t)(c.begin() + 2) == (uintptr_t)i);
      CHECK(c.size() == 3);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = &c.unchecked_push_back(moint(3));
      CHECK((uintptr_t)(c.begin() + 3) == (uintptr_t)i);
      CHECK(c.size() == 4);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
      i = &c.unchecked_push_back(moint(4));
      CHECK((uintptr_t)(c.begin() + 4) == (uintptr_t)i);
      CHECK(c.size() == 5);
      for (std::size_t j = 0; j < c.size(); ++j) {
        CHECK(c[j] == moint(j));
      }
    }
  }

  std::cerr << "TESTS PASSED" << std::endl;
  return 0;
}
