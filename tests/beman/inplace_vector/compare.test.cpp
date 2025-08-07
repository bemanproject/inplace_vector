#include <beman/inplace_vector/inplace_vector.hpp>
#include <gtest/gtest.h>

#include <cmath>
#include <compare>

using namespace beman::inplace_vector;

template <class T>
concept has_threeway = requires(const T &t) {
  { t <=> t };
};

template <class T>
concept lessthan_comparable =
    beman::inplace_vector::details::lessthan_comparable<T>;

template <typename T> struct vec_list {
  T empty;
  T base;            // base
  T copy;            // identical to base
  T greater;         // greater number of elements
  T lesser;          // lesser number of elements
  T bigger;          // bigger value of the elements
  T smaller;         // smaller value of the elements
  T greater_smaller; // greater number of elements but smaller values
  T lesser_bigger;   // lesser number of elements but bigger values
};

template <typename T> static void runtests(vec_list<T> &list) {

  static_assert(std::three_way_comparable<T> || lessthan_comparable<T>);

  // if T::value_type is threewaycomparable with ordering X then T must also
  // be comparable with ordering X

  using VT = typename T::value_type;

  if constexpr (std::three_way_comparable<VT, std::strong_ordering>)
    static_assert(std::three_way_comparable<T, std::strong_ordering>);

  if constexpr (std::three_way_comparable<VT, std::weak_ordering>)
    static_assert(std::three_way_comparable<T, std::weak_ordering>);

  if constexpr (std::three_way_comparable<VT, std::partial_ordering>)
    static_assert(std::three_way_comparable<T, std::partial_ordering>);

  if constexpr (std::equality_comparable<VT>) {
    EXPECT_TRUE(list.empty == list.empty);
    EXPECT_TRUE(list.empty != list.base);
  }

  EXPECT_TRUE((list.base <=> list.copy) == 0);
  EXPECT_TRUE((list.base <=> list.greater) < 0);
  EXPECT_TRUE((list.base <=> list.lesser) > 0);

  EXPECT_TRUE((list.base <=> list.bigger) < 0);
  EXPECT_TRUE((list.base <=> list.smaller) > 0);

  EXPECT_TRUE((list.base <=> list.greater_smaller) < 0);
  EXPECT_TRUE((list.base <=> list.lesser_bigger) > 0);

  if constexpr (std::equality_comparable<VT>) {
    EXPECT_TRUE(list.base == list.copy);
    EXPECT_TRUE(list.base != list.greater);
    EXPECT_TRUE(list.base != list.lesser);
  }
  EXPECT_TRUE(list.base <= list.copy);
  EXPECT_TRUE(list.base >= list.copy);
  EXPECT_TRUE(list.base < list.greater);
  EXPECT_TRUE(list.base <= list.greater);
  EXPECT_TRUE(list.base > list.lesser);
  EXPECT_TRUE(list.base >= list.lesser);

  if constexpr (std::equality_comparable<VT>) {
    EXPECT_TRUE(list.copy == list.base);
    EXPECT_TRUE(list.copy != list.greater);
    EXPECT_TRUE(list.copy != list.lesser);
  }
  EXPECT_TRUE(list.copy <= list.base);
  EXPECT_TRUE(list.copy >= list.base);
  EXPECT_TRUE(list.greater > list.base);
  EXPECT_TRUE(list.greater >= list.base);
  EXPECT_TRUE(list.lesser < list.base);
  EXPECT_TRUE(list.lesser <= list.base);
};

TEST(Compare, threeway_int) {
  vec_list<inplace_vector<int, 4>> list{
      .empty{},
      .base{1, 2, 3},
      .copy{1, 2, 3},
      .greater{4, 5, 6},
      .lesser{0, 0, 0},
      .bigger{1, 2, 3, 0},
      .smaller{1, 2},
      .greater_smaller{2, 2},
      .lesser_bigger{0, 2, 3, 4},
  };

  runtests(list);
}

TEST(Compare, threeway_float) {
  vec_list<inplace_vector<float, 4>> list{
      .empty{},
      .base{1.0f, 2.0f, 3.0f},
      .copy{1.0f, 2.0f, 3.0f},
      .greater{4.0f, 5.0f, 6.0f},
      .lesser{0.0f, 0.0f, 0.0f},
      .bigger{1.0f, 2.0f, 3.0f, 0.0f},
      .smaller{1.0f, 2.0f},
      .greater_smaller{2.0f, 2.0f},
      .lesser_bigger{0.0f, 2.0f, 3.0f, 4.0f},
  };

  runtests(list);

  // compare unorderable values

  EXPECT_EQ(std::nanf("") <=> std::nanf(""), std::partial_ordering::unordered);
  EXPECT_FALSE(std::nanf("") == std::nanf(""));
  EXPECT_FALSE(std::nanf("") < std::nanf(""));
  EXPECT_FALSE(std::nanf("") > std::nanf(""));
  EXPECT_FALSE(std::nanf("") >= std::nanf(""));
  EXPECT_FALSE(std::nanf("") <= std::nanf(""));

  inplace_vector<float, 4> vnan{std::nanf("")};
  inplace_vector<float, 4> vnan2{std::nanf("")};

  EXPECT_EQ(vnan <=> vnan2, std::partial_ordering::unordered);
  EXPECT_FALSE(vnan == vnan2);
  EXPECT_FALSE(vnan < vnan2);
  EXPECT_FALSE(vnan > vnan2);
  EXPECT_FALSE(vnan >= vnan2);
  EXPECT_FALSE(vnan <= vnan2);
}

TEST(Compare, threeway_comparable1) {
  struct comparable1 {
    int a;
    int b;
    constexpr auto operator<=>(const comparable1 &) const = default;
  };

  static_assert(std::three_way_comparable<comparable1>);
  static_assert(has_threeway<comparable1>);
  static_assert(std::three_way_comparable<inplace_vector<comparable1, 4>>);
  static_assert(has_threeway<inplace_vector<comparable1, 4>>);

  vec_list<inplace_vector<comparable1, 4>> list{
      .empty{},
      .base{{1, 2}, {3, 4}},
      .copy{{1, 2}, {3, 4}},
      .greater{{5, 6}, {7, 8}},
      .lesser{{0, 0}, {0, 0}},
      .bigger{{1, 2}, {3, 4}, {5, 6}},
      .smaller{{1, 2}},
      .greater_smaller{{2, 2}, {3, 3}},
      .lesser_bigger{{0, 2}, {3, 3}, {4, 4}},
  };

  runtests(list);
}

TEST(Compare, threeway_comparable2) {

  struct comparable2 {
    int a;
    int b;
    constexpr bool operator==(const comparable2 &) const = delete;
    constexpr bool operator<(const comparable2 &other) const {
      return a < other.a || (a == other.a && b < other.b);
    };
  };

  static_assert(!std::three_way_comparable<comparable2>);
  static_assert(!has_threeway<comparable2>);
  static_assert(lessthan_comparable<comparable2>);
  static_assert(std::three_way_comparable<inplace_vector<comparable2, 4>>);
  static_assert(has_threeway<inplace_vector<comparable2, 4>>);

  vec_list<inplace_vector<comparable2, 4>> list{
      .empty{},
      .base{{1, 2}, {3, 4}},
      .copy{{1, 2}, {3, 4}},
      .greater{{5, 6}, {7, 8}},
      .lesser{{0, 0}, {0, 0}},
      .bigger{{1, 2}, {3, 4}, {5, 6}},
      .smaller{{1, 2}},
      .greater_smaller{{2, 2}, {3, 3}},
      .lesser_bigger{{0, 2}, {3, 3}, {4, 4}},
  };

  runtests(list);
}

TEST(Compare, threeway_strong_ordering) {

  struct weaktype {
    int a;
    constexpr std::strong_ordering
    operator<=>(const weaktype &other) const = default;
  };

  using T = weaktype;

  vec_list<inplace_vector<weaktype, 4>> list{
      .empty{},
      .base{T{1}, T{2}, T{3}},
      .copy{T{1}, T{2}, T{3}},
      .greater{T{4}, T{5}, T{6}},
      .lesser{T{0}, T{0}, T{0}},
      .bigger{T{1}, T{2}, T{3}, T{0}},
      .smaller{T{1}, T{2}},
      .greater_smaller{T{2}, T{2}},
      .lesser_bigger{T{0}, T{2}, T{3}, T{4}},
  };

  runtests(list);
}

TEST(Compare, threeway_weak_ordering) {

  struct weaktype {
    int a;
    constexpr std::weak_ordering
    operator<=>(const weaktype &other) const = default;
  };

  using T = weaktype;

  vec_list<inplace_vector<weaktype, 4>> list{
      .empty{},
      .base{T{1}, T{2}, T{3}},
      .copy{T{1}, T{2}, T{3}},
      .greater{T{4}, T{5}, T{6}},
      .lesser{T{0}, T{0}, T{0}},
      .bigger{T{1}, T{2}, T{3}, T{0}},
      .smaller{T{1}, T{2}},
      .greater_smaller{T{2}, T{2}},
      .lesser_bigger{T{0}, T{2}, T{3}, T{4}},
  };

  runtests(list);
}

TEST(Compare, threeway_partial_ordering) {

  struct custom {
    int a;
    constexpr auto operator<=>(const custom &other) const {
      if (a == -1 && other.a == -1)
        return std::partial_ordering::unordered;
      return std::partial_ordering(a <=> other.a);
    }

    constexpr bool operator==(const custom &other) const {
      if (a == -1 && other.a == -1)
        return false;
      return a == other.a;
    }
  };

  using T = custom;

  vec_list<inplace_vector<custom, 4>> list{
      .empty{},
      .base{T{1}, T{2}, T{3}},
      .copy{T{1}, T{2}, T{3}},
      .greater{T{4}, T{5}, T{6}},
      .lesser{T{0}, T{0}, T{0}},
      .bigger{T{1}, T{2}, T{3}, T{0}},
      .smaller{T{1}, T{2}},
      .greater_smaller{T{2}, T{2}},
      .lesser_bigger{T{0}, T{2}, T{3}, T{4}},
  };

  runtests(list);

  T t1{-1};
  T t2 = t1;
  EXPECT_EQ(t1 <=> t2, std::partial_ordering::unordered);

  inplace_vector<T, 4> v1{t1};
  inplace_vector<T, 4> v2{t2};

  EXPECT_EQ(v1 <=> v2, std::partial_ordering::unordered);
  EXPECT_FALSE(v1 == v2);
  EXPECT_FALSE(v1 < v2);
  EXPECT_FALSE(v1 > v2);
  EXPECT_FALSE(v1 >= v2);
  EXPECT_FALSE(v1 <= v2);
}

TEST(Compare, threeway_uncomparable) {

  struct uncomparable1 {
    int a;
  };

  static_assert(!std::three_way_comparable<uncomparable1>);
  static_assert(!has_threeway<uncomparable1>);
  static_assert(!std::three_way_comparable<inplace_vector<uncomparable1, 4>>);
  static_assert(!has_threeway<inplace_vector<uncomparable1, 4>>);

  struct uncomparable2 {
    int a;
    constexpr bool operator==(const uncomparable2 &) const = default;
  };

  static_assert(!std::three_way_comparable<uncomparable2>);
  static_assert(!has_threeway<uncomparable2>);
  static_assert(!std::three_way_comparable<inplace_vector<uncomparable2, 4>>);
  static_assert(!has_threeway<inplace_vector<uncomparable2, 4>>);

  struct uncomparable3 {
    int a;
    constexpr auto operator<=>(const uncomparable3 &) const = delete;
  };

  static_assert(!std::three_way_comparable<uncomparable3>);
  static_assert(!has_threeway<uncomparable3>);
  static_assert(!std::three_way_comparable<inplace_vector<uncomparable3, 4>>);
  static_assert(!has_threeway<inplace_vector<uncomparable3, 4>>);
}
