#include <beman/inplace_vector/inplace_vector.hpp>
#include <gtest/gtest.h>

#include <compare>

using namespace beman;

template <class T>
concept has_threeway = requires(const T &t) {
  { t <=> t };
};

TEST(Compare, threeway_int) {
  using T = inplace_vector<int, 4>;
  T empty{};
  T base{1, 2, 3};
  T copy = base;

  T greater{4, 5, 6};
  T lesser{0, 0, 0};

  T bigger{1, 2, 3, 0};
  T smaller{1, 2};

  T greater_smaller{2, 2};
  T lesser_bigger{0, 2, 3, 0};

  EXPECT_TRUE((base <=> copy) == 0);
  EXPECT_TRUE((base <=> greater) < 0);
  EXPECT_TRUE((base <=> lesser) > 0);

  EXPECT_TRUE((base <=> bigger) < 0);
  EXPECT_TRUE((base <=> smaller) > 0);

  EXPECT_TRUE((base <=> greater_smaller) < 0);
  EXPECT_TRUE((base <=> lesser_bigger) > 0);
}

TEST(COmpare, threeway_float) {
  using T = inplace_vector<float, 4>;
  T empty{};
  T base{1.0f, 2.0f, 3.0f};
  T copy = base;

  T greater{4.0f, 5.0f, 6.0f};
  T lesser{0.0f, 0.0f, 0.0f};

  T bigger{1.0f, 2.0f, 3.0f, 0.0f};
  T smaller{1.0f, 2.0f};

  T greater_smaller{2.0f, 2.0f};
  T lesser_bigger{0.0f, 2.0f, 3.0f, 0.0f};

  EXPECT_TRUE((base <=> copy) == 0);

  EXPECT_TRUE((base <=> greater) < 0);
  EXPECT_TRUE((base <=> lesser) > 0);

  EXPECT_TRUE((base <=> bigger) < 0);
  EXPECT_TRUE((base <=> smaller) > 0);

  EXPECT_TRUE((base <=> greater_smaller) < 0);
  EXPECT_TRUE((base <=> lesser_bigger) > 0);
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

  using T = inplace_vector<comparable1, 4>;

  T empty{};

  T base{{1, 2}, {3, 4}};
  T copy = base;
  T greater{{5, 6}, {7, 8}};
  T lesser{{0, 0}, {0, 0}};

  T bigger{{1, 2}, {3, 4}, {5, 6}};
  T smaller{{1, 2}};

  T greater_smaller{{2, 2}, {3, 3}};
  T lesser_bigger{{0, 2}, {3, 3}, {0, 0}};

  EXPECT_TRUE((empty <=> empty) == 0);
  EXPECT_TRUE((base <=> copy) == 0);

  EXPECT_TRUE((base <=> greater) < 0);
  EXPECT_TRUE((base <=> lesser) > 0);

  EXPECT_TRUE((base <=> bigger) < 0);
  EXPECT_TRUE((base <=> smaller) > 0);

  EXPECT_TRUE((base <=> greater_smaller) < 0);
  EXPECT_TRUE((base <=> lesser_bigger) > 0);
}

TEST(Compare, threeway_comparable2) {

  struct comparable2 {
    int a;
    int b;
    constexpr bool operator==(const comparable2 &) const = default;
    constexpr bool operator<(const comparable2 &other) const {
      return a < other.a || (a == other.a && b < other.b);
    };
  };

  static_assert(!std::three_way_comparable<comparable2>);
  static_assert(!has_threeway<comparable2>);
  static_assert(std::three_way_comparable<inplace_vector<comparable2, 4>>);
  static_assert(has_threeway<inplace_vector<comparable2, 4>>);

  using T = inplace_vector<comparable2, 4>;

  T empty{};
  T base{{1, 2}, {3, 4}};
  T copy = base;
  T greater{{5, 6}, {7, 8}};
  T lesser{{0, 0}, {0, 0}};

  T bigger{{1, 2}, {3, 4}, {5, 6}};
  T smaller{{1, 2}};

  T greater_smaller{{2, 2}, {3, 3}};
  T lesser_bigger{{0, 2}, {3, 3}, {0, 0}};

  EXPECT_TRUE((empty <=> empty) == 0);
  EXPECT_TRUE((base <=> copy) == 0);

  EXPECT_TRUE((base <=> greater) < 0);
  EXPECT_TRUE((base <=> lesser) > 0);

  EXPECT_TRUE((base <=> bigger) < 0);
  EXPECT_TRUE((base <=> smaller) > 0);

  EXPECT_TRUE((base <=> greater_smaller) < 0);
  EXPECT_TRUE((base <=> lesser_bigger) > 0);
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
    constexpr auto operator<=>(const uncomparable3 &) const {
      return std::partial_ordering::unordered;
    }
  };

  static_assert(!std::three_way_comparable<uncomparable3>);
  static_assert(has_threeway<uncomparable3>); // has op but returns unordered
  static_assert(!std::three_way_comparable<inplace_vector<uncomparable3, 4>>);
  static_assert(!has_threeway<inplace_vector<uncomparable3, 4>>);
}
