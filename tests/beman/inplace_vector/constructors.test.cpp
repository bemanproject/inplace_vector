#include <gtest/gtest.h>
#include <numeric>

#include "gtest_setup.hpp"

namespace {
// 23.3.14.2 Constructors [inplace.vector.cons]

template <typename Param> class Constructors : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Constructors, IVAllTypes);

TYPED_TEST(Constructors, SizedDefault) {
  // constexpr explicit inplace_vector(size_type n);
  // Preconditions: T is Cpp17DefaultInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n default-inserted elements.
  // Complexity : Linear in n.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV zero(0);
  EXPECT_EQ(zero, IV{});

  EXPECT_THROW(IV(IV::capacity() + 1), beman::bad_alloc);

  constexpr auto mid_size = std::midpoint(0ul, IV::capacity());
  IV mid(mid_size);
  EXPECT_EQ(mid.size(), mid_size);
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV mid_correct;
    for (auto i = 0ul; i < mid_size; ++i)
      mid_correct.emplace_back();

    EXPECT_EQ(mid, mid_correct);
  }

  IV full(IV::capacity());
  EXPECT_EQ(full.size(), IV::capacity());
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV full_correct;
    for (auto i = 0ul; i < full.size(); ++i)
      full_correct.emplace_back();

    EXPECT_EQ(full, full_correct);
  }
}

TYPED_TEST(Constructors, SizedValue) {
  // constexpr inplace_vector(size_type n, const T& value);
  // Preconditions: T is Cpp17CopyInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n copies of value.
  // Complexity: Linear in n.
  // TODO
  GTEST_SKIP();
}

TYPED_TEST(Constructors, CopyIter) {
  // template<class InputIterator>
  //   constexpr inplace_vector(InputIterator first, InputIterator last);
  // Effects: Constructs an inplace_vector equal to the range [first, last).
  // Complexity: Linear in distance(first, last).
  // TODO
  GTEST_SKIP();
}

TYPED_TEST(Constructors, CopyRanges) {
  // template<container-compatible-range<T> R>
  // constexpr inplace_vector(from_range_t, R&& rg);
  // Effects: Constructs an inplace_vector with the elements of the range rg.
  // Complexity: Linear in ranges::distance(rg).
  // TODO
  GTEST_SKIP();
}
}; // namespace
