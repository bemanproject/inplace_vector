#include <algorithm>
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

  EXPECT_EQ(IV(0), IV{});

  EXPECT_THROW(IV(IV::capacity() + 1), std::bad_alloc);

  constexpr auto mid_size = std::midpoint(0uz, IV::capacity());
  IV mid(mid_size);
  EXPECT_EQ(mid.size(), mid_size);
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV mid_correct;
    for (auto i = 0uz; i < mid_size; ++i)
      mid_correct.emplace_back();

    EXPECT_EQ(mid, mid_correct);
  }

  IV full((IV::capacity()));
  EXPECT_EQ(full.size(), IV::capacity());
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV full_correct;
    for (auto i = 0uz; i < full.size(); ++i)
      full_correct.emplace_back();

    EXPECT_EQ(full, full_correct);
  }
}

TYPED_TEST(Constructors, SizedValue) {
  // constexpr inplace_vector(size_type n, const T& value);
  // Preconditions: T is Cpp17CopyInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n copies of value.
  // Complexity: Linear in n.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  {
    T value{3519};
    IV device(0, value);
    EXPECT_EQ(device, IV{});

    EXPECT_THROW(IV(IV::capacity() + 1, value), std::bad_alloc);
  }

  if (IV::capacity() < 1)
    return;

  {
    T value{6810};
    IV device(1, value);
    EXPECT_EQ(device, IV{value});
  }

  {
    T value{8194};
    IV device(IV::capacity(), value);

    IV correct;
    for (auto i = 0uz; i < device.size(); ++i)
      correct.push_back(value);

    EXPECT_EQ(std::count(device.begin(), device.end(), value), IV::capacity());
  }
}

TYPED_TEST(Constructors, CopyIter) {
  // template<class InputIterator>
  //   constexpr inplace_vector(InputIterator first, InputIterator last);
  // Effects: Constructs an inplace_vector equal to the range [first, last).
  // Complexity: Linear in distance(first, last).

  using IV = TestFixture::IV;

  auto reference = this->unique();
  IV device(reference.begin(), reference.end());

  EXPECT_EQ(device, reference);
}

TYPED_TEST(Constructors, CopyRanges) {
  // template<container-compatible-range<T> R>
  // constexpr inplace_vector(from_range_t, R&& rg);
  // Effects: Constructs an inplace_vector with the elements of the range rg.
  // Complexity: Linear in ranges::distance(rg).

  using IV = TestFixture::IV;

  auto reference = this->unique();

  {
    IV device(beman::from_range, reference);
    EXPECT_EQ(device, reference);
  }

  if (IV::capacity() == 0)
    return;

  {
    IV device(beman::from_range, reference | std::ranges::views::take(1));
    EXPECT_EQ(device, IV{reference.front()});
  }

  {
    auto mid = std::midpoint(0uz, reference.size());
    IV device(beman::from_range, reference | std::ranges::views::take(mid));
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + mid));
  }
}

}; // namespace
