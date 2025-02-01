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

  constexpr auto mid_size = std::midpoint(0ul, IV::capacity());
  IV mid(mid_size);
  EXPECT_EQ(mid.size(), mid_size);
  if constexpr (std::is_scalar_v<T> || std::is_aggregate_v<T> ||
                !std::is_trivially_default_constructible_v<T>) {

    // all elements are value-initialized
    EXPECT_EQ(std::ranges::count(mid, T{}), mid.size());
  }

  IV full((IV::capacity()));
  EXPECT_EQ(full.size(), IV::capacity());
  if constexpr (std::is_scalar_v<T> || std::is_aggregate_v<T> ||
                !std::is_trivially_default_constructible_v<T>) {

    // all elements are value-initialized
    EXPECT_EQ(std::ranges::count(full, T{}), full.size());
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

  if constexpr (IV::capacity() < 1u)
    return;

  {
    T value{6810};
    IV device(1, value);
    EXPECT_EQ(device, IV{value});
  }

  {
    T value{8194};
    IV device(IV::capacity(), value);

    EXPECT_EQ(std::ranges::count(device, value), device.size());
  }
}

TYPED_TEST(Constructors, CopyIter) {
  // template<class InputIterator>
  //   constexpr inplace_vector(InputIterator first, InputIterator last);
  // Effects: Constructs an inplace_vector equal to the range [first, last).
  // Complexity: Linear in distance(first, last).

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  using InputIterator = TestFixture::InputIterator;

  for (std::size_t n : {std::size_t(0), IV::max_size() / 2u, IV::max_size()}) {
    // InputIterator
    InputIterator::num_deref = 0u;
    IV a(InputIterator{}, InputIterator{static_cast<int>(n)});
    EXPECT_EQ(a.size(), n);
    for (int i = 0; i < static_cast<int>(n); ++i) {
      EXPECT_EQ(a[i], T{i});
    }
    // Only single-pass through input range
    EXPECT_EQ(InputIterator::num_deref, n);

    // RandomAccessIterator
    IV b(a.begin(), a.end());
    EXPECT_EQ(b, a);
  }
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
    auto mid = std::midpoint(0ul, reference.size());
    IV device(beman::from_range, reference | std::ranges::views::take(mid));
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + mid));
  }
}

}; // namespace
