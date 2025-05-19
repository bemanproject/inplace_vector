#include <gtest/gtest.h>
#include <numeric>

#include "gtest_setup.hpp"

namespace {
// 23.3.14.3 Size and capacity [inplace.vector.capacity]

template <typename Param> class SizeNCapacity : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(SizeNCapacity, IVAllTypes);

TYPED_TEST(SizeNCapacity, Capacity) {
  // static constexpr size_type capacity() noexcept;
  // static constexpr size_type max_size() noexcept;
  // Returns: N.

  using IV = TestFixture::IV;
  constexpr auto N = TestFixture::N;

  EXPECT_EQ(IV::capacity(), N);
  IV device;
  EXPECT_EQ(device.max_size(), N);
}

TYPED_TEST(SizeNCapacity, ResizeDown) {
  // constexpr void resize(size_type sz);
  // Preconditions: T is Cpp17DefaultInsertable into inplace_vector.
  // Effects: If sz < size(), erases the last size() - sz elements from the
  // sequence. Otherwise, appends sz - size() default-inserted elements to the
  // sequence. Remarks: If an exception is thrown, there are no effects on
  // *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();

  auto mid_size = std::midpoint(0ul, device.size());
  device.resize(mid_size);
  EXPECT_EQ(device, IV(device.begin(), device.begin() + mid_size));

  device.resize(0);
  EXPECT_EQ(device, IV{});
}

TYPED_TEST(SizeNCapacity, ResizeUp) {
  // constexpr void resize(size_type sz);
  // Preconditions: T is Cpp17DefaultInsertable into inplace_vector.
  // Effects: If sz < size(), erases the last size() - sz elements from the
  // sequence. Otherwise, appends sz - size() default-inserted elements to the
  // sequence. Remarks: If an exception is thrown, there are no effects on
  // *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;

  SAFE_EXPECT_THROW(device.resize(device.capacity() + 1), std::bad_alloc);
  EXPECT_EQ(device, IV{});

  if (device.capacity() == 0)
    return;

  // Trying to pollute device[0]
  device.push_back(T{255});
  device.pop_back();
  EXPECT_TRUE(device.empty());

  device.resize(1);
  EXPECT_EQ(device.size(), 1);
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>)
    EXPECT_EQ(device, IV{T{0}});

  T front{341};
  device[0] = front;
  device.resize(device.capacity());
  EXPECT_EQ(device[0], front);

  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {
    IV expected(device.capacity(), T{0});
    expected[0] = front;
    EXPECT_EQ(device, expected);
  }

  IV before_resize(device);
  SAFE_EXPECT_THROW(device.resize(device.capacity() + 1), std::bad_alloc);
  EXPECT_EQ(device, before_resize);
}

// 23.3.14.4 Data [inplace.vector.data]

template <typename Param> class Data : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Data, IVAllTypes);

TYPED_TEST(Data, Test) {
  // constexpr       T* data()       noexcept;
  // constexpr const T* data() const noexcept;
  //
  // Returns: A pointer such that [data(), data() + size()) is a valid range.
  // For a non-empty inplace_vector, data() == addressof(front()) is true.
  // Complexity: Constant time.

  auto device = this->unique();
  device.data();
  if (device.capacity() == 0)
    return;

  EXPECT_EQ(device.data(), std::addressof(device.front()));
}

}; // namespace
