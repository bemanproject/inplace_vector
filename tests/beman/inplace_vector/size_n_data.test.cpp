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

// These functions are marked as freestand delete.
// See size_n_data_fs.test.cpp.
//
// constexpr void resize(size_type sz);

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
