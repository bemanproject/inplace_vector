#include <array>

#include "gtest_setup.hpp"

namespace {

template <typename Param> class NoExceptions : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(NoExceptions, IVAllTypes);

TYPED_TEST(NoExceptions, NonThrowing) {

  using IV = TestFixture::IV;

  const auto reference = this->unique();

  IV device;

  device.assign(reference.begin(), reference.end());
  EXPECT_EQ(device, reference);
  device.clear();
}

TYPED_TEST(NoExceptions, IVAllTypes) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV sanitycheck = this->unique();
  EXPECT_EQ(sanitycheck.size(), IV::capacity());

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.emplace_back(T{});
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.resize(IV::capacity() + 1);
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.resize(IV::capacity() + 1, T{});
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.reserve(IV::capacity() + 1);
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device{};
        device.append_range(std::array<T, IV::capacity() + 2>{});
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.append_range(std::array<T, 2>{});
      },
      ".*");

  // TODO consider adding test for append_range of unsized range

  if constexpr (IV::capacity() > 0) {
    EXPECT_DEATH(
        {
          IV device = this->unique();
          // test macro fails to compile if we use std::array<T, N> without
          // parenthesis ()
          auto range = (std::array<T, IV::capacity()>{});
          device.insert(device.end(), range.begin(), range.end());
        },
        ".*");
  }

  EXPECT_DEATH(
      {
        IV device{};
        // test macro fails to compile if we use std::array<T, N> without
        // parenthesis ()
        auto range = (std::array<T, IV::capacity() + 1>{});
        device.insert(device.end(), range.begin(), range.end());
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        auto e = device.at(IV::capacity() + 1);
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device;
        const auto e = device.at(IV::capacity() + 1);
      },
      ".*");
}
} // namespace
