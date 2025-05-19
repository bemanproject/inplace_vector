#include <array>

#include <cstdlib>
#define BEMAN_IV_THROW_OR_ABORT(x) abort()

#include "gtest_setup.hpp"

namespace {

// clang-format off
using exceptionTestTypes = ::testing::Types<
    TestParam<Trivial, 0>, TestParam<Trivial, 42>,
    TestParam<NonTrivial, 0>, TestParam<NonTrivial, 42>>;
// clang-format on

template <typename Param> class NoExceptions : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(NoExceptions, exceptionTestTypes);

TYPED_TEST(NoExceptions, NonThrowing) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;

  device.assign(reference.begin(), reference.end());
  EXPECT_EQ(device, reference);

  EXPECT_EQ(device.try_emplace_back(T{}), nullptr);
  EXPECT_EQ(device.try_push_back(T{}), nullptr);
  auto range = std::array<T, 1>{};
  EXPECT_EQ(device.try_append_range(range), range.begin());

  IV sanitycheck = this->unique();
  EXPECT_EQ(sanitycheck.size(), IV::capacity());
}

TYPED_TEST(NoExceptions, emplace_back) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.emplace_back(T{});
      },
      ".*");
}

TYPED_TEST(NoExceptions, resize) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

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
}

TYPED_TEST(NoExceptions, reserve) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.reserve(IV::capacity() + 1);
      },
      ".*");
}

TYPED_TEST(NoExceptions, append_range) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

  EXPECT_DEATH(
      {
        IV device{};
        device.append_range(std::array<T, IV::capacity() + 1>{});
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        device.append_range(std::array<T, 1>{});
      },
      ".*");

  // TODO consider adding test for append_range of unsized range
}

TYPED_TEST(NoExceptions, insert) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

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
}

TYPED_TEST(NoExceptions, at) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  GTEST_FLAG_SET(death_test_style, "fast");

  EXPECT_DEATH(
      {
        IV device = this->unique();
        auto e = device.at(IV::capacity());
      },
      ".*");

  EXPECT_DEATH(
      {
        IV device;
        const auto e = device.at(IV::capacity());
      },
      ".*");
}
} // namespace
