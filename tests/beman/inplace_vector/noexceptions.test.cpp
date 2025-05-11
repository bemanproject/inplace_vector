#include <array>

bool abort_called = false;
#define BEMAN_IV_THROW(x) abort_called = true;

#include "gtest_setup.hpp"

namespace {
template <typename Param> class NoExceptions : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(NoExceptions, IVAllTypes);

TYPED_TEST(NoExceptions, NonThrowing) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;
  abort_called = false;

  device.assign(reference.begin(), reference.end());
  EXPECT_EQ(device, reference);
  device.clear();

  EXPECT_EQ(abort_called, false);
}

TYPED_TEST(NoExceptions, Throwing) {

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();
  auto range = std::array<T, IV::capacity() + 1>{};

  IV device;
  device.assign(reference.begin(), reference.end());

  abort_called = false;
  device.emplace_back(T{});
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.resize(IV::capacity() + 1);
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.resize(IV::capacity() + 1, T{});
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.reserve(IV::capacity() + 1);
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.append_range(range);
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.insert(device.end(), range.begin(), range.end());
  EXPECT_EQ(abort_called, true);

  abort_called = false;
  device.at(IV::capacity() + 1);
  EXPECT_EQ(abort_called, true);
}
} // namespace
