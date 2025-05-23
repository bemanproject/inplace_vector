#include <gtest/gtest.h>
#include <type_traits>

#include "gtest_setup.hpp"

namespace {

template <typename Param> class Freestanding : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Freestanding, IVAllTypes);

TYPED_TEST(Freestanding, alltypes) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;
  using size_type = IV::size_type;

  IV vec;
  for (size_type i = 0; i < IV::capacity(); ++i) {
    EXPECT_NE(vec.try_push_back(T{}), nullptr);
  }
  EXPECT_EQ(vec.try_push_back(T{}), nullptr);
  EXPECT_EQ(vec.size(), IV::capacity());
  vec.clear();
  EXPECT_EQ(vec.size(), 0);

  static_assert(requires(IV vec, T t) { vec.try_push_back(t); });
  static_assert(requires(IV vec, T t) { vec.try_emplace_back(t); });

  static_assert(!requires(IV vec, T t) { vec.push_back(t); });
  static_assert(!requires(IV vec, T t) { vec.emplace_back(t); });
  static_assert(!requires(IV vec, size_type t) { vec.reserve(t); });

  // TODO make sure all of the freestanding-delete functions are deleted
}
} // namespace
