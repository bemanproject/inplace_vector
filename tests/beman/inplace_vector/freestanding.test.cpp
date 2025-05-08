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

}
} // namespace