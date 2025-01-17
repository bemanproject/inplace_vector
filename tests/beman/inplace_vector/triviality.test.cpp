#include <gtest/gtest.h>

#include "gtest_setup.hpp"

namespace {

// 6 Let IV denote a specialization of inplace_vector<T, N>.
// If N is zero, then IV is trivially copyable and empty, and
// std::is_trivially_default_constructible_v<IV> is true. Otherwise:
//
// (6.1) — If is_trivially_copy_constructible_v<T> is true, then IV has a
// trivial copy constructor
//
// (6.2) — If is_trivially_move_constructible_v<T> is true, then
// IV has a trivial move constructor.
//
// (6.3) — If is_trivially_destructible_v<T> is true, then:
// (6.3.1) — IV has a trivial destructor.
// (6.3.2) — If is_trivially_copy_constructible_v<T> &&
// is_trivially_copy_assignable_v<T> is true, then IV has a trivial copy
// assignment operator.
// (6.3.3) — If is_trivially_move_constructible_v<T> &&
// is_trivially_move_assignable_v<T> is true, then IV has a trivial move
// assignment operator.

template <typename Param> class Triviality : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Triviality, IVAllTypes);

TYPED_TEST(Triviality, ZeroSized) {
  // 6 Let IV denote a specialization of inplace_vector<T, N>.
  // If N is zero, then IV is trivially copyable and empty, and
  // std::is_trivially_default_constructible_v<IV> is true.

  constexpr auto N = TestFixture::N;
  using IV = TestFixture::IV;

  if constexpr (N == 0) {
    EXPECT_TRUE(std::is_trivially_copyable_v<IV>);
    EXPECT_TRUE(std::is_empty_v<IV>);
    EXPECT_TRUE(std::is_trivially_default_constructible_v<IV>);
  }
}

TYPED_TEST(Triviality, TrivialCopyConstructible) {
  // (6.1) — If is_trivially_copy_constructible_v<T> is true, then IV has a
  // trivial copy constructor

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  if constexpr (std::is_trivially_copy_constructible_v<T>) {
    EXPECT_TRUE(std::is_trivially_copy_constructible_v<IV>);
  }
}

TYPED_TEST(Triviality, TrivialMoveConstructible) {
  // (6.2) — If is_trivially_move_constructible_v<T> is true, then IV has a
  // trivial move constructor.

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  if constexpr (std::is_trivially_move_constructible_v<T>) {
    EXPECT_TRUE(std::is_trivially_move_constructible_v<IV>);
  }
}

TYPED_TEST(Triviality, TrivialDestructor) {
  // (6.3) — If is_trivially_destructible_v<T> is true, then:
  // (6.3.1) — IV has a trivial destructor.

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  if constexpr (std::is_trivially_destructible_v<T>) {
    EXPECT_TRUE(std::is_trivially_destructible_v<IV>);
  }
}

TYPED_TEST(Triviality, TrivialCopyAssignment) {
  // (6.3) — If is_trivially_destructible_v<T> is true, then:
  // (6.3.2) — If is_trivially_copy_constructible_v<T> &&
  // is_trivially_copy_assignable_v<T> is true, then IV has a trivial copy
  // assignment operator.

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  if constexpr (std::is_trivially_destructible_v<T> &&
                std::is_trivially_copy_constructible_v<T> &&
                std::is_trivially_copy_assignable_v<T>) {
    EXPECT_TRUE(std::is_trivially_copy_assignable_v<IV>);
  }
}

TYPED_TEST(Triviality, TrivialMoveAssignment) {
  // (6.3) — If is_trivially_destructible_v<T> is true, then:
  // (6.3.3) — If is_trivially_move_constructible_v<T> &&
  // is_trivially_move_assignable_v<T> is true, then IV has a trivial move
  // assignment operator.

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  if constexpr (std::is_trivially_destructible_v<T> &&
                std::is_trivially_move_constructible_v<T> &&
                std::is_trivially_move_assignable_v<T>) {
    EXPECT_TRUE(std::is_trivially_move_assignable_v<IV>);
  }
}

}; // namespace
