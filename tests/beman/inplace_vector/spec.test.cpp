#include "beman/inplace_vector/inplace_vector.hpp"
#include "gtest/gtest.h"
#include <gtest/gtest.h>
#include <type_traits>

using namespace beman;

// 23.3.14.1 Overview [inplace.vector.overview]
// 1 An inplace_vector is a contiguous container. Its capacity is fixed and its
// elements are stored within the inplace_vector object itself.
//
// This is not testable.

// 2 An inplace_vector meets all of the requirements of a container (23.2.2.2),
// of a reversible container (23.2.2.3), of a contiguous container, and of a
// sequence container, including most of the optional sequence container
// requirements (23.2.4). The exceptions are the push_front, prepend_range,
// pop_front, and emplace_front member functions, which are not provided.
// Descriptions are provided here only for operations on inplace_- vector that
// are not described in one of these tables or for operations where there is
// additional semantic information.
//
// TODO: Test container & reversible container & optional sequence container
// req.

// 3 For any N, inplace_vector<T, N>::iterator and inplace_vector<T,
// N>::const_iterator meet the constexpr iterator requirements.
//
// TODO: Test this.

// 4 For any N > 0, if is_trivial_v<T> is false, then no inplace_vector<T, N>
// member functions are usable in constant expressions.
//
// This would be tested in subsequent method tests.

// 5 Any member function of inplace_vector<T, N> that would cause the size to
// exceed N throws an exception of type bad_alloc.
//
// TODO: Test this.

// 6 Let IV denote a specialization of inplace_vector<T, N>. If N is zero, then
// IV is both trivial and empty. Otherwise:
// (6.1) — If is_trivially_copy_constructible_v<T> is true, then IV has a
// trivial copy constructor
// (6.2) — If is_trivially_move_constructible_v<T> is true, then IV has a
// trivial move constructor.
// (6.3) — If is_trivially_destructible_v<T> is true, then:
// (6.3.1) — IV has a trivial destructor.
// (6.3.2) — If is_trivially_copy_constructible_v<T> &&
// is_trivially_copy_assignable_v<T> is true, then IV has a trivial copy
// assignment operator.
// (6.3.3) — If is_trivially_move_constructible_v<T> &&
// is_trivially_move_assignable_v<T> is true, then IV has a trivial move
// assignment operator.

TEST(Overview, Section6) {
  // Let IV denote a specialization of inplace_vector<T, N>.
  {
    SCOPED_TRACE("If N is zero, then IV is both trivial and empty.");

    struct T {
      T() = delete;
      ~T() {}

      T(T &) = delete;
      T &operator=(T &) = delete;

      T(T &&) = delete;
      T &operator=(T &&) = delete;
    };
    static_assert(!std::is_trivial_v<T>);
    using IV = inplace_vector<T, 0>;
    EXPECT_TRUE(std::is_trivial_v<IV> && std::is_empty_v<IV>);
  }
  {
    SCOPED_TRACE(
        "(6.1) — If is_trivially_copy_constructible_v<T> is true, then "
        "IV has a trivial copy constructor");

    struct T {
      int a = 0;
      int b;

      T(T const &) = default;

      T() = delete;
      T(T &&) = delete;
      // TODO: This type is also trivially destructible
      // ~T() {}
    };
    static_assert(std::is_trivially_copy_constructible_v<T>);
    using IV = inplace_vector<T, 5>;
    EXPECT_TRUE(std::is_trivially_copy_constructible_v<IV>);
  }
  {
    SCOPED_TRACE(
        "(6.2) — If is_trivially_move_constructible_v<T> is true, then IV "
        "has a trivial move constructor.");

    struct T {
      int a = 0;
      int b;

      T(T &&) = default;

      T() = delete;
      T(T const &) = default;
      // TODO: This type is also trivially destructible
      // ~T() {}
    };
    static_assert(std::is_trivially_move_constructible_v<T>);
    using IV = inplace_vector<T, 5>;
    EXPECT_TRUE(std::is_trivially_move_constructible_v<IV>);
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.1) — IV has a trivial destructor.");

    struct T {
      int a = 0;
      int b;

      ~T() = default;

      T() = delete;
      T(T const &) = delete;
      T(T &&) = delete;
    };
    static_assert(std::is_trivially_destructible_v<T>);
    using IV = inplace_vector<T, 5>;
    EXPECT_TRUE(std::is_trivially_move_constructible_v<IV>);
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.2) — If is_trivially_copy_constructible_v<T> && "
                 "is_trivially_copy_assignable_v<T> is true, then IV has a "
                 "trivial copy assignment operator.");

    struct T {
      int a = 0;
      int b;

      ~T() = default;
      T(T const &) = default;
      T &operator=(T const &) = default;

      T() = delete;
      T(T &&) = delete;
    };
    static_assert(std::is_trivially_destructible_v<T>);
    static_assert(std::is_trivially_copy_constructible_v<T>);
    static_assert(std::is_trivially_copy_assignable_v<T>);

    using IV = inplace_vector<T, 5>;
    EXPECT_TRUE(std::is_trivially_copy_assignable_v<IV>);
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.3) — If is_trivially_move_constructible_v<T> && "
                 "is_trivially_move_assignable_v<T> is true, then IV has a "
                 "trivial move assignment operator.");

    struct T {
      int a = 0;
      int b;

      ~T() = default;
      T(T &&) = default;
      T &operator=(T &&) = default;

      T() = delete;
      T(T const &) = delete;
    };
    static_assert(std::is_trivially_destructible_v<T>);
    static_assert(std::is_trivially_move_constructible_v<T>);
    static_assert(std::is_trivially_move_assignable_v<T>);

    using IV = inplace_vector<T, 5>;
    EXPECT_TRUE(std::is_trivially_move_assignable_v<IV>);
  }
}

// 23.3.14.2 Constructors [inplace.vector.cons]

TEST(Constructors, SizedDefault) {
  // constexpr explicit inplace_vector(size_type n);
  // Preconditions: T is Cpp17DefaultInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n default-inserted elements.
  // Complexity : Linear in n.
  // TODO
  GTEST_SKIP();
}

TEST(Constructors, SizedValue) {
  // constexpr inplace_vector(size_type n, const T& value);
  // Preconditions: T is Cpp17CopyInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n copies of value.
  // Complexity: Linear in n.
  // TODO
  GTEST_SKIP();
}

TEST(Constructors, CopyIter) {
  // template<class InputIterator>
  //   constexpr inplace_vector(InputIterator first, InputIterator last);
  // Effects: Constructs an inplace_vector equal to the range [first, last).
  // Complexity: Linear in distance(first, last).
  // TODO
  GTEST_SKIP();
}

TEST(Constructors, CopyRanges) {
  // template<container-compatible-range<T> R>
  // constexpr inplace_vector(from_range_t, R&& rg);
  // Effects: Constructs an inplace_vector with the elements of the range rg.
  // Complexity: Linear in ranges::distance(rg).
  // TODO
  GTEST_SKIP();
}

// 23.3.14.3 Size and capacity [inplace.vector.capacity]
// TODO

// 23.3.14.4 Data [inplace.vector.data]
// TODO

// 23.3.14.5 Modifiers [inplace.vector.modifiers]
// TODO

// 23.3.14.6 Erasure [inplace.vector.erasure]
// TODO
