#include "beman/inplace_vector/inplace_vector.hpp"
#include "gtest/gtest.h"
#include <algorithm>
#include <array>
#include <concepts>
#include <iterator>
#include <numeric>
#include <ranges>
#include <type_traits>

// TODO: Not tested in this test suite:
// constexpr, emplace (ensure only one copy made), exception safety on mutation

namespace {

// We run the tests on various element types with the help of GoogleTest's
// typed tests. Which types shall we use?
// We know from the specification of inplace_vector that there are separate
// implementation branches for trivial and non-trivial types (due to constexpr
// support). Furthermore, we have the requirements that the triviality of
// inplace_vector's SMFs depends on the triviality of the element type T:
// 1. If T is trivially copy constructible, then inplace_vector<T, N> is
// trivially copy constructible.
// 2. If T is trivially move constructible, then inplace_vector<T, N> is
// trivially move constructible.
// 3. If T is trivially destructible, then inplace_vector<T, N> is trivially
// destructible.
// 4. If T is trivially destructible, trivially copy constructible and trivially
// copy assignable, then inplace_vector<T, N> is trivially copy assignable.
// 5. If T is trivially destructible, trivially move constructible and trivially
// move assignable, then inplace_vector<T, N> is trivially move assignable.
//
// In cases 1, 2, 3, where there is only dependence on _one_ property of T, we
// have to run tests with at least three different types:
// - A trivial type, in order to cover the "trivial" implementation branch. For
// such a type, the condition in 1, 2, or 3 is always fulfilled implicitly.
// - A non-trivial type, in order to cover the "non-trivial" implementation
// branch, and where the condition in 1, 2, or 3 is fulfilled; e.g. a type which
// is trivially copy constructible, but not trivial. A type where only the
// default constructor is non-trivial can serve here and can be reused for all
// similar cases.
// - A type where the condition in 1, 2, or 3 is not fulfilled, this implies
// that the type is non-trivial. A type which has no trivial SMFs at all can
// serve here for simplicity and reuse.
//
// The cases 4 and 5 depend on three properties of T each. This means, for full
// coverage of all combinations we need 2^3 + 1 types (the + 1 is for the
// "trivial" implementation branch) in theory. In fact, there are fewer
// combinations, since it is not possible to create a type which is not
// trivially destructible and trivially copy or move constructible at the same
// time. The combination where T is non-trivial, but all three properties are
// fulfilled, can be covered by the type with a non-trivial default constructor,
// which was already mentioned above. The combination where none of the
// properties is fulfilled is covered by the type with no trivial SMF at all,
// also reused from above. For the rest of the combinations, we need to create
// additional types with the appropriate properties.
//
// The following table provides an overview and proof that all combinations for
// the SMFs are really covered. All types have the same interface, this
// makes it possible to write generic tests. Once the types and the test
// suite(s) are set up, we do not need to mention the types again, so the actual
// test cases will look quite clean. All tests are run with all types, even if
// not strictly necessary by the analysis above. This does not harm (except run
// time for the tests) and is an additional safe guard against implementation
// errors.
// clang-format off
/*
        | (Trivially default | Trivially    | Trivially copy | Trivially move | Trivially copy | Trivially move | Trivial* | Type
        | constructible)     | destructible | constructible  | constructible	 | assignable	   | assignable 	  |          |
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
Copy 	  | (YES)              | -            | YES            | -              | -              | -              | YES      | Trivial
c'tor   | (NO)               | -            | YES            | -              | -              | -              | NO       | NonTriviallyDefaultConstructible
	      | -                  | -            | NO             | -              | -              | -              | NO       | NonTrivial
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
Move    | (YES)              | -            | -              | YES            | -              | -              | YES      | Trivial
c'tor		| (NO)               | -            | -              | YES            | -              | -              | NO       | NonTriviallyDefaultConstructible
	      | -                  | -            | -              | NO             | -              | -              | NO       | NonTrivial
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
     	  | (YES)              | YES          | -              | -              | -              | -              | YES      | Trivial
D'tor   | (NO)               | YES          | -              | -              | -              | -              | NO       | NonTriviallyDefaultConstructible
	      | -                  | NO           | -              | -              | -              | -              | NO       | NonTrivial
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
     	  | (YES)              | YES          | YES            | -              | YES            | -              | YES      | Trivial
        | (NO)               | YES          | YES            | -              | YES            | -              | NO       | NonTriviallyDefaultConstructible
Copy    | -                  | YES          | YES            | -              | NO             |                | NO       | NonTriviallyCopyAssignable
assign- | -                  | YES          | NO             | -              | YES            |                | NO       | NonTriviallyCopyConstructible
meant   | -                  | YES          | NO             | -              | NO             |                | NO       | TriviallyDestructible
        | -                  | NO           | NO**           | -              | YES            |                | NO       | TriviallyAssignable
        | -                  | NO           | NO**           | -              | NO             |                | NO       | NonTrivial
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
     	  | (YES)              | YES          | -              | YES            | -              | YES            | YES      | Trivial
        | (NO)               | YES          | -              | YES            | -              | YES            | NO       | NonTriviallyDefaultConstructible
Move    | -                  | YES          | -              | YES            | -              | NO             | NO       | NonTriviallyMoveAssignable
assign- | -                  | YES          | -              | NO             | -              | YES            | NO       | NonTriviallyMoveConstructible
meant   | -                  | YES          | -              | NO             | -              | NO             | NO       | TriviallyDestructible
        | -                  | NO           | -              | NO**           | -              | YES            | NO       | TriviallyAssignable
        | -                  | NO           | -              | NO**           | -              | NO             | NO       | NonTrivial

*) The values in this column do not vary independently, they are implied by the other properties
**) Implied by "not trivially destructible"
*/

// A trivial type
struct Trivial {
  int value;
  friend constexpr bool operator==(Trivial x, Trivial y) = default;
};
static_assert(std::is_trivial_v                        <Trivial>);
static_assert(std::is_trivially_default_constructible_v<Trivial>);
static_assert(std::is_trivially_copy_constructible_v   <Trivial>);
static_assert(std::is_trivially_move_constructible_v   <Trivial>);
static_assert(std::is_trivially_destructible_v         <Trivial>);
static_assert(std::is_trivially_copy_assignable_v      <Trivial>);
static_assert(std::is_trivially_move_assignable_v      <Trivial>);

// A type which is not trivially default constructible (and thus not trivial),
// and all other SMFs are trivial.
struct NonTriviallyDefaultConstructible {
  int value = 0;
  friend constexpr bool operator==(NonTriviallyDefaultConstructible x, NonTriviallyDefaultConstructible y) = default;
};
static_assert(not std::is_trivial_v                        <NonTriviallyDefaultConstructible>);
static_assert(not std::is_trivially_default_constructible_v<NonTriviallyDefaultConstructible>);
static_assert(    std::is_default_constructible_v          <NonTriviallyDefaultConstructible>);
static_assert(    std::is_trivially_copy_constructible_v   <NonTriviallyDefaultConstructible>);
static_assert(    std::is_trivially_move_constructible_v   <NonTriviallyDefaultConstructible>);
static_assert(    std::is_trivially_destructible_v         <NonTriviallyDefaultConstructible>);
static_assert(    std::is_trivially_copy_assignable_v      <NonTriviallyDefaultConstructible>);
static_assert(    std::is_trivially_move_assignable_v      <NonTriviallyDefaultConstructible>);

// A type which is not trivially copy constructible (and thus not trivial),
// and all other SMFs are trivial.
struct NonTriviallyCopyConstructible {
  int value;
  constexpr NonTriviallyCopyConstructible() noexcept = default;
  constexpr NonTriviallyCopyConstructible(int v) noexcept : value(v) {}
  constexpr NonTriviallyCopyConstructible(NonTriviallyCopyConstructible const &other) noexcept : value(other.value) {}
  constexpr NonTriviallyCopyConstructible(NonTriviallyCopyConstructible &&) noexcept = default;
  constexpr NonTriviallyCopyConstructible &operator=(NonTriviallyCopyConstructible const &) noexcept = default;
  friend constexpr bool operator==(NonTriviallyCopyConstructible x, NonTriviallyCopyConstructible y) = default;
};
static_assert(not std::is_trivial_v                         <NonTriviallyCopyConstructible>);
static_assert(    std::is_trivially_default_constructible_v <NonTriviallyCopyConstructible>);
static_assert(not std::is_trivially_copy_constructible_v    <NonTriviallyCopyConstructible>);
static_assert(    std::is_copy_constructible_v              <NonTriviallyCopyConstructible>);
static_assert(    std::is_trivially_move_constructible_v    <NonTriviallyCopyConstructible>);
static_assert(    std::is_trivially_destructible_v          <NonTriviallyCopyConstructible>);
static_assert(    std::is_trivially_move_assignable_v       <NonTriviallyCopyConstructible>);
static_assert(    std::is_trivially_copy_assignable_v       <NonTriviallyCopyConstructible>);

// A type which is not trivially move constructible (and thus not trivial),
// and all other SMFs are trivial.
struct NonTriviallyMoveConstructible {
  int value;
  constexpr NonTriviallyMoveConstructible() noexcept = default;
  constexpr NonTriviallyMoveConstructible(int v) noexcept : value(v) {}
  constexpr NonTriviallyMoveConstructible(NonTriviallyMoveConstructible const &) noexcept = default;
  constexpr NonTriviallyMoveConstructible(NonTriviallyMoveConstructible &&other) noexcept : value(other.value) {}
  constexpr NonTriviallyMoveConstructible &operator=(NonTriviallyMoveConstructible const &) noexcept = default;
  friend constexpr bool operator==(NonTriviallyMoveConstructible x, NonTriviallyMoveConstructible y) = default;
};
static_assert(not std::is_trivial_v                         <NonTriviallyMoveConstructible>);
static_assert(    std::is_trivially_default_constructible_v <NonTriviallyMoveConstructible>);
static_assert(    std::is_trivially_copy_constructible_v    <NonTriviallyMoveConstructible>);
static_assert(not std::is_trivially_move_constructible_v    <NonTriviallyMoveConstructible>);
static_assert(    std::is_move_constructible_v              <NonTriviallyMoveConstructible>);
static_assert(    std::is_trivially_destructible_v          <NonTriviallyMoveConstructible>);
static_assert(    std::is_trivially_copy_assignable_v       <NonTriviallyMoveConstructible>);
static_assert(    std::is_trivially_move_assignable_v       <NonTriviallyMoveConstructible>);

// A type which is not trivially copy assignable (and thus not trivial),
// and all other SMFs are trivial.
struct NonTriviallyCopyAssignable {
  int value;
  constexpr NonTriviallyCopyAssignable() noexcept = default;
  constexpr NonTriviallyCopyAssignable(int v) noexcept : value(v) {}
  constexpr NonTriviallyCopyAssignable(NonTriviallyCopyAssignable const &) noexcept = default;

  constexpr NonTriviallyCopyAssignable &operator=(NonTriviallyCopyAssignable const &other) noexcept {
    value = other.value;
    return *this;
  }

  constexpr NonTriviallyCopyAssignable &operator=(NonTriviallyCopyAssignable &&) noexcept = default;
  friend constexpr bool operator==(NonTriviallyCopyAssignable x, NonTriviallyCopyAssignable y) = default;
};
static_assert(not std::is_trivial_v                        <NonTriviallyCopyAssignable>);
static_assert(    std::is_trivially_default_constructible_v<NonTriviallyCopyAssignable>);
static_assert(    std::is_trivially_copy_constructible_v   <NonTriviallyCopyAssignable>);
static_assert(    std::is_trivially_move_constructible_v   <NonTriviallyCopyAssignable>);
static_assert(    std::is_trivially_destructible_v         <NonTriviallyCopyAssignable>);
static_assert(not std::is_trivially_copy_assignable_v      <NonTriviallyCopyAssignable>);
static_assert(    std::is_copy_assignable_v                <NonTriviallyCopyAssignable>);
static_assert(    std::is_trivially_move_assignable_v      <NonTriviallyCopyAssignable>);

// A type which is not trivially move assignable (and thus not trivial),
// and all other SMFs are trivial.
struct NonTriviallyMoveAssignable {
  int value;
  constexpr NonTriviallyMoveAssignable() noexcept = default;
  constexpr NonTriviallyMoveAssignable(int v) noexcept : value(v) {}
  constexpr NonTriviallyMoveAssignable(NonTriviallyMoveAssignable const &) noexcept = default;
  constexpr NonTriviallyMoveAssignable &operator=(NonTriviallyMoveAssignable const &) noexcept = default;

  constexpr NonTriviallyMoveAssignable &operator=(NonTriviallyMoveAssignable &&other) noexcept {
    value = other.value;
    return *this;
  }

  friend constexpr bool operator==(NonTriviallyMoveAssignable x, NonTriviallyMoveAssignable y) = default;
};
static_assert(not std::is_trivial_v                        <NonTriviallyMoveAssignable>);
static_assert(    std::is_trivially_default_constructible_v<NonTriviallyMoveAssignable>);
static_assert(    std::is_trivially_copy_constructible_v   <NonTriviallyMoveAssignable>);
static_assert(    std::is_trivially_move_constructible_v   <NonTriviallyMoveAssignable>);
static_assert(    std::is_trivially_destructible_v         <NonTriviallyMoveAssignable>);
static_assert(    std::is_trivially_copy_assignable_v      <NonTriviallyMoveAssignable>);
static_assert(not std::is_trivially_move_assignable_v      <NonTriviallyMoveAssignable>);
static_assert(    std::is_move_assignable_v                <NonTriviallyMoveAssignable>);

// A type which is trivially copy assignable and trivially move assignable,
// and all other SMS are non-trivial.
struct TriviallyAssignable {
  int value;
  constexpr ~TriviallyAssignable() {}
  friend constexpr bool operator==(TriviallyAssignable x, TriviallyAssignable y) = default;
};
static_assert(not std::is_trivial_v                        <TriviallyAssignable>);
static_assert(not std::is_trivially_default_constructible_v<TriviallyAssignable>);
static_assert(    std::is_default_constructible_v          <TriviallyAssignable>);
static_assert(not std::is_trivially_copy_constructible_v   <TriviallyAssignable>);
static_assert(    std::is_copy_constructible_v             <TriviallyAssignable>);
static_assert(not std::is_trivially_move_constructible_v   <TriviallyAssignable>);
static_assert(    std::is_move_constructible_v             <TriviallyAssignable>);
static_assert(not std::is_trivially_destructible_v         <TriviallyAssignable>);
static_assert(    std::is_destructible_v                   <TriviallyAssignable>);
static_assert(    std::is_trivially_copy_assignable_v      <TriviallyAssignable>);
static_assert(    std::is_trivially_move_assignable_v      <TriviallyAssignable>);

// A type which is trivially destructible, and all other SMFs are non-trivial.
struct TriviallyDestructible {
  int value;
  constexpr TriviallyDestructible() noexcept : value() {}
  constexpr TriviallyDestructible(int v) noexcept : value(v) {}
  constexpr TriviallyDestructible(TriviallyDestructible const &other) noexcept : value(other.value) {}
  constexpr ~TriviallyDestructible() = default;

  constexpr TriviallyDestructible &operator=(TriviallyDestructible const &other) noexcept {
    value = other.value;
    return *this;
  }

  friend constexpr bool operator==(TriviallyDestructible x, TriviallyDestructible y) = default;
};
static_assert(not std::is_trivial_v                        <TriviallyDestructible>);
static_assert(not std::is_trivially_default_constructible_v<TriviallyDestructible>);
static_assert(    std::is_default_constructible_v          <TriviallyDestructible>);
static_assert(not std::is_trivially_copy_constructible_v   <TriviallyDestructible>);
static_assert(    std::is_copy_constructible_v             <TriviallyDestructible>);
static_assert(not std::is_trivially_move_constructible_v   <TriviallyDestructible>);
static_assert(    std::is_move_constructible_v             <TriviallyDestructible>);
static_assert(    std::is_trivially_destructible_v         <TriviallyDestructible>);
static_assert(not std::is_trivially_copy_assignable_v      <TriviallyDestructible>);
static_assert(    std::is_copy_assignable_v                <TriviallyDestructible>);
static_assert(not std::is_trivially_move_assignable_v      <TriviallyDestructible>);
static_assert(    std::is_move_assignable_v                <TriviallyDestructible>);

// A type with no trivial member function at all.
struct NonTrivial {
  static std::size_t num_objects;
  int value;

  constexpr NonTrivial() noexcept : NonTrivial(0) {}
  constexpr NonTrivial(int v) noexcept : value(v) {
    if (not std::is_constant_evaluated()) {
      ++num_objects;
    }
  }
  constexpr NonTrivial(NonTrivial const &other) noexcept
    : NonTrivial(other.value) {}

  constexpr NonTrivial &operator=(NonTrivial const &other) noexcept {
    if (not std::is_constant_evaluated()) {
      ++num_objects;
    }
    value = other.value;
    return *this;
  }

  constexpr ~NonTrivial() {
    if (not std::is_constant_evaluated()) {
      --num_objects;
    }
  }
  friend constexpr bool operator==(NonTrivial x, NonTrivial y) = default;
};
std::size_t NonTrivial::num_objects;

template<typename T, typename = void>
struct counts_objects : std::false_type {};
template<typename T>
struct counts_objects<T, std::void_t<decltype(T::num_objects)>>
  : std::true_type {};
template<typename T>
inline constexpr bool counts_objects_v = counts_objects<T>::value;

static_assert(not std::is_trivial_v                        <NonTrivial>);
static_assert(not std::is_trivially_default_constructible_v<NonTrivial>);
static_assert(    std::is_default_constructible_v          <NonTrivial>);
static_assert(not std::is_trivially_copy_constructible_v   <NonTrivial>);
static_assert(    std::is_copy_constructible_v             <NonTrivial>);
static_assert(not std::is_trivially_move_constructible_v   <NonTrivial>);
static_assert(    std::is_move_constructible_v             <NonTrivial>);
static_assert(not std::is_trivially_destructible_v         <NonTrivial>);
static_assert(    std::is_destructible_v                   <NonTrivial>);
static_assert(not std::is_trivially_copy_assignable_v      <NonTrivial>);
static_assert(    std::is_copy_assignable_v                <NonTrivial>);
static_assert(not std::is_trivially_move_assignable_v      <NonTrivial>);
static_assert(    std::is_move_assignable_v                <NonTrivial>);

// clang-format on

template <typename T, std::size_t N> struct TestParam {
  using value_type = T;
  inline static constexpr std::size_t capacity = N;
};

using AllTypes = ::testing::Types<
    TestParam<Trivial, 0>, TestParam<Trivial, 1>, TestParam<Trivial, 5>,
    TestParam<Trivial, 42>, TestParam<NonTriviallyDefaultConstructible, 0>,
    TestParam<NonTriviallyDefaultConstructible, 1>,
    TestParam<NonTriviallyDefaultConstructible, 5>,
    TestParam<NonTriviallyDefaultConstructible, 42>,
    TestParam<NonTriviallyCopyConstructible, 0>,
    TestParam<NonTriviallyCopyConstructible, 1>,
    TestParam<NonTriviallyCopyConstructible, 5>,
    TestParam<NonTriviallyCopyConstructible, 42>,
    TestParam<NonTriviallyMoveConstructible, 0>,
    TestParam<NonTriviallyMoveConstructible, 1>,
    TestParam<NonTriviallyMoveConstructible, 5>,
    TestParam<NonTriviallyMoveConstructible, 42>,
    TestParam<NonTriviallyCopyAssignable, 0>,
    TestParam<NonTriviallyCopyAssignable, 1>,
    TestParam<NonTriviallyCopyAssignable, 5>,
    TestParam<NonTriviallyCopyAssignable, 42>,
    TestParam<NonTriviallyMoveAssignable, 0>,
    TestParam<NonTriviallyMoveAssignable, 1>,
    TestParam<NonTriviallyMoveAssignable, 5>,
    TestParam<NonTriviallyMoveAssignable, 42>,
    TestParam<TriviallyAssignable, 0>, TestParam<TriviallyAssignable, 1>,
    TestParam<TriviallyAssignable, 5>, TestParam<TriviallyAssignable, 42>,
    TestParam<TriviallyDestructible, 0>, TestParam<TriviallyDestructible, 1>,
    TestParam<TriviallyDestructible, 5>, TestParam<TriviallyDestructible, 42>,
    TestParam<NonTrivial, 0>, TestParam<NonTrivial, 1>,
    TestParam<NonTrivial, 5>, TestParam<NonTrivial, 42>>;

template <typename Param> class BasicTest : public ::testing::Test {
public:
  using T = Param::value_type;
  inline static constexpr std::size_t N = Param::capacity;
  using X = beman::inplace_vector<T, N>;
  using IV = X;

  // Returns IV of size n with unique values
  static IV unique(typename IV::size_type n = IV::max_size()) {
    static T val = T{};
    IV res;
    while (n > 0) {
      res.push_back(val);
      ++val.value;
      --n;
    }
    return res;
  }
};

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

// [container.rqmts]
template <typename Param>
class ContainerRequirements : public BasicTest<Param> {};
TYPED_TEST_SUITE(ContainerRequirements, AllTypes);

TYPED_TEST(ContainerRequirements, ValueType) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // typename X::value_type
  //   Result: T
  //   Preconditions: T is Cpp17Erasable from X (see [container.alloc.reqmts]).
  EXPECT_TRUE((std::is_same_v<typename X::value_type, T>));
}

TYPED_TEST(ContainerRequirements, Reference) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // typename X::reference
  //   Result: T&
  EXPECT_TRUE((std::is_same_v<typename X::reference, T &>));

  // typename X::const_reference
  //   Result: const T&
  EXPECT_TRUE((std::is_same_v<typename X::const_reference, const T &>));
}

TYPED_TEST(ContainerRequirements, Iterator) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // typename X::iterator
  //   Result: A type that meets the forward iterator requirements
  //     ([forward.iterators]) with value type T. The type X​::​iterator is
  //     convertible to X​::​const_iterator.
  EXPECT_TRUE(std::forward_iterator<typename X::iterator>);
  EXPECT_TRUE(
      std::equality_comparable<typename X::iterator>); // required by
                                                       // [forward.iterators],
                                                       // but not by
                                                       // std::forward_iterator
  EXPECT_TRUE((std::is_same_v<decltype(*std::declval<typename X::iterator>()),
                              T &>)); // required by [forward.iterators], but
                                      // not by std::forward_iterator
  EXPECT_TRUE((
      std::is_convertible_v<typename X::iterator, typename X::const_iterator>));

  // typename X::const_iterator
  //   Result: A type that meets the requirements of a constant iterator and
  //     those of a forward iterator with value type T.
  EXPECT_TRUE(std::forward_iterator<typename X::const_iterator>);
  EXPECT_TRUE(std::equality_comparable<
              typename X::const_iterator>); // required by
                                            // [forward.iterators], but
                                            // not by
                                            // std::forward_iterator
  EXPECT_TRUE(
      (std::is_same_v<decltype(*std::declval<typename X::const_iterator>()),
                      const T &>)); // required by [forward.iterators],
                                    // but not by std::forward_iterator
}

TYPED_TEST(ContainerRequirements, DifferenceType) {
  using X = TestFixture::X;

  // typename X::difference_type
  //   Result: A signed integer type, identical to the difference type of
  //     X​::​iterator and X​::​const_iterator.
  EXPECT_TRUE(std::is_signed_v<typename X::difference_type>);
  EXPECT_TRUE((std::is_same_v<typename X::difference_type,
                              typename std::iterator_traits<
                                  typename X::iterator>::difference_type>));
  EXPECT_TRUE(
      (std::is_same_v<typename X::difference_type,
                      typename std::iterator_traits<
                          typename X::const_iterator>::difference_type>));
}

TYPED_TEST(ContainerRequirements, SizeType) {
  using X = TestFixture::X;

  // typename X::size_type
  //   Result: An unsigned integer type that can represent any non-negative
  //     value of X​::​difference_type.
  EXPECT_TRUE(std::is_unsigned_v<typename X::size_type>);
  EXPECT_GE(sizeof(typename X::size_type), sizeof(typename X::difference_type));
}

TYPED_TEST(ContainerRequirements, DefaultConstructor) {
  using X = TestFixture::X;

  // X u;
  // X u = X();
  //   Postconditions: u.empty()
  //   Complexity: Constant.
  {
    X u;
    EXPECT_TRUE(u.empty());
    // How to test complexity?
  }
  {
    X u = X();
    EXPECT_TRUE(u.empty());
    // How to test complexity?
  }
}

TYPED_TEST(ContainerRequirements, CopyConstructor) {
  using X = TestFixture::X;

  // X u(v);
  // X u = v;
  //   Preconditions: T is Cpp17CopyInsertable into X (see below).
  //   Postconditions: u == v.
  //   Complexity: Linear.
  X const v(TestFixture::unique());
  {
    X u(v);
    EXPECT_EQ(u, v);
    // How to test complexity?
  }
  {
    X u = v;
    EXPECT_EQ(u, v);
    // How to test complexity?
  }
}

TYPED_TEST(ContainerRequirements, MoveConstructor) {
  using X = TestFixture::X;

  // X u(rv);
  // X u = rv;
  //   Postconditions: u is equal to the value that rv had before this
  //     construction.
  //   Complexity: Linear.
  X const v(TestFixture::unique());
  auto const rv = [&v]() { return v; };
  {
    X u(rv());
    EXPECT_EQ(u, v);
    // How to test complexity?
  }
  {
    X u = rv();
    EXPECT_EQ(u, v);
    // How to test complexity?
  }
}

TYPED_TEST(ContainerRequirements, CopyAssignment) {
  using X = TestFixture::X;

  // t = v;
  //   Result: X&.
  //   Postconditions: t == v.
  //   Complexity: Linear.
  X const v(TestFixture::unique(X::max_size() / 2));
  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X t(n);
    t = v;
    EXPECT_TRUE((std::is_same_v<decltype(t = v), X &>));
    EXPECT_EQ(t, v);
  }
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, MoveAssignment) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // t = rv
  //   Result: X&.
  //   Effects: All existing elements of t are either move assigned to or
  //     destroyed.
  //   Postconditions: If t and rv do not refer to the same object, t
  //     is equal to the value that rv had before this assignment.
  //   Complexity: Linear.
  X const v(TestFixture::unique(X::max_size() / 2));
  auto const rv = [&v]() { return v; };
  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    if constexpr (counts_objects_v<T>) {
      T::num_objects = 0;
    }
    X t(n);
    if constexpr (counts_objects_v<T>) {
      ASSERT_EQ(T::num_objects, t.size());
    }
    t = rv();
    EXPECT_TRUE((std::is_same_v<decltype(t = rv()), X &>));
    if constexpr (counts_objects_v<T>) {
      EXPECT_EQ(T::num_objects, v.size());
    }
    EXPECT_EQ(t, v);
  }
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Destructor) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // a.~X()
  //   Result: void.
  //   Effects: Destroys every element of a; any memory obtained is deallocated.
  //   Complexity: Linear.
  if constexpr (counts_objects_v<T>) {
    T::num_objects = 0;
  }
  alignas(X) std::byte storage[sizeof(X)];
  X *pa = new (static_cast<void *>(storage)) X(X::max_size());
  X &a = *pa;
  if constexpr (counts_objects_v<T>) {
    ASSERT_EQ(T::num_objects, X::max_size());
  }
  a.~X();
  EXPECT_TRUE(std::is_void_v<decltype(a.~X())>);
  if constexpr (counts_objects_v<T>) {
    EXPECT_EQ(T::num_objects, 0);
  }
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Begin) {
  using X = TestFixture::X;

  // b.begin()
  //   Result: iterator; const_iterator for constant b.
  //   Returns: An iterator referring to the first element in the container.
  ///  Complexity: Constant.
  // b.cbegin()
  //   Result: const_iterator.
  //   Returns: const_cast<X const&>(b).begin()
  //   Complexity: Constant.

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X b(n);
    X const cb(n);
    EXPECT_TRUE((std::is_same_v<decltype(b.begin()), typename X::iterator>));
    EXPECT_TRUE(
        (std::is_same_v<decltype(cb.begin()), typename X::const_iterator>));
    EXPECT_TRUE(
        (std::is_same_v<decltype(b.cbegin()), typename X::const_iterator>));
    EXPECT_EQ(b.cbegin(), const_cast<X const &>(b).begin());
    if (n > 0) {
      EXPECT_EQ(std::addressof(*b.begin()), std::addressof(b.data()[0]));
      EXPECT_EQ(std::addressof(*cb.begin()), std::addressof(cb.data()[0]));
      EXPECT_EQ(std::addressof(*b.cbegin()), std::addressof(b.data()[0]));
    }
    // How to test complexity?
  }
}

TYPED_TEST(ContainerRequirements, End) {
  using X = TestFixture::X;

  // b.end()
  //   Result: iterator; const_iterator for constant b.
  //   Returns: An iterator which is the past-the-end value for the container.
  ///  Complexity: Constant.
  // b.cend()
  //   Result: const_iterator.
  //   Returns: const_cast<X const&>(b).end()
  //   Complexity: Constant.

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X b(n);
    X const cb(n);
    EXPECT_TRUE((std::is_same_v<decltype(b.end()), typename X::iterator>));
    EXPECT_TRUE(
        (std::is_same_v<decltype(cb.end()), typename X::const_iterator>));
    EXPECT_TRUE(
        (std::is_same_v<decltype(b.cend()), typename X::const_iterator>));
    EXPECT_EQ(b.cend(), const_cast<X const &>(b).end());
    if (n > 0) {
      EXPECT_EQ(std::addressof(*(b.end() - 1)),
                std::addressof(b.data()[b.size() - 1]));
      EXPECT_EQ(std::addressof(*(cb.end() - 1)),
                std::addressof(cb.data()[cb.size() - 1]));
      EXPECT_EQ(std::addressof(*(b.cend() - 1)),
                std::addressof(b.data()[b.size() - 1]));
    }
    // How to test complexity?
  }
}

TYPED_TEST(ContainerRequirements, Ordering) {
  using X = TestFixture::X;

  // i <=> j
  //   Result: strong_ordering.
  //   Constraints: X​::​iterator meets the random access iterator
  //   requirements.
  //   Complexity: Constant.
  EXPECT_TRUE(std::random_access_iterator<typename X::iterator>);
  EXPECT_TRUE(std::random_access_iterator<typename X::const_iterator>);
  EXPECT_TRUE((std::is_same_v<decltype(std::declval<typename X::iterator>() <=>
                                       std::declval<typename X::iterator>()),
                              std::strong_ordering>));
  EXPECT_TRUE(
      (std::is_same_v<decltype(std::declval<typename X::iterator>() <=>
                               std::declval<typename X::const_iterator>()),
                      std::strong_ordering>));
  EXPECT_TRUE(
      (std::is_same_v<decltype(std::declval<typename X::const_iterator>() <=>
                               std::declval<typename X::iterator>()),
                      std::strong_ordering>));
  EXPECT_TRUE(
      (std::is_same_v<decltype(std::declval<typename X::const_iterator>() <=>
                               std::declval<typename X::const_iterator>()),
                      std::strong_ordering>));

  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Equality) {
  using X = TestFixture::X;

  // c == b
  //   Preconditions: T meets the Cpp17EqualityComparable requirements.
  //   Result: bool.
  //   Returns: equal(c.begin(), c.end(), b.begin(), b.end())
  //   [Note 1: The algorithm equal is defined in [alg.equal]. — end note]
  //   Complexity: Constant if c.size() != b.size(), linear otherwise.
  //   Remarks: == is an equivalence relation.
  // c != b
  //   Effects: Equivalent to !(c == b).
  std::array<X, 3> values;
  values[0] = X::max_size() > 0 ? TestFixture::unique(X::max_size() - 1)
                                : X{}; // { 0, 1, ... }
  values[1] = values[0];
  if (values[1].size() < X::max_size()) {
    values[1].push_back(TestFixture::unique(1)[0]);
  } // { 0, 1, 2, ... }
  values[2] = X::max_size() > 0 ? X(X::max_size() - 1) : X{}; // { 0, 0, ... }
  for (X const &c : values) {
    EXPECT_TRUE(c == c);
    for (X const &b : values) {
      EXPECT_TRUE((std::is_same_v<decltype(c == b), bool>));
      EXPECT_EQ(c == b, (std::equal(c.begin(), c.end(), b.begin(), b.end())));
      EXPECT_EQ(c == b, b == c);
      EXPECT_EQ(c != b, !(c == b));
      for (X const &a : values) {
        EXPECT_TRUE(a == b && b == c ? a == c : true);
      }
    }
  }
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Swap) {
  using X = TestFixture::X;

  // t.swap(s)
  //   Result: void.
  //   Effects: Exchanges the contents of t and s.
  //   Complexity: Linear.
  // swap(t, s)
  //   Effects: Equivalent to t.swap(s).

  X const t_proto(TestFixture::unique());
  X const s_proto(X::max_size());
  X t(t_proto);
  X s(s_proto);

  EXPECT_TRUE(std::is_void_v<decltype(t.swap(s))>);
  t.swap(s);
  EXPECT_EQ(t, s_proto);
  EXPECT_EQ(s, t_proto);
  EXPECT_TRUE(std::is_void_v<decltype(swap(t, s))>);
  swap(t, s);
  EXPECT_EQ(t, t_proto);
  EXPECT_EQ(s, s_proto);

  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Size) {
  using X = TestFixture::X;

  // c.size()
  //   Result: size_type.
  //   Returns: distance(c.begin(), c.end()), i.e., the number of elements in
  //     the container.
  //   Complexity: Constant.
  //   Remarks: The number of elements is
  //     defined by the rules of constructors, inserts, and erases.

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X c(n);
    EXPECT_TRUE((std::is_same_v<decltype(c.size()), typename X::size_type>));
    EXPECT_EQ(c.size(), std::distance(c.begin(), c.end()));
  }
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, MaxSize) {
  using X = TestFixture::X;
  constexpr auto N = TestFixture::N;

  // c.max_size()
  //   Result: size_type.
  //   Returns: distance(begin(), end()) for the largest possible container.
  //   Complexity: Constant.
  X c(N);
  EXPECT_TRUE((std::is_same_v<decltype(c.max_size()), typename X::size_type>));
  EXPECT_EQ(c.max_size(), std::distance(c.begin(), c.end()));
  // How to test complexity?
}

TYPED_TEST(ContainerRequirements, Empty) {
  using X = TestFixture::X;

  // c.empty()
  //   Result: bool.
  //   Returns: c.begin() == c.end()
  //   Complexity: Constant.
  //   Remarks: If the container is empty, then c.empty() is true.}

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X c(n);
    EXPECT_TRUE((std::is_same_v<decltype(c.empty()), bool>));
    EXPECT_EQ(c.empty(), c.begin() == c.end());
  }
  // How to test complexity?
}

// Still [container.reqmts]:
// Unless otherwise specified (see [associative.reqmts.except],
// [unord.req.except], [deque.modifiers], [inplace.vector.modifiers], and
// [vector.modifiers]) all container types defined in this Clause meet the
// following additional requirements:
// - If an exception is thrown by an insert() or emplace() function while
//   inserting a single element, that function has no effects.
//   --> specified in [inplace.vector.modifiers]
// - If an exception is thrown by a push_back(), push_front(), emplace_back(),
//   or emplace_front() function, that function has no effects.
//   --> push_front()/emplace_front() n.a. for inplace_vector,
//       push_back()/emplace_back() specified in [inplace.vector.modifiers]
// - No erase(), clear(), pop_back() or pop_front() function throws an
//   exception.
//   --> erase() specified in [inplace.vector.modifiers], pop_front()
//       n.a. for inplace_vector
TYPED_TEST(ContainerRequirements, NothrowClear) {
  using X = TestFixture::X;

  EXPECT_TRUE(noexcept(std::declval<X>().clear()));
}
TYPED_TEST(ContainerRequirements, NothrowPopBack) {
  using X = TestFixture::X;

  // pop_back() has a narrow contract, therefore we cannot check noexcept().
  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X c(n);
    if (n > 0) {
      EXPECT_NO_THROW(c.pop_back());
    }
  }
}
// - No copy constructor or assignment operator of a returned iterator throws an
//   exception.
TYPED_TEST(ContainerRequirements, NothrowIterator) {
  using X = TestFixture::X;

  EXPECT_TRUE(std::is_nothrow_copy_constructible_v<typename X::iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_constructible_v<typename X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_move_constructible_v<typename X::iterator>);
  EXPECT_TRUE(std::is_nothrow_move_constructible_v<typename X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_assignable_v<typename X::iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_assignable_v<typename X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_move_assignable_v<typename X::iterator>);
  EXPECT_TRUE(std::is_nothrow_move_assignable_v<typename X::const_iterator>);
}
// - No swap() function throws an exception.
//   --> Specified in [inplace.vector.overview]
// - No swap() function invalidates any references, pointers, or iterators
//   referring to the elements of the containers being swapped.
//   --> Waived by previous paragraph in [container.reqmts]

// [container.rev.reqmts]
template <typename Param>
class ReversibleContainerRequirements : public BasicTest<Param> {};
TYPED_TEST_SUITE(ReversibleContainerRequirements, AllTypes);

TYPED_TEST(ReversibleContainerRequirements, ReverseIterator) {
  using T = TestFixture::T;
  using X = TestFixture::X;

  // typename X::reverse_iterator
  //   Result: The type reverse_iterator<X​::​iterator>, an iterator type
  //     whose value type is T.
  EXPECT_TRUE((std::is_same_v<typename X::reverse_iterator,
                              std::reverse_iterator<typename X::iterator>>));
  EXPECT_TRUE((std::is_same_v<typename std::iterator_traits<
                                  typename X::reverse_iterator>::value_type,
                              T>));

  // typename X::const_reverse_iterator
  //   Result: The type reverse_iterator<X​::​const_iterator>, a constant
  //     iterator type whose value type is T.
  EXPECT_TRUE(
      (std::is_same_v<typename X::const_reverse_iterator,
                      std::reverse_iterator<typename X::const_iterator>>));
  EXPECT_TRUE(
      (std::is_same_v<typename std::iterator_traits<
                          typename X::const_reverse_iterator>::value_type,
                      T>));
}

TYPED_TEST(ReversibleContainerRequirements, RBegin) {
  using X = TestFixture::X;

  // a.rbegin()
  //   Result: reverse_iterator; const_reverse_iterator for constant a.
  //   Returns: reverse_iterator(end())
  //   Complexity: Constant.
  // a.crbegin()
  //   Result: const_reverse_iterator.
  //   Returns: const_cast<X const&>(a).rbegin()
  //   Complexity: Constant.

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X a(n);
    X const ca(n);
    EXPECT_TRUE(
        (std::is_same_v<decltype(a.rbegin()), typename X::reverse_iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(ca.rbegin()),
                                typename X::const_reverse_iterator>));
    EXPECT_EQ(a.rbegin(), typename X::reverse_iterator(a.end()));
    EXPECT_EQ(ca.rbegin(), typename X::const_reverse_iterator(ca.end()));
    EXPECT_TRUE((std::is_same_v<decltype(a.crbegin()),
                                typename X::const_reverse_iterator>));
    EXPECT_EQ(a.crbegin(), typename X::const_reverse_iterator(a.cend()));
    EXPECT_EQ(a.crbegin(), const_cast<X const &>(a).rbegin());
    // How to test complexity?
  }
}

TYPED_TEST(ReversibleContainerRequirements, REnd) {
  using X = TestFixture::X;

  // a.rend()
  //   Result: reverse_iterator; const_reverse_iterator for constant a.
  //   Returns: reverse_iterator(begin())
  //   Complexity: Constant.
  // a.crend()
  //   Result: const_reverse_iterator.
  //   Returns: const_cast<X const&>(a).rend()
  //   Complexity: Constant.

  for (typename X::size_type n = 0; n <= X::max_size(); ++n) {
    X a(n);
    X const ca(n);
    EXPECT_TRUE(
        (std::is_same_v<decltype(a.rend()), typename X::reverse_iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(ca.rend()),
                                typename X::const_reverse_iterator>));
    EXPECT_EQ(a.rend(), typename X::reverse_iterator(a.begin()));
    EXPECT_EQ(ca.rend(), typename X::const_reverse_iterator(ca.begin()));
    EXPECT_TRUE((std::is_same_v<decltype(a.crend()),
                                typename X::const_reverse_iterator>));
    EXPECT_EQ(a.crend(), typename X::const_reverse_iterator(a.cbegin()));
    EXPECT_EQ(a.crend(), const_cast<X const &>(a).rend());
    // How to test complexity?
  }
}

// [sequence.reqmts]
template <typename Param>
class SequenceContainerRequirments : public BasicTest<Param> {};
TYPED_TEST_SUITE(SequenceContainerRequirments, AllTypes);

// X u(n, t);
// Preconditions: T is Cpp17CopyInsertable into X.
// Effects: Constructs a sequence container with n copies of t.
// Postconditions: distance(u.begin(), u.end()) == n is true.

// X u(i, j);
// Preconditions: T is Cpp17EmplaceConstructible into X from *i. For vector, if
// the iterator does not meet the Cpp17ForwardIterator requirements
// ([forward.iterators]), T is also Cpp17MoveInsertable into X. Effects:
// Constructs a sequence container equal to the range [i, j). Each iterator in
// the range [i, j) is dereferenced exactly once. Postconditions:
// distance(u.begin(), u.end()) == distance(i, j) is true.

// X(from_range, rg)
// Preconditions: T is Cpp17EmplaceConstructible into X from
// *ranges​::​begin(rg). For vector, if R models neither
// ranges​::​sized_range nor ranges​::​forward_range, T is also
// Cpp17MoveInsertable into X. Effects: Constructs a sequence container equal to
// the range rg. Each iterator in the range rg is dereferenced exactly once.
// Postconditions: distance(begin(), end()) == ranges​::​distance(rg) is
// true.

// X(il)
// Effects: Equivalent to X(il.begin(), il.end()).

// a = il
// Result: X&.
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable.
// Effects: Assigns the range [il.begin(), il.end()) into a. All existing
// elements of a are either assigned to or destroyed. Returns: *this.

// a.emplace(p, args)
// Result: iterator.
// Preconditions: T is Cpp17EmplaceConstructible into X from args. For vector,
// inplace_vector, and deque, T is also Cpp17MoveInsertable into X and
// Cpp17MoveAssignable. Effects: Inserts an object of type T constructed with
// std​::​forward<Args>(args)... before p. [Note 1: args can directly or
// indirectly refer to a value in a. — end note] Returns: An iterator that
// points to the new element.

// a.insert(p, t)
// Result: iterator.
// Preconditions: T is Cpp17CopyInsertable into X. For vector, inplace_vector,
// and deque, T is also Cpp17CopyAssignable. Effects: Inserts a copy of t before
// p. Returns: An iterator that points to the copy of t inserted into a.

// a.insert(p, rv)
// Result: iterator.
// Preconditions: T is Cpp17MoveInsertable into X. For vector, inplace_vector,
// and deque, T is also Cpp17MoveAssignable. Effects: Inserts a copy of rv
// before p. Returns: An iterator that points to the copy of rv inserted into a.

// a.insert(p, n, t)
// Result: iterator.
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable.
// Effects: Inserts n copies of t before p.
// Returns: An iterator that points to the copy of the first element inserted
// into a, or p if n == 0.

// a.insert(p, i, j)
// Result: iterator.
// Preconditions: T is Cpp17EmplaceConstructible into X from *i. For vector,
// inplace_vector, and deque, T is also Cpp17MoveInsertable into X, and T meets
// the Cpp17MoveConstructible, Cpp17MoveAssignable, and Cpp17Swappable
// ([swappable.requirements]) requirements. Neither i nor j are iterators into
// a. Effects: Inserts copies of elements in [i, j) before p. Each iterator in
// the range [i, j) shall be dereferenced exactly once. Returns: An iterator
// that points to the copy of the first element inserted into a, or p if i == j.

// a.insert_range(p, rg)
// Result: iterator.
// Preconditions: T is Cpp17EmplaceConstructible into X from
// *ranges​::​begin(rg). For vector, inplace_vector, and deque, T is also
// Cpp17MoveInsertable into X, and T meets the Cpp17MoveConstructible,
// Cpp17MoveAssignable, and Cpp17Swappable ([swappable.requirements])
// requirements. rg and a do not overlap. Effects: Inserts copies of elements in
// rg before p. Each iterator in the range rg is dereferenced exactly once.
// Returns: An iterator that points to the copy of the first element inserted
// into a, or p if rg is empty.

// a.insert(p, il)
// Effects: Equivalent to a.insert(p, il.begin(), il.end()).
// a.erase(q)
// Result: iterator.
// Preconditions: For vector, inplace_vector, and deque, T is
// Cpp17MoveAssignable. Effects: Erases the element pointed to by q. Returns: An
// iterator that points to the element immediately following q prior to the
// element being erased. If no such element exists, a.end() is returned.

// a.erase(q1, q2)
// Result: iterator.
// Preconditions: For vector, inplace_vector, and deque, T is
// Cpp17MoveAssignable. Effects: Erases the elements in the range [q1, q2).
// Returns: An iterator that points to the element pointed to by q2 prior to any
// elements being erased. If no such element exists, a.end() is returned.

// a.clear()
// Result: void
// Effects: Destroys all elements in a. Invalidates all references, pointers,
// and iterators referring to the elements of a and may invalidate the
// past-the-end iterator. Postconditions: a.empty() is true. Complexity: Linear.

// a.assign(i, j)
// Result: void
// Preconditions: T is Cpp17EmplaceConstructible into X from *i and assignable
// from *i. For vector, if the iterator does not meet the forward iterator
// requirements ([forward.iterators]), T is also Cpp17MoveInsertable into X.
// Neither i nor j are iterators into a. Effects: Replaces elements in a with a
// copy of [i, j). Invalidates all references, pointers and iterators referring
// to the elements of a. For vector and deque, also invalidates the past-the-end
// iterator. Each iterator in the range [i, j) is dereferenced exactly once.

// a.assign_range(rg)
// Result: void
// Mandates: assignable_from<T&, ranges​::​range_reference_t<R>> is modeled.
// Preconditions: T is Cpp17EmplaceConstructible into X from
// *ranges​::​begin(rg). For vector, if R models neither
// ranges​::​sized_range nor ranges​::​forward_range, T is also
// Cpp17MoveInsertable into X. rg and a do not overlap. Effects: Replaces
// elements in a with a copy of each element in rg. Invalidates all references,
// pointers, and iterators referring to the elements of a. For vector and deque,
// also invalidates the past-the-end iterator. Each iterator in the range rg is
// dereferenced exactly once.

// a.assign(il)
// Effects: Equivalent to a.assign(il.begin(), il.end()).

// a.assign(n, t)
// Result: void
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable. t is
// not a reference into a. Effects: Replaces elements in a with n copies of t.
// Invalidates all references, pointers and iterators referring to the elements
// of a. For vector and deque, also invalidates the past-the-end iterator. For
// every sequence container defined in this Clause and in [strings]:
//
// If the constructor
// template<class InputIterator>
//   X(InputIterator first, InputIterator last,
//     const allocator_type& alloc = allocator_type());
//
// is called with a type InputIterator that does not qualify as an input
// iterator, then the constructor shall not participate in overload resolution.
//
// If the member functions of the forms:
// template<class InputIterator>
//   return-type F(const_iterator p,
//                 InputIterator first, InputIterator last);       // such as
//                 insert
//
// template<class InputIterator>
//   return-type F(InputIterator first, InputIterator last);       // such as
//   append, assign
//
// template<class InputIterator>
//   return-type F(const_iterator i1, const_iterator i2,
//                 InputIterator first, InputIterator last);       // such as
//                 replace
//
// are called with a type InputIterator that does not qualify as an input
// iterator, then these functions shall not participate in overload resolution.
// A deduction guide for a sequence container shall not participate in overload
// resolution if it has an InputIterator template parameter and a type that does
// not qualify as an input iterator is deduced for that parameter, or if it has
// an Allocator template parameter and a type that does not qualify as an
// allocator is deduced for that parameter. The following operations are
// provided for some types of sequence containers but not others. Operations
// other than prepend_range and append_range are implemented so as to take
// amortized constant time. Result: reference; const_reference for constant a.
// Returns: *a.begin()
// a.back()
// Result: reference; const_reference for constant a.
// Effects: Equivalent to:
// auto tmp = a.end();
// --tmp;
// return *tmp;
// Remarks: Required for basic_string, array, deque, inplace_vector, list, and
// vector.

// a.emplace_back(args)
// Returns: a.back().

// a.push_back(t)
// Result: void
// Preconditions: T is Cpp17CopyInsertable into X.
// Effects: Appends a copy of t.

// a.push_back(rv)
// Result: void
// Preconditions: T is Cpp17MoveInsertable into X.
// Effects: Appends a copy of rv.

// a.pop_back()
// Result: void
// Preconditions: a.empty() is false.
// Effects: Destroys the last element.

// a[n]
// Result: reference; const_reference for constant
// Effects: Equivalent to: return *(a.begin() + n);

// a.at(n)
// Result: reference; const_reference for constant a
// Returns: *(a.begin() + n)
// Throws: out_of_range if n >= a.size().

// 3 For any N, inplace_vector<T, N>::iterator and inplace_vector<T,
// N>::const_iterator meet the constexpr iterator requirements.
//
// 4 For any N > 0 , if T is not trivially copyable or
// is_trivially_default_constructible_v<T> is false, then no inplace_vector<T,
// N> member functions are usable in constant expressions.
//
// TODO: This would be tested in another test suite.

// 5 Any member function of inplace_vector<T, N> that would cause the size to
// exceed N throws an exception of type bad_alloc.
//
// This is tested in individual function tests.

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

template <typename Param> class Triviality : public BasicTest<Param> {};
TYPED_TEST_SUITE(Triviality, AllTypes);

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

// 23.3.14.2 Constructors [inplace.vector.cons]

template <typename Param> class Constructors : public BasicTest<Param> {};
TYPED_TEST_SUITE(Constructors, AllTypes);

TYPED_TEST(Constructors, SizedDefault) {
  // constexpr explicit inplace_vector(size_type n);
  // Preconditions: T is Cpp17DefaultInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n default-inserted elements.
  // Complexity : Linear in n.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV zero(0);
  EXPECT_EQ(zero, IV{});

  EXPECT_THROW(IV(IV::capacity() + 1), beman::bad_alloc);

  constexpr auto mid_size = std::midpoint(0ul, IV::capacity());
  IV mid(mid_size);
  EXPECT_EQ(mid.size(), mid_size);
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV mid_correct;
    for (auto i = 0ul; i < mid_size; ++i)
      mid_correct.emplace_back();

    EXPECT_EQ(mid, mid_correct);
  }

  IV full(IV::capacity());
  EXPECT_EQ(full.size(), IV::capacity());
  if (std::is_same_v<T, NonTriviallyDefaultConstructible> ||
      std::is_same_v<T, NonTrivial>) {

    IV full_correct;
    for (auto i = 0ul; i < full.size(); ++i)
      full_correct.emplace_back();

    EXPECT_EQ(full, full_correct);
  }
}

TYPED_TEST(Constructors, SizedValue) {
  // constexpr inplace_vector(size_type n, const T& value);
  // Preconditions: T is Cpp17CopyInsertable into inplace_vector.
  // Effects: Constructs an inplace_vector with n copies of value.
  // Complexity: Linear in n.
  // TODO
  GTEST_SKIP();
}

TYPED_TEST(Constructors, CopyIter) {
  // template<class InputIterator>
  //   constexpr inplace_vector(InputIterator first, InputIterator last);
  // Effects: Constructs an inplace_vector equal to the range [first, last).
  // Complexity: Linear in distance(first, last).
  // TODO
  GTEST_SKIP();
}

TYPED_TEST(Constructors, CopyRanges) {
  // template<container-compatible-range<T> R>
  // constexpr inplace_vector(from_range_t, R&& rg);
  // Effects: Constructs an inplace_vector with the elements of the range rg.
  // Complexity: Linear in ranges::distance(rg).
  // TODO
  GTEST_SKIP();
}

// 23.3.14.3 Size and capacity [inplace.vector.capacity]

template <typename Param> class SizeNCapacity : public BasicTest<Param> {};
TYPED_TEST_SUITE(SizeNCapacity, AllTypes);

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

  EXPECT_THROW(device.resize(device.capacity() + 1), beman::bad_alloc);
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
  EXPECT_THROW(device.resize(device.capacity() + 1), beman::bad_alloc);
  EXPECT_EQ(device, before_resize);
}

// 23.3.14.4 Data [inplace.vector.data]

template <typename Param> class Data : public BasicTest<Param> {};
TYPED_TEST_SUITE(Data, AllTypes);

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

// 23.3.14.5 Modifiers [inplace.vector.modifiers]
template <typename Param> class Modifiers : public BasicTest<Param> {};
TYPED_TEST_SUITE(Modifiers, AllTypes);

TYPED_TEST(Modifiers, InsertSingleConstRef) {
  // constexpr iterator insert(const_iterator position, const T& x);

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;
  IV reference;

  if (device.capacity() > 0) {
    reference = this->unique();

    auto res = device.insert(device.begin(), reference[0]);
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + 1));

    if (device.capacity() > 1) {
      res = device.insert(device.end(), reference.back());
      EXPECT_EQ(res, device.end() - 1);
      EXPECT_EQ(device, IV({reference[0], reference.back()}));

      for (auto i = 1ul; i < (reference.size() - 1); ++i) {
        res = device.insert(device.end() - 1, reference[i]);
        EXPECT_EQ(res, device.begin() + i);

        IV correct(reference.begin(), reference.begin() + i + 1);
        correct.push_back(reference.back());

        EXPECT_EQ(device, correct);
      }
    }
  }

  T val{272};
  EXPECT_THROW(device.insert(device.begin(), val), beman::bad_alloc);
  EXPECT_EQ(device, reference);

  EXPECT_THROW(device.insert(device.begin(), val), beman::bad_alloc);
  EXPECT_EQ(device, reference);
}

TYPED_TEST(Modifiers, InsertSingleRV) {
  // constexpr iterator insert(const_iterator position, T&& x);

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;
  IV reference;

  if (device.capacity() > 0) {
    reference = this->unique();

    auto res = device.insert(device.begin(), T{reference[0]});
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + 1));

    if (device.capacity() > 1) {
      res = device.insert(device.end(), T{reference.back()});
      EXPECT_EQ(res, device.end() - 1);
      EXPECT_EQ(device, IV({reference[0], reference.back()}));

      for (auto i = 1ul; i < (reference.size() - 1); ++i) {
        res = device.insert(device.end() - 1, T{reference[i]});
        EXPECT_EQ(res, device.begin() + i);

        IV correct(reference.begin(), reference.begin() + i + 1);
        correct.push_back(reference.back());

        EXPECT_EQ(device, correct);
      }
    }
  }

  EXPECT_THROW(device.insert(device.begin(), T{272}), beman::bad_alloc);
  EXPECT_EQ(device, reference);

  EXPECT_THROW(device.insert(device.begin(), T{272}), beman::bad_alloc);
  EXPECT_EQ(device, reference);
}

TYPED_TEST(Modifiers, InsertEmplace) {
  //   constexpr iterator emplace(const_iterator position, Args&&... args);

  using IV = TestFixture::IV;

  IV device;
  IV reference;

  if (device.capacity() > 0) {
    reference = this->unique();

    auto res = device.emplace(device.begin(), reference[0].value);
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + 1));

    if (device.capacity() > 1) {
      res = device.emplace(device.end(), reference.back().value);
      EXPECT_EQ(res, device.end() - 1);
      EXPECT_EQ(device, IV({reference[0], reference.back()}));

      for (auto i = 1ul; i < (reference.size() - 1); ++i) {
        res = device.emplace(device.end() - 1, reference[i].value);
        EXPECT_EQ(res, device.begin() + i);

        IV correct(reference.begin(), reference.begin() + i + 1);
        correct.push_back(reference.back());

        EXPECT_EQ(device, correct);
      }
    }
  }

  EXPECT_THROW(device.emplace(device.begin(), 272), beman::bad_alloc);
  EXPECT_EQ(device, reference);

  EXPECT_THROW(device.emplace(device.begin(), 272), beman::bad_alloc);
  EXPECT_EQ(device, reference);
}

TYPED_TEST(Modifiers, InsertMulti) {
  // constexpr iterator insert(const_iterator position, size_type n, const T&
  // x);
  //
  // Complexity: Linear in the number of elements inserted plus the distance
  // to the end of the vector.
  // Remarks: If an exception is thrown other than by the copy constructor,
  // move constructor, assignment operator, or move assignment operator of T or
  // by any InputIterator operation, there are no effects. Otherwise, if an
  // exception is thrown, then size()  ≥ n and elements in the range begin() +
  // [0, n) are not modified.

  using IV = TestFixture::IV;

  IV device;

  if (device.capacity() > 0) {
    auto duplicate = this->unique(1)[0];
    IV reference(device.capacity(), duplicate);

    if (device.capacity() > 1) {
      auto front = this->unique(1)[0];
      reference[0] = front;
      device.push_back(front);
    }

    auto num_fill = device.capacity() - device.size();
    device.insert(device.end(), num_fill, duplicate);

    EXPECT_EQ(device, IV(reference.begin(), reference.end()));
  }

  EXPECT_NO_THROW(device.insert(device.begin(), 0, {2538}));
  EXPECT_THROW(device.insert(device.begin(), 1, {2538}), beman::bad_alloc);
}

TYPED_TEST(Modifiers, InsertInitList) {
  // constexpr iterator insert(const_iterator position, initializer_list<T> il);
  //
  // Let n be the value of size() before this call for the append_range
  // overload, and distance(begin, position) otherwise.
  // Complexity: Linear in the number of elements inserted plus the distance
  // to the end of the vector.
  // Remarks: If an exception is thrown other than by the copy constructor,
  // move constructor, assignment operator, or move assignment operator of T or
  // by any InputIterator operation, there are no effects. Otherwise, if an
  // exception is thrown, then size()  ≥ n and elements in the range begin() +
  // [0, n) are not modified.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;
  auto res = device.insert(device.end(), {});
  EXPECT_EQ(res, device.end());
  EXPECT_EQ(device, IV());

  if (device.capacity() > 0) {
    res = device.insert(device.begin(), {T{0}});
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV{T{0}});

    if (device.capacity() >= 3) {
      res = device.insert(device.begin(), {T{1}, T{2}});
      EXPECT_EQ(res, device.begin());

      IV expected{T{1}, T{2}, T{0}};
      EXPECT_EQ(device, expected);
    }
  }

  auto full = this->unique();
  EXPECT_NO_THROW(full.insert(full.begin(), {}));
  EXPECT_THROW(full.insert(full.begin(), {T{25}}), beman::bad_alloc);
}

TYPED_TEST(Modifiers, InsertRange) {
  // template<container-compatible-range<T> R>
  //   constexpr iterator insert_range(const_iterator position, R&& rg);
  //
  // Let n be the value of size() before this call for the append_range
  // overload, and distance(begin, position) otherwise.
  // Complexity: Linear in the number of elements inserted plus the distance
  // to the end of the vector.
  // Remarks: If an exception is thrown other than by the copy constructor,
  // move constructor, assignment operator, or move assignment operator of T or
  // by any InputIterator operation, there are no effects. Otherwise, if an
  // exception is thrown, then size()  ≥ n and elements in the range begin() +
  // [0, n) are not modified.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;
  auto reference = this->unique();

  auto res = device.insert_range(device.end(), reference | std::views::take(0));
  EXPECT_EQ(res, device.end());

  res = device.insert_range(device.end(), reference);
  EXPECT_EQ(res, device.begin());
  EXPECT_EQ(device, reference);
  device.clear();

  if (device.capacity() > 0) {
    res = device.insert_range(device.end(), reference | std::views::take(1));
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV({reference.front()}));

    if (device.capacity() > 1) {
      res = device.insert_range(
          device.begin() + 1,
          std::ranges::subrange(reference.end() - 1, reference.end()));
      EXPECT_EQ(res, device.begin() + 1);
      EXPECT_EQ(device, IV({reference.front(), reference.back()}));

      if (device.capacity() > 2) {
        res = device.insert_range(device.begin() + 1,
                                  reference | std::views::drop(1) |
                                      std::views::take(reference.size() - 2));
        EXPECT_EQ(res, device.begin() + 1);
        EXPECT_EQ(device, reference);
      }
    }
  }

  EXPECT_NO_THROW(device.insert_range(device.begin(), std::array<T, 0>{}));
  EXPECT_EQ(device, reference);

  EXPECT_THROW(device.insert_range(device.begin(), std::array<T, 1>{T{25}}),
               beman::bad_alloc);
}

TYPED_TEST(Modifiers, InsertItrRange) {
  //   constexpr iterator emplace(const_iterator position, Args&&... args);
  // template<container-compatible-range<T> R>
  //   constexpr void append_range(R&& rg);
  //
  // Let n be the value of size() before this call for the append_range
  // overload, and distance(begin, position) otherwise.
  // Complexity: Linear in the number of elements inserted plus the distance
  // to the end of the vector.
  // Remarks: If an exception is thrown other than by the copy constructor,
  // move constructor, assignment operator, or move assignment operator of T or
  // by any InputIterator operation, there are no effects. Otherwise, if an
  // exception is thrown, then size()  ≥ n and elements in the range begin() +
  // [0, n) are not modified.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  IV device;
  auto reference = this->unique();

  auto res = device.insert(device.end(), reference.end(), reference.end());
  EXPECT_EQ(res, device.end());

  res = device.insert(device.end(), reference.begin(), reference.end());
  EXPECT_EQ(res, device.begin());
  EXPECT_EQ(device, reference);
  device.clear();

  if (device.capacity() > 0) {
    res = device.insert(device.end(), reference.begin(), reference.begin() + 1);
    EXPECT_EQ(res, device.begin());
    EXPECT_EQ(device, IV({reference.front()}));

    if (device.capacity() > 1) {
      res = device.insert(device.begin() + 1, reference.end() - 1,
                          reference.end());
      EXPECT_EQ(res, device.begin() + 1);
      EXPECT_EQ(device, IV({reference.front(), reference.back()}));

      if (device.capacity() > 2) {
        res = device.insert(device.begin() + 1, reference.begin() + 1,
                            reference.end() - 1);
        EXPECT_EQ(res, device.begin() + 1);
        EXPECT_EQ(device, reference);
      }
    }
  }

  EXPECT_NO_THROW(
      device.insert(device.begin(), reference.end(), reference.end()));
  EXPECT_EQ(device, reference);

  std::array<T, 1> single_array{T{25}};
  EXPECT_THROW(
      device.insert(device.begin(), single_array.begin(), single_array.end()),
      beman::bad_alloc);
}

TYPED_TEST(Modifiers, InsertAppendRange) {
  // template<container-compatible-range<T> R>
  //   constexpr void append_range(R&& rg);
  //
  // Let n be the value of size() before this call for the append_range
  // overload, and distance(begin, position) otherwise.
  // Complexity: Linear in the number of elements inserted plus the distance
  // to the end of the vector.
  // Remarks: If an exception is thrown other than by the copy constructor,
  // move constructor, assignment operator, or move assignment operator of T or
  // by any InputIterator operation, there are no effects. Otherwise, if an
  // exception is thrown, then size()  ≥ n and elements in the range begin() +
  // [0, n) are not modified.

  using IV = TestFixture::IV;

  IV device;
  auto reference = this->unique();

  device.append_range(reference | std::views::take(0));
  EXPECT_EQ(device, IV());

  device.append_range(reference);
  EXPECT_EQ(device, reference);
  device.clear();

  auto half_size = std::midpoint(0ul, reference.size());
  device.append_range(reference | std::views::take(half_size));
  device.append_range(reference | std::views::drop(half_size));
  EXPECT_EQ(device, reference);
}

TYPED_TEST(Modifiers, PushBackConstRef) {
  // constexpr reference push_back(const T& x);
  //
  // Returns: back().
  // Throws: bad_alloc or any exception thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    auto val = reference[i];
    auto res = device.push_back(val);
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }

  T val{0};
  EXPECT_THROW(device.push_back(val), beman::bad_alloc);
}

TYPED_TEST(Modifiers, PushBackRV) {
  // constexpr reference push_back(T&& x);
  //
  // Returns: back().
  // Throws: bad_alloc or any exception thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    T val{reference[i]};
    auto res = device.push_back(std::move(val));
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }

  T val{0};
  EXPECT_THROW(device.push_back(val), beman::bad_alloc);
}

// TODO: Check if there's extra copies

TYPED_TEST(Modifiers, EmplaceBack) {
  // template<class... Args>
  //   constexpr reference emplace_back(Args&&... args);
  //
  // Returns: back().
  // Throws: bad_alloc or any exception thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    auto res = device.emplace_back(reference[i].value);
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }

  EXPECT_THROW(device.emplace_back(0), beman::bad_alloc);
}

TYPED_TEST(Modifiers, TryEmplaceBack) {
  // template<class... Args>
  // constexpr pointer try_emplace_back(Args&&... args);
  //
  // Let vals denote a pack:
  // (8.1) std::forward<Args>(args)... for the first overload,
  // (8.2) x for the second overload,
  // (8.3) std::move(x) for the third overload.
  //
  // Preconditions: value_type is Cpp17EmplaceConstructible into inplace_vector
  // from vals....
  // Effects: If size() < capacity() is true, appends an object of type T
  // direct-non-list-initialized with vals.... Otherwise, there are no effects.
  // Returns: nullptr if size() == capacity() is true, otherwise
  // addressof(back()).
  // Throws: Nothing unless an exception is thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;

  const auto reference = this->unique();
  IV device;
  if (!reference.empty()) {
    for (auto i = 0ul; i < reference.size(); ++i) {
      auto res = device.try_emplace_back(reference[i].value);
      EXPECT_EQ(res, std::addressof(device.back()));
      EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
    }

    auto res = device.try_emplace_back(reference[0].value);
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, reference);
  } else {
    auto res = device.try_emplace_back(0);
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, IV());
  }
}

TYPED_TEST(Modifiers, TryPushBackConstRef) {
  // constexpr pointer try_push_back(const T& x);
  //
  // Let vals denote a pack:
  // (8.1) std::forward<Args>(args)... for the first overload,
  // (8.2) x for the second overload,
  // (8.3) std::move(x) for the third overload.
  //
  // Preconditions: value_type is Cpp17EmplaceConstructible into inplace_vector
  // from vals....
  // Effects: If size() < capacity() is true, appends an object of type T
  // direct-non-list-initialized with vals.... Otherwise, there are no effects.
  // Returns: nullptr if size() == capacity() is true, otherwise
  // addressof(back()).
  // Throws: Nothing unless an exception is thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();
  IV device;

  if (!reference.empty()) {
    for (auto i = 0ul; i < reference.size(); ++i) {
      auto res = device.try_push_back(reference[i]);
      EXPECT_EQ(res, std::addressof(device.back()));
      EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
    }

    auto res = device.try_push_back(reference[0]);
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, reference);
  } else {
    T val{0};

    auto res = device.try_push_back(val);
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, IV());
  }
}

TYPED_TEST(Modifiers, TryPushBackRV) {
  // constexpr pointer try_push_back(T&& x);
  //
  // Let vals denote a pack:
  // (8.1) std::forward<Args>(args)... for the first overload,
  // (8.2) x for the second overload,
  // (8.3) std::move(x) for the third overload.
  //
  // Preconditions: value_type is Cpp17EmplaceConstructible into inplace_vector
  // from vals....
  // Effects: If size() < capacity() is true, appends an object of type T
  // direct-non-list-initialized with vals.... Otherwise, there are no effects.
  // Returns: nullptr if size() == capacity() is true, otherwise
  // addressof(back()).
  // Throws: Nothing unless an exception is thrown by the initialization of the
  // inserted element.
  // Complexity: Constant.
  // Remarks: If an exception is thrown, there are no effects on *this.

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;

  if (!reference.empty()) {
    for (auto i = 0ul; i < reference.size(); ++i) {
      T val{reference[i].value};

      auto res = device.try_push_back(std::move(val));
      EXPECT_EQ(res, std::addressof(device.back()));
      EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
    }

    auto res = device.try_push_back(reference[0]);
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, reference);
  } else {
    T val{0};

    auto res = device.try_push_back(std::move(val));
    EXPECT_EQ(res, nullptr);
    EXPECT_EQ(device, IV());
  }
}

TYPED_TEST(Modifiers, TryAppendRanges) {
  // template<container-compatible-range<T> R>
  // constexpr ranges::borrowed_iterator_t<R> try_append_range(R&& rg);
  //
  // Preconditions: value_type is Cpp17EmplaceConstructible into inplace_vector
  // from *ranges::begin(rg).
  //
  // Effects: Appends copies of initial elements
  // in rg before end(), until all elements are inserted or size() == capacity()
  // is true. Each iterator in the range rg is dereferenced at most once.
  //
  // Returns: An iterator pointing to the first element of rg that was not
  // inserted into *this, or ranges::end(rg) if no such element exists.
  // Complexity: Linear in the number of elements inserted.
  //
  // Remarks: Let n be the value of size() prior to this call. If an exception
  // is thrown after the insertion of k elements, then size() equals n + k ,
  // elements in the range begin() + [0, n) are not modified, and elements in
  // the range begin() + [n, n + k) correspond to the inserted elements.

  // TODO
  GTEST_SKIP();
}

TYPED_TEST(Modifiers, UncheckedEmplacedBack) {
  // template<class... Args>
  // constexpr reference unchecked_emplace_back(Args&&... args);
  //
  // Preconditions: size() < capacity() is true.
  // Effects: Equivalent to: return
  // *try_emplace_back(std::forward<Args>(args)...);

  using IV = TestFixture::IV;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    auto res = device.unchecked_emplace_back(reference[i].value);
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }
}

TYPED_TEST(Modifiers, UncheckedPushBackConstRef) {
  // constexpr reference unchecked_push_back(const T& x);
  // constexpr reference unchecked_push_back(T&& x);
  // Preconditions: size() < capacity() is true.
  // Effects: Equivalent to: return
  // *try_push_back(std​::​forward<decltype(x)>(x));

  using IV = TestFixture::IV;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    auto res = device.unchecked_push_back(reference[i]);
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }
}

TYPED_TEST(Modifiers, UncheckedPushBackRV) {
  // constexpr reference unchecked_push_back(const T& x);
  // constexpr reference unchecked_push_back(T&& x);
  // Preconditions: size() < capacity() is true.
  // Effects: Equivalent to: return
  // *try_push_back(std​::​forward<decltype(x)>(x));

  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto reference = this->unique();

  IV device;
  for (auto i = 0ul; i < reference.size(); ++i) {
    T val{reference[i].value};

    auto res = device.unchecked_push_back(std::move(val));
    EXPECT_EQ(res, device.back());
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i + 1));
  }
}

TYPED_TEST(Modifiers, ReserveNonEmpty) {
  // static constexpr void reserve(size_type n);
  //
  // Effects: None.
  // Throws: bad_alloc if n > capacity() is true.

  using IV = TestFixture::IV;

  const auto reference = this->unique();

  IV device(reference);

  device.reserve(device.size());
  EXPECT_EQ(device, reference);

  device.reserve(0);
  EXPECT_EQ(device, reference);

  device.reserve(device.capacity());
  EXPECT_EQ(device, reference);

  EXPECT_THROW(device.reserve(device.capacity() + 1), beman::bad_alloc);
}

TYPED_TEST(Modifiers, ReserveEmpty) {
  // static constexpr void reserve(size_type n);
  //
  // Effects: None.
  // Throws: bad_alloc if n > capacity() is true.

  using IV = TestFixture::IV;

  IV device;

  device.reserve(device.size());
  EXPECT_EQ(device, IV());

  device.reserve(0);
  EXPECT_EQ(device, IV());

  device.reserve(device.capacity());
  EXPECT_EQ(device, IV());

  EXPECT_THROW(device.reserve(device.capacity() + 1), beman::bad_alloc);
}

TYPED_TEST(Modifiers, ShrinkToFitNonEmpty) {
  // static constexpr void shrink_to_fit() noexcept;
  // Effects: None.

  using IV = TestFixture::IV;

  auto reference = this->unique();

  IV device(reference);
  reference.shrink_to_fit();

  EXPECT_EQ(device, reference);
}

TYPED_TEST(Modifiers, ShrinkToFitEmpty) {
  // static constexpr void shrink_to_fit() noexcept;
  // Effects: None.

  using IV = TestFixture::IV;

  IV device;
  device.shrink_to_fit();

  EXPECT_EQ(device, IV());
}

TYPED_TEST(Modifiers, EraseSingle) {
  // constexpr iterator erase(const_iterator position);
  //
  // Effects: Invalidates iterators and references at or after the point of the
  // erase.
  // Throws: Nothing unless an exception is thrown by the assignment
  // operator or move assignment operator of T.

  auto device = this->unique();

  if (device.empty())
    return;

  auto itr = device.erase(device.begin());
  if (device.empty())
    return;

  EXPECT_EQ(itr, device.begin());

  auto last_itr = device.end() - 1;

  itr = device.erase(last_itr);
  EXPECT_EQ(itr, device.end());

  auto mid_idx = device.size() / 2;
  auto mid_itr = device.begin() + mid_idx;
  itr = device.erase(mid_itr);
  EXPECT_EQ(itr, device.begin() + mid_idx);

  auto size = device.size();
  for (auto i = 0ul; i < size; ++i)
    device.erase(device.begin());

  EXPECT_TRUE(device.empty())
      << "device still have " << device.size() << " elements";
}

TYPED_TEST(Modifiers, EraseSingleConst) {
  // constexpr iterator erase(const_iterator position);
  //
  // Effects: Invalidates iterators and references at or after the point of the
  // erase.
  // Throws: Nothing unless an exception is thrown by the assignment
  // operator or move assignment operator of T.

  auto device = this->unique();

  if (device.empty())
    return;

  auto itr = device.erase(device.cbegin());
  if (device.empty())
    return;

  EXPECT_EQ(itr, device.cbegin());

  auto last_itr = device.cend() - 1;

  itr = device.erase(last_itr);
  EXPECT_EQ(itr, device.cend());

  auto mid_idx = device.size() / 2;
  auto mid_itr = device.cbegin() + mid_idx;
  itr = device.erase(mid_itr);
  EXPECT_EQ(itr, device.cbegin() + mid_idx);

  auto size = device.size();
  for (auto i = 0ul; i < size; ++i)
    device.erase(device.cbegin());

  EXPECT_TRUE(device.empty())
      << "device still have " << device.size() << " elements";
}

TYPED_TEST(Modifiers, EraseRange) {
  // constexpr iterator erase(const_iterator first, const_iterator last);
  //
  // Effects: Invalidates iterators and references at or after the point of the
  // erase.
  // Throws: Nothing unless an exception is thrown by the assignment
  // operator or move assignment operator of T.
  // Complexity: The destructor of T is called the number of times equal to the
  // number of the elements erased, but the assignment operator of T is called
  // the number of times equal to the number of elements after the erased
  // elements.

  using IV = TestFixture::IV;

  auto reference = this->unique();
  IV device(reference);

  auto itr = device.erase(device.begin(), device.begin());
  EXPECT_EQ(itr, device.begin());
  EXPECT_EQ(device, IV(reference));

  if (device.empty())
    return;

  itr = device.erase(device.begin(), device.begin() + 1);
  EXPECT_EQ(itr, device.begin());
  EXPECT_EQ(device, IV(reference.begin() + 1, reference.end()));

  if (device.empty())
    return;

  reference = IV(device);

  auto last_itr = device.end() - 1;

  itr = device.erase(last_itr, device.end());
  EXPECT_EQ(itr, device.end());
  EXPECT_EQ(device, IV(reference.begin(), reference.end() - 1));

  if (device.size() >= 4) {
    reference = IV(device);

    auto from_itr = device.begin() + 1;
    auto to_itr = device.end() - 1;

    itr = device.erase(from_itr, to_itr);
    EXPECT_EQ(itr, device.begin() + 1);
    EXPECT_EQ(device, IV({reference[0], reference.back()}));
  }
}

TYPED_TEST(Modifiers, EraseRangeAll) {
  // constexpr iterator erase(const_iterator first, const_iterator last);
  //
  // Effects: Invalidates iterators and references at or after the point of the
  // erase.
  // Throws: Nothing unless an exception is thrown by the assignment
  // operator or move assignment operator of T.
  // Complexity: The destructor of T is called the number of times equal to the
  // number of the elements erased, but the assignment operator of T is called
  // the number of times equal to the number of elements after the erased
  // elements.

  auto device = this->unique();
  auto itr = device.erase(device.begin(), device.end());
  EXPECT_EQ(itr, device.end());
  EXPECT_TRUE(device.empty());
}

TYPED_TEST(Modifiers, PopBack) {
  // constexpr void pop_back();
  //
  // Effects: Invalidates iterators and references at or after the point of the
  // erase.
  // Throws: Nothing unless an exception is thrown by the assignment
  // operator or move assignment operator of T.
  // Complexity: The destructor of T is called the number of times equal to the
  // number of the elements erased, but the assignment operator of T is called
  // the number of times equal to the number of elements after the erased
  // elements.

  using IV = TestFixture::IV;

  auto reference = this->unique();
  IV device(reference);

  if (reference.size() == 0)
    return;

  for (auto i = int(reference.size()); i > 0; --i) {
    EXPECT_EQ(device, IV(reference.begin(), reference.begin() + i));
    device.pop_back();
  }

  EXPECT_TRUE(device.empty())
      << "device still have " << device.size() << " elements";
}

// 23.3.14.6 Erasure [inplace.vector.erasure]

template <typename Param> class Erasure : public BasicTest<Param> {};
TYPED_TEST_SUITE(Erasure, AllTypes);

TYPED_TEST(Erasure, ByValue) {
  // template<class T, size_t N, class U = T>
  //   constexpr size_t erase(inplace_vector<T, N>& c, const U& value);
  //
  // Effects: Equivalent to:
  // auto it = remove(c.begin(), c.end(), value);
  // auto r = distance(it, c.end());
  // c.erase(it, c.end());
  // return r;

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  IV device;
  if constexpr (device.capacity() == 0)
    return;

  T duplicates{4612};
  auto uniques = this->unique(device.capacity() / 2);

  for (auto i = 0ul; i < uniques.size(); ++i) {
    device.push_back(uniques[i]);
    if (device.size() != device.capacity())
      device.push_back(duplicates);
  }

  // TODO: uncomment this after erase is implemented
  // beman::erase(device, duplicates);
  // EXPECT_EQ(uniques, device);

  GTEST_SKIP() << "Not implemented";
}

TYPED_TEST(Erasure, ByPred) {
  // template<class T, size_t N, class Predicate>
  // constexpr size_t erase_if(inplace_vector<T, N>& c, Predicate pred);
  //
  // Effects: Equivalent to:
  // auto it = remove_if(c.begin(), c.end(), pred);
  // auto r = distance(it, c.end());
  // c.erase(it, c.end());
  // return r;

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  IV device;
  if constexpr (device.capacity() == 0)
    return;

  for (auto i = 0; i < static_cast<int>(device.capacity()); ++i)
    device.push_back(T{i});

  // TODO: complete this when its implemented
  // beman::erase_if(device,
  //                 [&](auto &v) { return v.value > (device.capacity() / 2);
  //                 });

  GTEST_SKIP() << "Not implemented";
}

} // namespace
