#include "beman/inplace_vector/inplace_vector.hpp"
#include "gtest/gtest.h"
#include <algorithm>
#include <concepts>
#include <iterator>
#include <tuple>
#include <type_traits>

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
ment    | -                  | YES          | NO             | -              | NO             |                | NO       | TriviallyDestructible
        | -                  | NO           | NO**           | -              | YES            |                | NO       | TriviallyAssignable
        | -                  | NO           | NO**           | -              | NO             |                | NO       | NonTrivial
--------+--------------------+--------------+----------------+----------------+----------------+----------------+----------+----------------------------------
     	  | (YES)              | YES          | -              | YES            | -              | YES            | YES      | Trivial
        | (NO)               | YES          | -              | YES            | -              | YES            | NO       | NonTriviallyDefaultConstructible
Move    | -                  | YES          | -              | YES            | -              | NO             | NO       | NonTriviallyMoveAssignable
assign- | -                  | YES          | -              | NO             | -              | YES            | NO       | NonTriviallyMoveConstructible
ment    | -                  | YES          | -              | NO             | -              | NO             | NO       | TriviallyDestructible
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
  
  constexpr NonTrivial() noexcept : NonTrivial(int{}) {}
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

// TODO: Test sequence container reqs.

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

// 6 Let IV denote a specialization of inplace_vector<T, N>. If N is zero,
// then IV is both trivial and empty. Otherwise: (6.1) — If
// is_trivially_copy_constructible_v<T> is true, then IV has a trivial copy
// constructor (6.2) — If is_trivially_move_constructible_v<T> is true, then
// IV has a trivial move constructor. (6.3) — If
// is_trivially_destructible_v<T> is true, then: (6.3.1) — IV has a trivial
// destructor. (6.3.2) — If is_trivially_copy_constructible_v<T> &&
// is_trivially_copy_assignable_v<T> is true, then IV has a trivial copy
// assignment operator.
// (6.3.3) — If is_trivially_move_constructible_v<T> &&
// is_trivially_move_assignable_v<T> is true, then IV has a trivial move
// assignment operator.

template <typename Param> class Overview : public BasicTest<Param> {};
TYPED_TEST_SUITE(Overview, AllTypes);

TYPED_TEST(Overview, Section6) {
  using T = TestFixture::T;
  constexpr auto N = TestFixture::N;
  using IV = TestFixture::IV;

  // Let IV denote a specialization of inplace_vector<T, N>.
  {
    SCOPED_TRACE("If N is zero, then IV is both trivial and empty.");
    if constexpr (N == 0) {
      EXPECT_TRUE(std::is_trivial_v<IV>);
      EXPECT_TRUE(std::is_empty_v<IV>);
    }
  }
  {
    SCOPED_TRACE(
        "(6.1) — If is_trivially_copy_constructible_v<T> is true, then "
        "IV has a trivial copy constructor");
    if constexpr (std::is_trivially_copy_constructible_v<T>) {
      EXPECT_TRUE(std::is_trivially_copy_constructible_v<IV>);
    }
  }
  {
    SCOPED_TRACE(
        "(6.2) — If is_trivially_move_constructible_v<T> is true, then IV "
        "has a trivial move constructor.");
    if constexpr (std::is_trivially_move_constructible_v<T>) {
      EXPECT_TRUE(std::is_trivially_move_constructible_v<IV>);
    }
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.1) — IV has a trivial destructor.");
    if constexpr (std::is_trivially_destructible_v<T>) {
      EXPECT_TRUE(std::is_trivially_destructible_v<IV>);
    }
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.2) — If is_trivially_copy_constructible_v<T> && "
                 "is_trivially_copy_assignable_v<T> is true, then IV has a "
                 "trivial copy assignment operator.");
    if constexpr (std::is_trivially_destructible_v<T> &&
                  std::is_trivially_copy_constructible_v<T> &&
                  std::is_trivially_copy_assignable_v<T>) {
      EXPECT_TRUE(std::is_trivially_copy_assignable_v<IV>);
    }
  }
  {
    SCOPED_TRACE("(6.3) — If is_trivially_destructible_v<T> is true, then:\n"
                 "(6.3.3) — If is_trivially_move_constructible_v<T> && "
                 "is_trivially_move_assignable_v<T> is true, then IV has a "
                 "trivial move assignment operator.");
    if constexpr (std::is_trivially_destructible_v<T> &&
                  std::is_trivially_move_constructible_v<T> &&
                  std::is_trivially_move_assignable_v<T>) {
      EXPECT_TRUE(std::is_trivially_move_assignable_v<IV>);
    }
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
  // TODO
  GTEST_SKIP();
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
// TODO

// 23.3.14.4 Data [inplace.vector.data]
// TODO

// 23.3.14.5 Modifiers [inplace.vector.modifiers]
// TODO

// 23.3.14.6 Erasure [inplace.vector.erasure]
// TODO

#if 0

template <typename T> class InplaceVectorSMFs : public ::testing::Test {
public:
  using IV = beman::inplace_vector::inplace_vector<T, 10>;
  using IV0 = beman::inplace_vector::inplace_vector<T, 0>;

  struct InputIterator {
    using difference_type = std::ptrdiff_t;
    using value_type = T;
    using iterator_category = std::input_iterator_tag;

    T step;
    explicit constexpr InputIterator(T n) noexcept : step(n) {}
    constexpr InputIterator &operator++() noexcept {
      ++step.value;
      return *this;
    }
    constexpr InputIterator operator++(int) noexcept {
      auto prev = *this;
      ++(*this);
      return prev;
    }
    constexpr T operator*() const noexcept { return step; }
    constexpr bool operator==(InputIterator const &) const noexcept = default;
  };
  static_assert(std::input_iterator<InputIterator>);
};

using AllTypes =
    ::testing::Types<Trivial, NonTriviallyDefaultConstructible,
                     NonTriviallyCopyConstructible,
                     NonTriviallyMoveConstructible, NonTriviallyCopyAssignable,
                     NonTriviallyMoveAssignable, TriviallyAssignable,
                     TriviallyDestructible, NonTrivial>;
TYPED_TEST_SUITE(InplaceVectorSMFs, AllTypes);

// [containers.sequences.inplace.vector.cons]
TYPED_TEST(InplaceVectorSMFs, ConsDefault) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;

  IV v;
  EXPECT_EQ(v.size(), 0u);

  IV0 v0;
  EXPECT_EQ(v0.size(), 0u);

  if constexpr (std::is_trivial_v<TypeParam>) {
    constexpr IV cv;
    static_assert(cv.size() == 0u);

    constexpr IV0 cv0;
    static_assert(cv0.size() == 0u);
  }
}

// [containers.sequences.inplace.vector.cons]
TYPED_TEST(InplaceVectorSMFs, ConsSize) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;
  using T = TypeParam;

  IV v(IV::capacity());
  EXPECT_EQ(v.size(), IV::capacity());
  for (auto const &elem : v) {
    EXPECT_EQ(elem, T{});
  }

  IV0 v0(0u);
  EXPECT_EQ(v0.size(), 0u);

  if constexpr (false && /* TODO */ std::is_trivial_v<TypeParam>) {
    constexpr IV cv(IV::capacity());
    static_assert(IV::capacity());
    constexpr auto default_inserted = [](IV const &v) {
      for (auto const &elem : v) {
        if (elem != T{}) {
          return false;
        }
      }
      return true;
    };
    static_assert(default_inserted(cv));

    constexpr IV0 cv0(0);
    static_assert(cv0.size() == 0u);
  }

  // [containers.sequences.inplace.vector.overview]
  // 5. Any member function of inplace_vector<T, N> that would cause the size to
  // exceed N throws an exception of type bad_alloc.
  EXPECT_THROW(IV v(IV::capacity() + 1u), std::bad_alloc);
  EXPECT_THROW(IV0 v0(1u), std::bad_alloc);
}

// [containers.sequences.inplace.vector.cons]
TYPED_TEST(InplaceVectorSMFs, ConsSizeValue) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;
  using T = TypeParam;

  IV v(IV::capacity(), T{42});
  // [sequence.reqmts]
  EXPECT_EQ(static_cast<IV::size_type>(std::distance(v.begin(), v.end())),
            IV::capacity());
  for (auto const &elem : v) {
    EXPECT_EQ(elem, T{42});
  }

  IV0 v0(0u, T{42});
  EXPECT_EQ(v0.size(), 0u);

  if constexpr (false && /* TODO */ std::is_trivial_v<TypeParam>) {
    constexpr IV cv(IV::capacity(), T{42});
    // [sequence.reqmts]
    static_assert(std::distance(v.begin(), v.end()) == IV::capacity());
    constexpr auto all_of = [](IV const &v, T const &x) {
      for (auto const &elem : v) {
        if (elem != x) {
          return false;
        }
      }
      return true;
    };
    static_assert(all_of(cv, T{42}));

    constexpr IV0 cv0(0);
    static_assert(cv0.size() == 0u);
  }

  // [containers.sequences.inplace.vector.overview]
  // 5. Any member function of inplace_vector<T, N> that would cause the size to
  // exceed N throws an exception of type bad_alloc.
  EXPECT_THROW(IV v(IV::capacity() + 1u, T{5}), std::bad_alloc);
  EXPECT_THROW(IV0 v0(1u, T{5}), std::bad_alloc);
}

// [containers.sequences.inplace.vector.cons]
TYPED_TEST(InplaceVectorSMFs, ConsIterator) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;
  using T = TypeParam;
  using InputIterator = TestFixture::InputIterator;

  constexpr auto first = InputIterator{T{0}};
  constexpr auto last = InputIterator{T{5}};
  IV v(first, last);
  EXPECT_EQ(std::distance(v.begin(), v.end()), std::distance(first, last));
  for (int i = 0; i < v.size(); ++i) {
    EXPECT_EQ(v[i], T{i});
  }

  IV0 v0(first, first);
  EXPECT_EQ(v0.size(), 0u);

  if constexpr (false && /* TODO */ std::is_trivial_v<TypeParam>) {
    constexpr IV cv(first, last);
    static_assert(std::distance(cv.begin(), cv.end()) ==
                  std::distance(first, last));
    static_assert(cv[0] == T{0});
    static_assert(cv[1] == T{1});
    static_assert(cv[2] == T{2});
    static_assert(cv[3] == T{3});
    static_assert(cv[4] == T{4});

    constexpr IV0 cv0(first, first);
    static_assert(cv0.size() == 0u);
  }

  // [containers.sequences.inplace.vector.overview]
  // 5. Any member function of inplace_vector<T, N> that would cause the size to
  // exceed N throws an exception of type bad_alloc.
  EXPECT_THROW(IV v(first, InputIterator{T{IV::capacity() + 1}}),
               std::bad_alloc);
  EXPECT_THROW(IV0 v0(first, last), std::bad_alloc);
}

// [containers.sequences.inplace.vector.cons]
#if defined(__cpp_lib_containers_ranges)
TYPED_TEST(InplaceVectorSMFs, ConsRange) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;
  using T = TypeParam;
  using Range = std::array<T, IV::capacity()>;
  using Range0 = std::array<T, 0>;
  using Range_too_large = std::array<T, IV::capacity() + 1>;

  Range r;
  r.fill(T{42});
  IV v(std::from_range, r);
  EXPECT_EQ(v.size(), r.size());
  for (std::size_t idx = 0u; idx < v.size(); ++idx) {
    EXPECT_EQ(v[idx], r[idx]);
  }

  constexpr Range0 const r0;
  IV0 v0(std::from_range, r0);
  EXPECT_EQ(v0.size(), 0u);

  if constexpr (/*false && /* TODO */ std::is_trivial_v<TypeParam>) {
    constexpr Range cr{42, 99, 3, 4, 7, 1024};
    constexpr IV cv(std::from_range, r);
    static_assert(v.size() == r.size());
    constexpr auto range_equal = [](auto const &r1, auto const &r2) {
      auto f1 = std::begin(r1);
      auto f2 = std::begin(r2);
      auto const l1 = std::end(r1);
      auto const l2 = std::end(r2);
      for (; f1 != l1 && f2 != l2; ++f1, ++f2) {
        if (*f1 != *f2) {
          return false;
        }
      }
      return (f1 == l1) && (f2 == l2);
    };
    static_assert(range_equal(cv, r));

    constexpr IV0 cv0(std::from_range, r0);
    static_assert(cv0.size() == 0u);
  }

  // [containers.sequences.inplace.vector.overview]
  // 5. Any member function of inplace_vector<T, N> that would cause the size to
  // exceed N throws an exception of type bad_alloc.
  EXPECT_THROW(IV v(std::from_range, Range_too_large{}), std::bad_alloc);
  EXPECT_THROW(IV0 v0(std::from_range, r), std::bad_alloc);
}
#endif // defined(__cpp_lib_containers_ranges)

// [sequence.reqmts]
TYPED_TEST(InplaceVectorSMFs, ConsInitList) {
  using IV = TestFixture::IV;
  using IV0 = TestFixture::IV0;
  using T = TypeParam;

  std::initializer_list<T> const il = {T{0}, T{1}, T{2}, T{3}, T{4},
                                       T{5}, T{6}, T{7}, T{8}, T{9}};
  IV v(il);
  EXPECT_EQ(std::distance(v.begin(), v.end()),
            std::distance(il.begin(), il.end()));
  for (std::size_t i = 0; i < v.size(); ++i) {
    EXPECT_EQ(v[i], T(i));
  }

  constexpr std::initializer_list<T> il0 = {};
  IV0 v0(il0);
  EXPECT_EQ(v0.size(), 0u);

  // [containers.sequences.inplace.vector.overview]
  // 5. Any member function of inplace_vector<T, N> that would cause the size to
  // exceed N throws an exception of type bad_alloc.
  std::initializer_list<T> const il_too_large = {
      T{0}, T{1}, T{2}, T{3}, T{4}, T{5}, T{6}, T{7}, T{8}, T{9}, T{10}};
  EXPECT_THROW(IV v(il_too_large), std::bad_alloc);
  EXPECT_THROW(IV0 v0(il), std::bad_alloc);
}
#endif

} // namespace
