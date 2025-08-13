#include <gtest/gtest.h>
#include <type_traits>

#include "beman/inplace_vector/inplace_vector.hpp"

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

using IVAllTypes = ::testing::Types<
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

template <typename Param> class IVBasicTest : public ::testing::Test {
public:
  using T = Param::value_type;
  inline static constexpr std::size_t N = Param::capacity;
  using X = beman::inplace_vector::inplace_vector<T, N>;
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

  struct InputIterator {
    static std::size_t num_deref;
    T value;
    using difference_type = std::ptrdiff_t;
    using value_type = T;

    constexpr InputIterator() noexcept : value{0} {}
    constexpr InputIterator(int i) noexcept : value{i} {}

    T operator*() const {
      ++num_deref;
      return value;
    };

    InputIterator &operator++() {
      ++value.value;
      return *this;
    };
    void operator++(int) { ++*this; }

    constexpr bool operator==(InputIterator other) noexcept {
      return value.value == other.value.value;
    }
  };
  static_assert(std::input_iterator<InputIterator>);
};

template <typename Param>
std::size_t IVBasicTest<Param>::InputIterator::num_deref;

#if BEMAN_INPLACE_VECTOR_NO_EXCEPTIONS()
#define SAFE_EXPECT_THROW(x, y)                                                \
  do {                                                                         \
  } while (0)
#else
#define SAFE_EXPECT_THROW(x, y) EXPECT_THROW(x, y)
#endif
