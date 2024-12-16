#include "beman/inplace_vector/inplace_vector.hpp"
#include "gtest/gtest.h"
#include <algorithm>
#include <concepts>
#include <iterator>
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

TEST(ContainerRequirements, ValueType) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // typename X::value_type
  //   Result: T
  //   Preconditions: T is Cpp17Erasable from X (see [container.alloc.reqmts]).
  EXPECT_TRUE((std::is_same_v<X::value_type, T>));
}

TEST(ContainerRequirements, Reference) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // typename X::reference
  //   Result: T&
  EXPECT_TRUE((std::is_same_v<X::reference, T &>));

  // typename X::const_reference
  //   Result: const T&
  EXPECT_TRUE((std::is_same_v<X::const_reference, const T &>));
}

TEST(ContainerRequirements, Iterator) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // typename X::iterator
  //   Result: A type that meets the forward iterator requirements
  //     ([forward.iterators]) with value type T. The type X​::​iterator is
  //     convertible to X​::​const_iterator.
  EXPECT_TRUE(std::forward_iterator<X::iterator>);
  EXPECT_TRUE(
      std::equality_comparable<X::iterator>); // required by
                                              // [forward.iterators], but not by
                                              // std::forward_iterator
  EXPECT_TRUE((std::is_same_v<decltype(*std::declval<X::iterator>()),
                              T &>)); // required by [forward.iterators], but
                                      // not by std::forward_iterator
  EXPECT_TRUE((std::is_convertible_v<X::iterator, X::const_iterator>));

  // typename X::const_iterator
  //   Result: A type that meets the requirements of a constant iterator and
  //     those of a forward iterator with value type T.
  EXPECT_TRUE(std::forward_iterator<X::const_iterator>);
  EXPECT_TRUE(
      std::equality_comparable<X::const_iterator>); // required by
                                                    // [forward.iterators], but
                                                    // not by
                                                    // std::forward_iterator
  EXPECT_TRUE((std::is_same_v<decltype(*std::declval<X::const_iterator>()),
                              const T &>)); // required by [forward.iterators],
                                            // but not by std::forward_iterator
}

TEST(ContainerRequirements, DifferenceType) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // typename X::difference_type
  //   Result: A signed integer type, identical to the difference type of
  //     X​::​iterator and X​::​const_iterator.
  EXPECT_TRUE(std::is_signed_v<X::difference_type>);
  EXPECT_TRUE(
      (std::is_same_v<X::difference_type,
                      std::iterator_traits<X::iterator>::difference_type>));
  EXPECT_TRUE((std::is_same_v<
               X::difference_type,
               std::iterator_traits<X::const_iterator>::difference_type>));
}

TEST(ContainerRequirements, SizeType) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // typename X::size_type
  //   Result: An unsigned integer type that can represent any non-negative
  //     value of X​::​difference_type.
  EXPECT_TRUE(std::is_unsigned_v<X::size_type>);
  EXPECT_GE(sizeof(X::size_type), sizeof(X::difference_type));
}

TEST(ContainerRequirements, DefaultConstructor) {
  using T = int;
  using X = inplace_vector<T, 5>;

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

TEST(ContainerRequirements, CopyConstructor) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // X u(v);
  // X u = v;
  //   Preconditions: T is Cpp17CopyInsertable into X (see below).
  //   Postconditions: u == v.
  //   Complexity: Linear.
  X v{0, 1, 2, 3, 4};
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

TEST(ContainerRequirements, MoveConstructor) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // X u(rv);
  // X u = rv;
  //   Postconditions: u is equal to the value that rv had before this
  //     construction.
  //   Complexity: Linear.
  X v{0, 1, 2, 3, 4};
  auto const rv = [v]() { return v; };
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

TEST(ContainerRequirements, CopyAssignment) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // t = v;
  //   Result: X&.
  //   Postconditions: t == v.
  //   Complexity: Linear.
  X const v{0, 1, 2, 3, 4};
  X t;
  t = v;
  EXPECT_TRUE((std::is_same_v<decltype(t = v), X &>));
  EXPECT_EQ(t, v);
  // How to test complexity?
}

struct Counting {
  static int num_objects;
  int value = 0;
  Counting() noexcept { ++num_objects; }
  Counting(int val) noexcept : value(val) { ++num_objects; }
  Counting(Counting const &) noexcept { ++num_objects; }
  ~Counting() { --num_objects; }
  Counting &operator=(Counting const &) noexcept {
    ++num_objects;
    return *this;
  }
  bool operator==(Counting const &) const noexcept = default;
};
int Counting::num_objects;

TEST(ContainerRequirements, MoveAssignment) {
  Counting::num_objects = 0;
  using X = inplace_vector<Counting, 5>;

  // t = rv
  //   Result: X&.
  //   Effects: All existing elements of t are either move assigned to or
  //     destroyed.
  //   Postconditions: If t and rv do not refer to the same object, t
  //     is equal to the value that rv had before this assignment.
  //   Complexity: Linear.
  X const v{0, 1, 2, 3};
  auto const rv = [&v]() { return v; };
  ASSERT_EQ(Counting::num_objects, v.size());
  X t(5);
  ASSERT_EQ(Counting::num_objects, v.size() + t.size());
  t = rv();
  EXPECT_TRUE((std::is_same_v<decltype(t = rv()), X &>));
  EXPECT_EQ(Counting::num_objects, 2 * v.size()); // t[4] has been destroyed
  EXPECT_EQ(t, v);
  // How to test complexity?
}

TEST(ContainerRequirements, Destructor) {
  Counting::num_objects = 0;
  using X = inplace_vector<Counting, 5>;

  // a.~X()
  //   Result: void.
  //   Effects: Destroys every element of a; any memory obtained is deallocated.
  //   Complexity: Linear.
  alignas(X) std::byte storage[sizeof(X)];
  X *pa = new (static_cast<void *>(storage)) X(5);
  X &a = *pa;
  ASSERT_EQ(Counting::num_objects, 5);
  a.~X();
  EXPECT_TRUE(std::is_void_v<decltype(a.~X())>);
  EXPECT_EQ(Counting::num_objects, 0);
  // How to test complexity?
}

TEST(ContainerRequirements, Begin) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // b.begin()
  //   Result: iterator; const_iterator for constant b.
  //   Returns: An iterator referring to the first element in the container.
  ///  Complexity: Constant.
  // b.cbegin()
  //   Result: const_iterator.
  //   Returns: const_cast<X const&>(b).begin()
  //   Complexity: Constant.

  for (X::size_type n : {0, 1, 4, 5}) {
    X b(n);
    X const cb(n);
    EXPECT_TRUE((std::is_same_v<decltype(b.begin()), X::iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(cb.begin()), X::const_iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(b.cbegin()), X::const_iterator>));
    EXPECT_EQ(b.cbegin(), const_cast<X const &>(b).begin());
    if (n > 0) {
      EXPECT_EQ(std::addressof(*b.begin()), std::addressof(b.data()[0]));
      EXPECT_EQ(std::addressof(*cb.begin()), std::addressof(cb.data()[0]));
      EXPECT_EQ(std::addressof(*b.cbegin()), std::addressof(b.data()[0]));
    }
    // How to test complexity?
  }
}

TEST(ContainerRequirements, End) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // b.end()
  //   Result: iterator; const_iterator for constant b.
  //   Returns: An iterator which is the past-the-end value for the container.
  ///  Complexity: Constant.
  // b.cend()
  //   Result: const_iterator.
  //   Returns: const_cast<X const&>(b).end()
  //   Complexity: Constant.

  for (X::size_type n : {0, 1, 4, 5}) {
    X b(n);
    X const cb(n);
    EXPECT_TRUE((std::is_same_v<decltype(b.end()), X::iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(cb.end()), X::const_iterator>));
    EXPECT_TRUE((std::is_same_v<decltype(b.cend()), X::const_iterator>));
    EXPECT_EQ(b.end(), const_cast<X const &>(b).end());
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

TEST(ContainerRequirements, Ordering) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // i <=> j
  //   Result: strong_ordering.
  //   Constraints: X​::​iterator meets the random access iterator
  //   requirements.
  //   Complexity: Constant.
  EXPECT_TRUE(std::random_access_iterator<X::iterator>);
  EXPECT_TRUE(std::random_access_iterator<X::const_iterator>);
  EXPECT_TRUE((std::is_same_v<decltype(std::declval<X::iterator>() <=>
                                       std::declval<X::iterator>()),
                              std::strong_ordering>));
  EXPECT_TRUE((std::is_same_v<decltype(std::declval<X::iterator>() <=>
                                       std::declval<X::const_iterator>()),
                              std::strong_ordering>));
  EXPECT_TRUE((std::is_same_v<decltype(std::declval<X::const_iterator>() <=>
                                       std::declval<X::iterator>()),
                              std::strong_ordering>));
  EXPECT_TRUE((std::is_same_v<decltype(std::declval<X::const_iterator>() <=>
                                       std::declval<X::const_iterator>()),
                              std::strong_ordering>));

  // Alternative:
  // EXPECT_TRUE((requires(X::iterator i, X::iterator j) {
  //   { i <=> j } -> std::same_as<std::strong_ordering>;
  // }));

  // How to test complexity?
}

TEST(ContainerRequirements, Equality) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // c == b
  //   Preconditions: T meets the Cpp17EqualityComparable requirements.
  //   Result: bool.
  //   Returns: equal(c.begin(), c.end(), b.begin(), b.end())
  //   [Note 1: The algorithm equal is defined in [alg.equal]. — end note]
  //   Complexity: Constant if c.size() != b.size(), linear otherwise.
  //   Remarks: == is an equivalence relation.
  // c != b
  //   Effects: Equivalent to !(c == b).
  X const values[]{{0, 1, 2, 3, 4}, {0, 1, 2, 3}, {0, 0, 0, 0}};
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

TEST(ContainerRequirements, Swap) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // t.swap(s)
  //   Result: void.
  //   Effects: Exchanges the contents of t and s.
  //   Complexity: Linear.
  // swap(t, s)
  //   Effects: Equivalent to t.swap(s).

  X const t_proto{0, 1, 2, 3, 4};
  X const s_proto{42, 99, 7};
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

TEST(ContainerRequirements, Size) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // c.size()
  //   Result: size_type.
  //   Returns: distance(c.begin(), c.end()), i.e., the number of elements in
  //     the container.
  //   Complexity: Constant.
  //   Remarks: The number of elements is
  //     defined by the rules of constructors, inserts, and erases.

  X::size_type const values[]{0, X::max_size() > 0 ? 1 : 0, X::max_size() / 2,
                              X::max_size() > 0 ? X::max_size() - 1 : 0,
                              X::max_size()};
  for (X::size_type n : values) {
    X c(n);
    EXPECT_TRUE((std::is_same_v<decltype(c.size()), X::size_type>));
    EXPECT_EQ(c.size(), std::distance(c.begin(), c.end()));
  }
  // How to test complexity?
}

TEST(ContainerRequirements, MaxSize) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // c.max_size()
  //   Result: size_type.
  //   Returns: distance(begin(), end()) for the largest possible container.
  //   Complexity: Constant.
  X c(X::capacity());
  EXPECT_TRUE((std::is_same_v<decltype(c.max_size()), X::size_type>));
  EXPECT_EQ(c.max_size(), std::distance(c.begin(), c.end()));
  // How to test complexity?
}

TEST(ContainerRequirements, Empty) {
  using T = int;
  using X = inplace_vector<T, 5>;

  // c.empty()
  //   Result: bool.
  //   Returns: c.begin() == c.end()
  //   Complexity: Constant.
  //   Remarks: If the container is empty, then c.empty() is true.}

  X::size_type const values[]{0, X::max_size() > 0 ? 1 : 0, X::max_size() / 2,
                              X::max_size() > 0 ? X::max_size() - 1 : 0,
                              X::max_size()};
  for (X::size_type n : values) {
    X c(n);
    EXPECT_TRUE((std::is_same_v<decltype(c.empty()), bool>));
    EXPECT_EQ(c.empty(), c.begin() == c.end());
  }
  // How to test complexity?
}

// Still [container.reqmts] (23.2.2.2):
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
TEST(ContainerRequirements, NothrowClear) {
  using T = int;
  using X = inplace_vector<T, 5>;

  EXPECT_TRUE(noexcept(std::declval<X>().clear()));
}
TEST(ContainerRequirements, NothrowPopBack) {
  using T = int;
  using X = inplace_vector<T, 5>;

  X::size_type const values[]{0, X::max_size() > 0 ? 1 : 0, X::max_size() / 2,
                              X::max_size() > 0 ? X::max_size() - 1 : 0,
                              X::max_size()};
  // pop_back() has a narrow contract, therefore we cannot check noexcept().
  for (X::size_type n : values) {
    X c(n);
    if (n > 0) {
      EXPECT_NO_THROW(c.pop_back());
    }
  }
}
// - No copy constructor or assignment operator of a returned iterator throws an
//   exception.
TEST(ContainerRequirements, NothrowIterator) {
  using T = int;
  using X = inplace_vector<T, 5>;

  EXPECT_TRUE(std::is_nothrow_copy_constructible_v<X::iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_constructible_v<X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_move_constructible_v<X::iterator>);
  EXPECT_TRUE(std::is_nothrow_move_constructible_v<X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_assignable_v<X::iterator>);
  EXPECT_TRUE(std::is_nothrow_copy_assignable_v<X::const_iterator>);
  EXPECT_TRUE(std::is_nothrow_move_assignable_v<X::iterator>);
  EXPECT_TRUE(std::is_nothrow_move_assignable_v<X::const_iterator>);
}
// - No swap() function throws an exception.
//   --> Specified in [inplace.vector.overview]
// - No swap() function invalidates any references, pointers, or iterators
//   referring to the elements of the containers being swapped.
//   --> Waived by previous paragraph in [container.reqmts]

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
