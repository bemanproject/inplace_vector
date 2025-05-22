#include "gtest/gtest.h"
#include <algorithm>
#include <array>
#include <gtest/gtest.h>
#include <new>

#include "gtest_setup.hpp"

namespace {

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
class ContainerRequirements : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(ContainerRequirements, IVAllTypes);

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
class ReversibleContainerRequirements : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(ReversibleContainerRequirements, IVAllTypes);

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
class SequenceContainerRequirements : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(SequenceContainerRequirements, IVAllTypes);

// X u(n, t);
// Preconditions: T is Cpp17CopyInsertable into X.
// Effects: Constructs a sequence container with n copies of t.
// Postconditions: distance(u.begin(), u.end()) == n is true.

// See: Constructors/SizedValue

// X u(i, j);
// Preconditions: T is Cpp17EmplaceConstructible into X from *i. For vector, if
// the iterator does not meet the Cpp17ForwardIterator requirements
// ([forward.iterators]), T is also Cpp17MoveInsertable into X. Effects:
// Constructs a sequence container equal to the range [i, j). Each iterator in
// the range [i, j) is dereferenced exactly once. Postconditions:
// distance(u.begin(), u.end()) == distance(i, j) is true.

// See: Constructors/CopyIter

// X(from_range, rg)
// Preconditions: T is Cpp17EmplaceConstructible into X from
// *ranges​::​begin(rg). For vector, if R models neither
// ranges​::​sized_range nor ranges​::​forward_range, T is also
// Cpp17MoveInsertable into X. Effects: Constructs a sequence container equal to
// the range rg. Each iterator in the range rg is dereferenced exactly once.
// Postconditions: distance(begin(), end()) == ranges​::​distance(rg) is
// true.

// See: Constructors/CopyRanges

// X(il)
// Effects: Equivalent to X(il.begin(), il.end()).

TYPED_TEST(SequenceContainerRequirements, ConstructorInitializerList) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  if (IV::capacity() == 0) {
    SAFE_EXPECT_THROW(IV({T{20}}), std::bad_alloc);
    return;
  }

  IV device({T{20}});

  IV correct;
  correct.emplace_back(20);
  EXPECT_EQ(device, correct);

  if (IV::capacity() == 1)
    return;

  device = IV({T{20}, T{21}});
  correct.emplace_back(21);

  EXPECT_EQ(device, correct);
}

// a = il
// Result: X&.
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable.
// Effects: Assigns the range [il.begin(), il.end()) into a. All existing
// elements of a are either assigned to or destroyed. Returns: *this.

TYPED_TEST(SequenceContainerRequirements, AssignInitializerList) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  if (IV::capacity() == 0) {
    IV device;
    SAFE_EXPECT_THROW(device = {T{52}}, std::bad_alloc);
    return;
  }

  IV device;
  device = {T{20}};
  EXPECT_EQ(device, IV{T{20}});
}

// a.emplace(p, args)
// Result: iterator.
// Preconditions: T is Cpp17EmplaceConstructible into X from args. For vector,
// inplace_vector, and deque, T is also Cpp17MoveInsertable into X and
// Cpp17MoveAssignable. Effects: Inserts an object of type T constructed with
// std​::​forward<Args>(args)... before p. [Note 1: args can directly or
// indirectly refer to a value in a. — end note] Returns: An iterator that
// points to the new element.

// See Modifiers/InsertEmplace

// a.insert(p, t)
// Result: iterator.
// Preconditions: T is Cpp17CopyInsertable into X. For vector, inplace_vector,
// and deque, T is also Cpp17CopyAssignable. Effects: Inserts a copy of t before
// p. Returns: An iterator that points to the copy of t inserted into a.

// See Modifiers/InsertSingleConstRef

// a.insert(p, rv)
// Result: iterator.
// Preconditions: T is Cpp17MoveInsertable into X. For vector, inplace_vector,
// and deque, T is also Cpp17MoveAssignable. Effects: Inserts a copy of rv
// before p. Returns: An iterator that points to the copy of rv inserted into a.

// See Modifiers/InsertSingleRV

// a.insert(p, n, t)
// Result: iterator.
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable.
// Effects: Inserts n copies of t before p.
// Returns: An iterator that points to the copy of the first element inserted
// into a, or p if n == 0.

// See Modifiers/InsertMulti

// a.insert(p, i, j)
// Result: iterator.
// Preconditions: T is Cpp17EmplaceConstructible into X from *i. For vector,
// inplace_vector, and deque, T is also Cpp17MoveInsertable into X, and T meets
// the Cpp17MoveConstructible, Cpp17MoveAssignable, and Cpp17Swappable
// ([swappable.requirements]) requirements. Neither i nor j are iterators into
// a. Effects: Inserts copies of elements in [i, j) before p. Each iterator in
// the range [i, j) shall be dereferenced exactly once. Returns: An iterator
// that points to the copy of the first element inserted into a, or p if i == j.

// See Modifiere/InsertItrRange

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

// See Modifiers/InsertRange

// a.insert(p, il)
// Effects: Equivalent to a.insert(p, il.begin(), il.end()).
// a.erase(q)
// Result: iterator.
// Preconditions: For vector, inplace_vector, and deque, T is
// Cpp17MoveAssignable. Effects: Erases the element pointed to by q. Returns: An
// iterator that points to the element immediately following q prior to the
// element being erased. If no such element exists, a.end() is returned.

// See Modifiers/InsertInitList

// a.erase(q1, q2)
// Result: iterator.
// Preconditions: For vector, inplace_vector, and deque, T is
// Cpp17MoveAssignable. Effects: Erases the elements in the range [q1, q2).
// Returns: An iterator that points to the element pointed to by q2 prior to any
// elements being erased. If no such element exists, a.end() is returned.

// See Modifiers/EraseRange

// a.clear()
// Result: void
// Effects: Destroys all elements in a. Invalidates all references, pointers,
// and iterators referring to the elements of a and may invalidate the
// past-the-end iterator. Postconditions: a.empty() is true. Complexity: Linear.

TYPED_TEST(SequenceContainerRequirements, Clear) {
  using IV = TestFixture::IV;

  auto device = this->unique();
  device.clear();
  EXPECT_EQ(device, IV{});
  EXPECT_TRUE(device.empty());
}

#if !BEMAN_INPLACE_VECTOR_FREESTANDING_DELETED()
// a.assign(i, j)
// Result: void
// Preconditions: T is Cpp17EmplaceConstructible into X from *i and assignable
// from *i. For vector, if the iterator does not meet the forward iterator
// requirements ([forward.iterators]), T is also Cpp17MoveInsertable into X.
// Neither i nor j are iterators into a.
// Effects: Replaces elements in a with a copy of [i, j). Invalidates all
// references, pointers and iterators referring to the elements of a. For vector
// and deque, also invalidates the past-the-end iterator. Each iterator in the
// range [i, j) is dereferenced exactly once.

TYPED_TEST(SequenceContainerRequirements, AssignIterRange) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;
  using InputIterator = TestFixture::InputIterator;

  {
    auto device = this->unique();

    const auto correct = this->unique();

    device.assign(correct.begin(), correct.end());
    EXPECT_EQ(device, correct);

    std::array<T, IV::capacity() + 1> ref{};
    SAFE_EXPECT_THROW(device.assign(ref.begin(), ref.end()), std::bad_alloc);
  }

  {
    IV device;
    device.assign(InputIterator{0}, InputIterator{IV::max_size()});
    EXPECT_EQ(device.size(), IV::max_size());
    // Each iterator in the range [i, j) is dereferenced exactly once.
    if (!device.empty()) {
      EXPECT_EQ(device.back(), T{static_cast<int>(IV::max_size() - 1)});
    }

    // [containers.sequences.inplace.vector.overview]
    // 5. Any member function of inplace_vector<T, N> that would cause the size
    // to exceed N throws an exception of type bad_alloc.
    SAFE_EXPECT_THROW(
        device.assign(InputIterator{0}, InputIterator{IV::max_size() + 1}),
        std::bad_alloc);
  }
}

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

TYPED_TEST(SequenceContainerRequirements, AssignRange) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();
  auto correct = this->unique();

  device.assign_range(correct);
  EXPECT_EQ(device, correct);

  std::array<T, IV::capacity() + 1> ref;
  std::copy(correct.begin(), correct.end(), ref.begin());
  ref.back() = T{5};
  SAFE_EXPECT_THROW(device.assign_range(ref), std::bad_alloc);
}

// a.assign(il)
// Effects: Equivalent to a.assign(il.begin(), il.end()).

TYPED_TEST(SequenceContainerRequirements, AssignFuncInitializerList) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();

  if (device.capacity() == 0) {
    SAFE_EXPECT_THROW(device.assign({T{50}}), std::bad_alloc);
    return;
  }

  device.assign({T{50}});
  EXPECT_EQ(device, IV{T{50}});
}

// a.assign(n, t)
// Result: void
// Preconditions: T is Cpp17CopyInsertable into X and Cpp17CopyAssignable. t is
// not a reference into a.
// Effects: Replaces elements in a with n copies of t.
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
// amortized constant time.

TYPED_TEST(SequenceContainerRequirements, AssignMulti) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();
  device.assign(0, T{6312});

  EXPECT_EQ(device, IV());

  if (device.capacity() > 0) {
    device.assign(1, T{6312});

    EXPECT_EQ(device, IV{T{6312}});

    device.assign(device.capacity(), T{5972});
    EXPECT_EQ(device, IV(IV::capacity(), T{5972}));
  }

  device.clear();
  SAFE_EXPECT_THROW(device.assign(device.capacity() + 1, T{12}),
                    std::bad_alloc);
}
#endif

// a.front()
// Result: reference; const_reference for constant a.
// Returns: *a.begin()

TYPED_TEST(SequenceContainerRequirements, Front) {
  using X = TestFixture::X;
  if constexpr (X::capacity() == 0) {
    return;
  }

  auto a = this->unique();
  auto const ca = this->unique();

  EXPECT_TRUE((std::is_same_v<decltype(a.front()), typename X::reference>));
  EXPECT_TRUE(
      (std::is_same_v<decltype(ca.front()), typename X::const_reference>));
  EXPECT_EQ(a.front(), *a.begin());
  EXPECT_EQ(ca.front(), *ca.begin());
}

// a.back()
// Result: reference; const_reference for constant a.
// Effects: Equivalent to:
// auto tmp = a.end();
// --tmp;
// return *tmp;

TYPED_TEST(SequenceContainerRequirements, Back) {
  using X = TestFixture::X;
  if constexpr (X::capacity() == 0) {
    return;
  }

  auto a = this->unique();
  auto const ca = this->unique();

  EXPECT_TRUE((std::is_same_v<decltype(a.back()), typename X::reference>));
  EXPECT_TRUE(
      (std::is_same_v<decltype(ca.back()), typename X::const_reference>));
  EXPECT_EQ(a.back(), *(a.end() - 1));
  EXPECT_EQ(ca.back(), *(ca.end() - 1));
}

// a.emplace_back(args)
// Returns: a.back().

// See: Modifiers/EmplaceBack

// a.push_back(t)
// Result: void
// Preconditions: T is Cpp17CopyInsertable into X.
// Effects: Appends a copy of t.

// See: Modifiers/EmplaceBack

// a.push_back(rv)
// Result: void
// Preconditions: T is Cpp17MoveInsertable into X.
// Effects: Appends a copy of rv.

// See: Modifiers/PushBackRV

// a.pop_back()
// Result: void
// Preconditions: a.empty() is false.
// Effects: Destroys the last element.

// See: Modifiers/PopBack

// a[n]
// Result: reference; const_reference for constant
// Effects: Equivalent to: return *(a.begin() + n);

TYPED_TEST(SequenceContainerRequirements, ElementAccess) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();

  for (auto i = 0ul; i < device.size(); ++i)
    EXPECT_EQ(device[i], *(device.begin() + i));
}

#if !BEMAN_INPLACE_VECTOR_FREESTANDING_DELETED()
// a.at(n)
// Result: reference; const_reference for constant a
// Returns: *(a.begin() + n)
// Throws: out_of_range if n >= a.size().

TYPED_TEST(SequenceContainerRequirements, ElementAccessAt) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  auto device = this->unique();

  for (auto i = 0ul; i < device.size(); ++i) {
    EXPECT_EQ(device.at(i), *(device.begin() + i));
  }

  SAFE_EXPECT_THROW(device.at(IV::capacity()), std::out_of_range);
}

TYPED_TEST(SequenceContainerRequirements, ElementAccessAtConst) {
  using IV = TestFixture::IV;
  using T = TestFixture::T;

  const auto device = this->unique();

  for (auto i = 0ul; i < device.size(); ++i) {
    EXPECT_EQ(device.at(i), *(device.begin() + i));
  }

  SAFE_EXPECT_THROW(device.at(IV::capacity()), std::out_of_range);
}
#endif

}; // namespace
