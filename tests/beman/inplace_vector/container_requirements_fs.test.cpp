#include "gtest/gtest.h"
#include <algorithm>
#include <array>
#include <gtest/gtest.h>
#include <new>

#include "gtest_setup.hpp"

namespace {

// [sequence.reqmts]
template <typename Param>
class SequenceContainerRequirements : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(SequenceContainerRequirements, IVAllTypes);

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

}; // namespace
