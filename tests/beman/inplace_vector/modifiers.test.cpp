#include <numeric>

#include "gtest_setup.hpp"
#include <gtest/gtest.h>

namespace {
// 23.3.14.5 Modifiers [inplace.vector.modifiers]
template <typename Param> class Modifiers : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Modifiers, IVAllTypes);

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
  SAFE_EXPECT_THROW(device.insert(device.begin(), val), std::bad_alloc);
  EXPECT_EQ(device, reference);

  SAFE_EXPECT_THROW(device.insert(device.begin(), val), std::bad_alloc);
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

  SAFE_EXPECT_THROW(device.insert(device.begin(), T{272}), std::bad_alloc);
  EXPECT_EQ(device, reference);

  SAFE_EXPECT_THROW(device.insert(device.begin(), T{272}), std::bad_alloc);
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

  SAFE_EXPECT_THROW(device.emplace(device.begin(), 272), std::bad_alloc);
  EXPECT_EQ(device, reference);

  SAFE_EXPECT_THROW(device.emplace(device.begin(), 272), std::bad_alloc);
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
  SAFE_EXPECT_THROW(device.insert(device.begin(), 1, {2538}), std::bad_alloc);
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
  SAFE_EXPECT_THROW(full.insert(full.begin(), {T{25}}), std::bad_alloc);
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

  SAFE_EXPECT_THROW(
      device.insert_range(device.begin(), std::array<T, 1>{T{25}}),
      std::bad_alloc);
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
  SAFE_EXPECT_THROW(
      device.insert(device.begin(), single_array.begin(), single_array.end()),
      std::bad_alloc);
}

TEST(Modifiers, InsertItrString) {
  // test if insertion of nontrivial type iterator into inplace_vector dose not
  // modify the input values
  using T = std::string;
  using IV = beman::inplace_vector::inplace_vector<T, 10>;
  using V = std::vector<T>;

  const V vec_const{"1", "2", "3"};
  V vec = vec_const;
  IV device;

  auto res = device.insert(device.begin(), vec.begin(), vec.end());
  EXPECT_EQ(res, device.begin());
  EXPECT_EQ(device.size(), 3);
  EXPECT_EQ(device, IV({"1", "2", "3"}));
  EXPECT_EQ(vec, vec_const);

  auto res2 = device.insert(device.end(), vec_const.begin(), vec_const.end());
  EXPECT_EQ(res2, device.begin() + 3);
  EXPECT_EQ(device.size(), 6);
  EXPECT_EQ(device, IV({"1", "2", "3", "1", "2", "3"}));
  EXPECT_EQ(vec, vec_const);
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
  SAFE_EXPECT_THROW(device.push_back(val), std::bad_alloc);
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
  SAFE_EXPECT_THROW(device.push_back(val), std::bad_alloc);
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

  SAFE_EXPECT_THROW(device.emplace_back(0), std::bad_alloc);
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

  using IV = TestFixture::IV;
  using T = TestFixture::T;
  using size_type = IV::size_type;

  IV device;
  auto reference = this->unique();

  device.try_append_range(reference | std::views::take(0));
  EXPECT_EQ(device, IV());
  device.clear();

  EXPECT_EQ(device.try_append_range(reference), reference.end());
  EXPECT_EQ(device, reference);
  EXPECT_EQ(device.try_append_range(reference), reference.begin());
  device.clear();

  auto range = std::array<T, IV::capacity() + 1>{};
  std::copy_n(reference.begin(), IV::capacity(), range.begin());
  EXPECT_EQ(device.try_append_range(range), range.end() - 1);
  EXPECT_EQ(device, reference);
  device.clear();

  auto half_size = std::midpoint(size_type(0), reference.size());
  EXPECT_EQ(device.try_append_range(reference | std::views::take(half_size)),
            reference.begin() + half_size);
  EXPECT_EQ(device.try_append_range(reference | std::views::drop(half_size)),
            reference.end());
  EXPECT_EQ(device, reference);

  device.clear();

  EXPECT_EQ(device.try_append_range(reference | std::views::drop(half_size)),
            reference.end());
  EXPECT_EQ(device.try_append_range(reference), reference.begin() + half_size);
  device.clear();
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

  SAFE_EXPECT_THROW(device.reserve(device.capacity() + 1), std::bad_alloc);
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

  SAFE_EXPECT_THROW(device.reserve(device.capacity() + 1), std::bad_alloc);
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
}; // namespace
