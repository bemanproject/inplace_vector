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

}; // namespace
