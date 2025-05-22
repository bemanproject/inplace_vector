#include <array>
#include <beman/inplace_vector/inplace_vector.hpp>
#include <gtest/gtest.h>

// constexpr void assign(InputIterator first, InputIterator last);
template <typename T>
concept has_assign_iterator = requires(T t, T::iterator it) {
  { t.assign(it, it) } -> std::same_as<void>;
};

// constexpr void assign_range(R&& rg);
template <typename T, typename R>
concept has_assign_range = requires(T t, R rg) {
  { t.assign_range(rg) } -> std::same_as<void>;
};

// constexpr void assign(size_type n, const T& u);
template <typename T>
concept has_assign_size = requires(T t, T::size_type n, T::const_reference u) {
  { t.assign(n, u) } -> std::same_as<void>;
};

// constexpr void assign(initializer_list il);
template <typename T>
concept has_assign_initializer =
    requires(T t, std::initializer_list<typename T::value_type> il) {
      { t.assign(il) } -> std::same_as<void>;
    };

// constexpr void resize(size_type sz);
template <typename T>
concept has_resize = requires(T t, T::size_type sz) {
  { t.resize(sz) } -> std::same_as<void>;
};

// constexpr void resize(size_type sz, const T& c);
template <typename T>
concept has_resize_ref =
    requires(T t, typename T::size_type sz, T::const_reference c) {
      { t.resize(sz, c) } -> std::same_as<void>;
    };

// void reserve(size_type n);
template <typename T>
concept has_reserve = requires(T t, T::size_type n) {
  { t.reserve(n) } -> std::same_as<void>;
};

// constexpr reference at(size_type n);
template <typename T>
concept has_at = requires(T t, T::size_type n) {
  { t.at(n) } -> std::same_as<typename T::reference>;
};

// constexpr const_reference at(size_type n) const;
template <typename T>
concept has_const_at = requires(const T t, T::size_type n) {
  { t.at(n) } -> std::same_as<typename T::const_reference>;
};

// constexpr reference emplace_back(Args&&... args);
template <typename T>
concept has_emplace_back = requires(T t, T::value_type v) {
  { t.emplace_back(v) } -> std::same_as<typename T::reference>;
};

// constexpr reference push_back(const T& x);
template <typename T>
concept has_push_back_const = requires(T t, typename T::const_reference x) {
  { t.push_back(x) } -> std::same_as<typename T::reference>;
};

// constexpr reference push_back(T&& x);
template <typename T>
concept has_push_back_rv = requires(T t, typename T::value_type x) {
  {
    t.push_back(static_cast<typename T::value_type &&>(x))
  } -> std::same_as<typename T::reference>;
};

// constexpr void append_range(R&& rg);
template <typename T, typename R>
concept has_append_range = requires(T t, R &&r) {
  { t.append_range(r) } -> std::same_as<void>;
};

// constexpr iterator emplace(const_iterator position, Args&&... args);
template <typename T>
concept has_emplace = requires(T t, T::const_iterator it, T::value_type v) {
  { t.emplace(it, v) } -> std::same_as<typename T::iterator>;
};

// constexpr iterator insert(const_iterator position, const T& x);
template <typename T>
concept has_insert_const =
    requires(T t, T::const_iterator it, T::const_reference x) {
      { t.insert(it, x) } -> std::same_as<typename T::iterator>;
    };

// constexpr iterator insert(const_iterator position, T&& x);
template <typename T>
concept has_insert_rv = requires(T t, T::const_iterator it, T::value_type &&x) {
  {
    t.insert(it, static_cast<typename T::value_type &&>(x))
  } -> std::same_as<typename T::iterator>;
};

// constexpr iterator insert(const_iterator position, size_type n, const T& x);
template <typename T>
concept has_insert_size =
    requires(T t, T::const_iterator it, T::size_type n, T::const_reference x) {
      { t.insert(it, n, x) } -> std::same_as<typename T::iterator>;
    };

// constexpr iterator insert(const_iterator position, InputIterator first,
// InputIterator last);
template <typename T>
concept has_insert_iterator =
    requires(T t, T::const_iterator it, T::iterator first, T::iterator last) {
      { t.insert(it, first, last) } -> std::same_as<typename T::iterator>;
    };

// InputIterator last); constexpr iterator insert_range(const_iterator position,
// R&& rg);
template <typename T, typename R>
concept has_insert_range = requires(T t, T::const_iterator it, R &&r) {
  { t.insert_range(it, r) } -> std::same_as<typename T::iterator>;
};

//  constexpr iterator insert(const_iterator position, initializer_list il);
template <typename T>
concept has_insert_initializer =
    requires(T t, typename T::const_iterator it,
             std::initializer_list<typename T::value_type> il) {
      { t.insert(it, il) } -> std::same_as<typename T::iterator>;
    };

TEST(Freestanding, deleted) {

  using IV = beman::inplace_vector<int, 10>;
  using FIV = beman::freestanding::inplace_vector<int, 10>;

  using range = std::array<int, 10>;
  using input_iterator = std::istream_iterator<int>;
  using initializer_list = std::initializer_list<int>;

  IV::value_type v;

  // constexpr explicit inplace_vector(size_type n);
  static_assert(std::is_constructible_v<IV, std::size_t>);
  static_assert(!std::is_constructible_v<FIV, std::size_t>);

  // constexpr inplace_vector(size_type n, const T& value);
  static_assert(std::is_constructible_v<IV, std::size_t, IV::const_reference>);
  static_assert(
      !std::is_constructible_v<FIV, std::size_t, IV::const_reference>);

  // constexpr inplace_vector(InputIterator first, InputIterator last);
  static_assert(std::is_constructible_v<IV, input_iterator, input_iterator>);
  static_assert(!std::is_constructible_v<FIV, input_iterator, input_iterator>);

  // constexpr inplace_vector(from_range_t, R&& rg);
  static_assert(std::is_constructible_v<IV, beman::from_range_t, range>);
  static_assert(!std::is_constructible_v<FIV, beman::from_range_t, range>);

  // constexpr inplace_vector(initializer_list<T> il);
  static_assert(std::is_constructible_v<IV, initializer_list>);
  static_assert(!std::is_constructible_v<FIV, initializer_list>);

  // constexpr inplace_vector& operator=(initializer_list<T>);
  static_assert(std::is_assignable_v<IV &, initializer_list>);
  static_assert(!std::is_assignable_v<FIV &, initializer_list>);

  // constexpr void assign(InputIterator first, InputIterator last)
  static_assert(has_assign_iterator<IV>);
  static_assert(!has_assign_iterator<FIV>);

  // constexpr void assign_range(R&& rg);
  static_assert(has_assign_range<IV, range>);
  static_assert(!has_assign_range<FIV, range>);

  // constexpr void assign(size_type n, const T& u);
  static_assert(has_assign_size<IV>);
  static_assert(!has_assign_size<FIV>);

  // constexpr void assign(initializer_list il);
  static_assert(has_assign_initializer<IV>);
  static_assert(!has_assign_initializer<FIV>);

  // constexpr void resize(size_type sz);
  static_assert(has_resize<IV>);
  static_assert(!has_resize<FIV>);

  // constexpr void resize(size_type sz, const T& c);
  static_assert(has_resize_ref<IV>);
  static_assert(!has_resize_ref<FIV>);

  // void reserve(size_type n);
  static_assert(has_reserve<IV>);
  static_assert(!has_reserve<FIV>);

  // constexpr reference at(size_type n);
  static_assert(has_at<IV>);
  static_assert(!has_at<FIV>);

  // constexpr const_reference at(size_type n) const;
  static_assert(has_const_at<IV>);
  static_assert(!has_const_at<FIV>);

  // emplace_back(Args&&... args);
  static_assert(has_emplace_back<IV>);
  static_assert(!has_emplace_back<FIV>);

  // constexpr reference push_back(const T& x);
  static_assert(has_push_back_const<IV>);
  static_assert(!has_push_back_const<FIV>);

  // constexpr reference push_back(T&& x);
  static_assert(has_push_back_rv<IV>);
  static_assert(!has_push_back_rv<FIV>);

  // constexpr void append_range(R&& rg);
  static_assert(has_append_range<IV, range>);
  static_assert(!has_append_range<FIV, range>);

  // constexpr iterator emplace(const_iterator position, Args&&... args);
  static_assert(has_emplace<IV>);
  static_assert(!has_emplace<FIV>);

  // constexpr iterator insert(const_iterator position, const T& x);
  static_assert(has_insert_const<IV>);
  static_assert(!has_insert_const<FIV>);

  // constexpr iterator insert(const_iterator position, T&& x);
  static_assert(has_insert_rv<IV>);
  static_assert(!has_insert_rv<FIV>);

  // constexpr iterator insert(const_iterator position, size_type n, T& x);
  static_assert(has_insert_size<IV>);
  static_assert(!has_insert_size<FIV>);

  // constexpr iterator insert(const_iterator position, InputIterator first,
  static_assert(has_insert_iterator<IV>);
  static_assert(!has_insert_iterator<FIV>);

  // InputIterator last); constexpr iterator insert_range(const_iterator
  // position, R&& rg);
  static_assert(has_insert_range<IV, range>);
  static_assert(!has_insert_range<FIV, range>);

  //  constexpr iterator insert(const_iterator position, initializer_list il);
  static_assert(has_insert_initializer<IV>);
  static_assert(!has_insert_initializer<FIV>);

  EXPECT_TRUE(true);
}

TEST(Freestanding, usage) {
  using IV = beman::freestanding::inplace_vector<int, 10>;
  using T = IV::value_type;

  IV device;

  EXPECT_EQ(device.size(), 0);

  for (auto i = 0; i < device.capacity(); ++i) {
    const auto value = T{i};
    auto res = device.try_emplace_back(value);
    EXPECT_NE(res, nullptr);
    EXPECT_EQ(*res, value);
    EXPECT_EQ(device.back(), value);
  }

  EXPECT_EQ(device.size(), device.capacity());

  EXPECT_EQ(nullptr, device.try_emplace_back(T{}));

  EXPECT_EQ(device.size(), device.capacity());

  for (auto i = int(device.capacity()); i > 0; --i) {
    const auto value = T{i - 1};
    EXPECT_EQ(device.back(), value);
    device.pop_back();
  }

  device.pop_back(); // UB should be safe

  EXPECT_EQ(device.size(), 0);
}
