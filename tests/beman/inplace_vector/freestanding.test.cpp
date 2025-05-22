#include <array>
#include <beman/inplace_vector/inplace_vector.hpp>
#include <gtest/gtest.h>

// constexpr void assign(InputIterator first, InputIterator last);
template <typename, typename = std::void_t<>>
struct has_assign_iterator : std::false_type {};
template <typename T>
struct has_assign_iterator<T, std::void_t<decltype(std::declval<T>().assign(
                                  std::declval<typename T::iterator>(),
                                  std::declval<typename T::iterator>()))>>
    : std::true_type {};

// constexpr void assign_range(R&& rg);
template <typename T, typename R, typename = void>
struct has_assign_range : std::false_type {};
template <typename T, typename R>
struct has_assign_range<T, R,
                        std::void_t<decltype(std::declval<T &>().assign_range(
                            std::declval<R &&>()))>> : std::true_type {};

// constexpr void assign(size_type n, const T& u);
template <typename T, typename = void>
struct has_assign_size : std::false_type {};
template <typename T>
struct has_assign_size<T, std::void_t<decltype(std::declval<T>().assign(
                              std::declval<typename T::size_type>(),
                              std::declval<typename T::const_reference>()))>>
    : std::true_type {};

// constexpr void assign(initializer_list il);
template <typename T, typename = void>
struct has_assign_initializer : std::false_type {};
template <typename T>
struct has_assign_initializer<
    T, std::void_t<decltype(std::declval<T>().assign(
           std::declval<std::initializer_list<typename T::value_type>>()))>>
    : std::true_type {};

// constexpr void resize(size_type sz);
template <typename T, typename = void> struct has_resize : std::false_type {};
template <typename T>
struct has_resize<T, std::void_t<decltype(std::declval<T>().resize(
                         std::declval<typename T::size_type>()))>>
    : std::true_type {};

// constexpr void resize(size_type sz, const T& c);
template <typename T, typename = void>
struct has_resize_ref : std::false_type {};
template <typename T>
struct has_resize_ref<T, std::void_t<decltype(std::declval<T>().resize(
                             std::declval<typename T::size_type>(),
                             std::declval<typename T::const_reference>()))>>
    : std::true_type {};

// void reserve(size_type n);
template <typename T, typename = void> struct has_reserve : std::false_type {};
template <typename T>
struct has_reserve<T, std::void_t<decltype(std::declval<T>().reserve(
                          std::declval<typename T::size_type>()))>>
    : std::true_type {};

// constexpr reference at(size_type n);
template <typename T, typename = void> struct has_at : std::false_type {};
template <typename T>
struct has_at<T, std::void_t<decltype(std::declval<T>().at(
                     std::declval<typename T::size_type>()))>>
    : std::true_type {};

// constexpr const_reference at(size_type n) const;
template <typename T, typename = void> struct has_const_at : std::false_type {};
template <typename T>
struct has_const_at<T, std::void_t<decltype(std::declval<const T>().at(
                           std::declval<typename T::size_type>()))>>
    : std::true_type {};

// emplace_back(Args&&... args);
template <typename T, typename = void>
struct has_emplace_back : std::false_type {};
template <typename T>
struct has_emplace_back<T, std::void_t<decltype(std::declval<T>().emplace_back(
                               std::declval<typename T::value_type>()))>>
    : std::true_type {};

// constexpr reference push_back(const T& x);
template <typename T, typename = void>
struct has_push_back_const : std::false_type {};
template <typename T>
struct has_push_back_const<T,
                           std::void_t<decltype(std::declval<T>().push_back(
                               std::declval<typename T::const_reference>()))>>
    : std::true_type {};

// constexpr reference push_back(T&& x);
template <typename T, typename = void>
struct has_push_back_rv : std::false_type {};
template <typename T>
struct has_push_back_rv<T, std::void_t<decltype(std::declval<T>().push_back(
                               std::declval<typename T::value_type>()))>>
    : std::true_type {};

// constexpr void append_range(R&& rg);
template <typename T, typename R, typename = void>
struct has_append_range : std::false_type {};
template <typename T, typename R>
struct has_append_range<
    T, R,
    std::void_t<decltype(std::declval<T>().append_range(std::declval<R &&>()))>>
    : std::true_type {};

// constexpr iterator emplace(const_iterator position, Args&&... args);
template <typename T, typename = void> struct has_emplace : std::false_type {};
template <typename T>
struct has_emplace<T, std::void_t<decltype(std::declval<T>().emplace(
                          std::declval<typename T::const_iterator>(),
                          std::declval<typename T::value_type>()))>>
    : std::true_type {};

// constexpr iterator insert(const_iterator position, const T& x);
template <typename T, typename = void>
struct has_insert_const : std::false_type {};
template <typename T>
struct has_insert_const<T, std::void_t<decltype(std::declval<T>().insert(
                               std::declval<typename T::const_iterator>(),
                               std::declval<typename T::const_reference>()))>>
    : std::true_type {};

// constexpr iterator insert(const_iterator position, T&& x);
template <typename T, typename = void>
struct has_insert_rv : std::false_type {};
template <typename T>
struct has_insert_rv<T, std::void_t<decltype(std::declval<T>().insert(
                            std::declval<typename T::const_iterator>(),
                            std::declval<typename T::value_type>()))>>
    : std::true_type {};

// constexpr iterator insert(const_iterator position, size_type n, const T& x);
template <typename T, typename = void>
struct has_insert_size : std::false_type {};
template <typename T>
struct has_insert_size<T, std::void_t<decltype(std::declval<T>().insert(
                              std::declval<typename T::const_iterator>(),
                              std::declval<typename T::size_type>(),
                              std::declval<typename T::const_reference>()))>>
    : std::true_type {};

// constexpr iterator insert(const_iterator position, InputIterator first,
// InputIterator last);
template <typename T, typename = void>
struct has_insert_iterator : std::false_type {};
template <typename T>
struct has_insert_iterator<T, std::void_t<decltype(std::declval<T>().insert(
                                  std::declval<typename T::const_iterator>(),
                                  std::declval<typename T::iterator>(),
                                  std::declval<typename T::iterator>()))>>
    : std::true_type {};

// InputIterator last); constexpr iterator insert_range(const_iterator position,
// R&& rg);
template <typename T, typename R, typename = void>
struct has_insert_range : std::false_type {};
template <typename T, typename R>
struct has_insert_range<
    T, R,
    std::void_t<decltype(std::declval<T>().insert_range(
        std::declval<typename T::const_iterator>(), std::declval<R &&>()))>>
    : std::true_type {};

//  constexpr iterator insert(const_iterator position, initializer_list il);
template <typename T, typename = void>
struct has_insert_initializer : std::false_type {};
template <typename T>
struct has_insert_initializer<
    T, std::void_t<decltype(std::declval<T>().insert(
           std::declval<typename T::const_iterator>(),
           std::declval<std::initializer_list<typename T::value_type>>()))>>
    : std::true_type {};

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
  static_assert(has_assign_iterator<IV>::value);
  static_assert(!has_assign_iterator<FIV>::value);

  // constexpr void assign_range(R&& rg);
  static_assert(has_assign_range<IV, range>::value);
  static_assert(!has_assign_range<FIV, range>::value);

  // constexpr void assign(size_type n, const T& u);
  static_assert(has_assign_size<IV>::value);
  static_assert(!has_assign_size<FIV>::value);

  // constexpr void assign(initializer_list il);
  static_assert(has_assign_initializer<IV>::value);
  static_assert(!has_assign_initializer<FIV>::value);

  // constexpr void resize(size_type sz);
  static_assert(has_resize<IV>::value);
  static_assert(!has_resize<FIV>::value);

  // constexpr void resize(size_type sz, const T& c);
  static_assert(has_resize_ref<IV>::value);
  static_assert(!has_resize_ref<FIV>::value);

  // void reserve(size_type n);
  static_assert(has_reserve<IV>::value);
  static_assert(!has_reserve<FIV>::value);

  // constexpr reference at(size_type n);
  static_assert(has_at<IV>::value);
  static_assert(!has_at<FIV>::value);

  // constexpr const_reference at(size_type n) const;
  static_assert(has_const_at<IV>::value);
  static_assert(!has_const_at<FIV>::value);

  // emplace_back(Args&&... args);
  static_assert(has_emplace_back<IV>::value);
  static_assert(!has_emplace_back<FIV>::value);

  // constexpr reference push_back(const T& x);
  static_assert(has_push_back_const<IV>::value);
  static_assert(!has_push_back_const<FIV>::value);

  // constexpr reference push_back(T&& x);
  static_assert(has_push_back_rv<IV>::value);
  static_assert(!has_push_back_rv<FIV>::value);

  // constexpr void append_range(R&& rg);
  static_assert(has_append_range<IV, range>::value);
  static_assert(!has_append_range<FIV, range>::value);

  // constexpr iterator emplace(const_iterator position, Args&&... args);
  static_assert(has_emplace<IV>::value);
  static_assert(!has_emplace<FIV>::value);

  // constexpr iterator insert(const_iterator position, const T& x);
  static_assert(has_insert_const<IV>::value);
  static_assert(!has_insert_const<FIV>::value);

  // constexpr iterator insert(const_iterator position, T&& x);
  static_assert(has_insert_rv<IV>::value);
  static_assert(!has_insert_rv<FIV>::value);

  // constexpr iterator insert(const_iterator position, size_type n, T& x);
  static_assert(has_insert_size<IV>::value);
  static_assert(!has_insert_size<FIV>::value);

  // constexpr iterator insert(const_iterator position, InputIterator first,
  static_assert(has_insert_iterator<IV>::value);
  static_assert(!has_insert_iterator<FIV>::value);

  // InputIterator last); constexpr iterator insert_range(const_iterator
  // position, R&& rg);
  static_assert(has_insert_range<IV, range>::value);
  static_assert(!has_insert_range<FIV, range>::value);

  //  constexpr iterator insert(const_iterator position, initializer_list il);
  static_assert(has_insert_initializer<IV>::value);
  static_assert(!has_insert_initializer<FIV>::value);

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

  for (auto i = int(device.capacity()); i > 0; --i) {
    const auto value = T{i - 1};
    EXPECT_EQ(device.back(), value);
    device.pop_back();
  }

  EXPECT_EQ(device.size(), 0);
}
