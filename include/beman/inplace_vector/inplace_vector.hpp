// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef BEMAN_INPLACE_VECTOR_INPLACE_VECTOR_HPP
#define BEMAN_INPLACE_VECTOR_INPLACE_VECTOR_HPP

#include <algorithm> // for rotate...
#include <array>
#include <compare>
#include <concepts>   // for lots...
#include <cstddef>    // for size_t
#include <cstdint>    // for fixed-width integer types
#include <cstdio>     // for assertion diagnostics
#include <cstdlib>    // for abort
#include <functional> // for less and equal_to
#include <iterator>   // for reverse_iterator and iterator traits
#include <limits>     // for numeric_limits
#include <memory>     // for destroy
#include <new>        // for operator new
#include <ranges>
#include <stdexcept>   // for length_error
#include <type_traits> // for aligned_storage and all meta-functions

// Artifact from previous implementation, can be used as hints for optimizer
#define IV_EXPECT(EXPR)

// beman::from_range_t
namespace beman {
struct from_range_t {};
inline constexpr from_range_t from_range;
}; // namespace beman

// Private utilities
namespace beman::details::inplace_vector {

// clang-format off
// Smallest unsigned integer that can represent values in [0, N].
template <size_t N>
using smallest_size_t
= std::conditional_t<(N < std::numeric_limits<uint8_t>::max()),  uint8_t,
    std::conditional_t<(N < std::numeric_limits<uint16_t>::max()), uint16_t,
    std::conditional_t<(N < std::numeric_limits<uint32_t>::max()), uint32_t,
    std::conditional_t<(N < std::numeric_limits<uint64_t>::max()), uint64_t,
                   size_t>>>>;
// clang-format on

// Index a random-access and sized range doing bound checks in debug builds
template <std::ranges::random_access_range Rng, std::integral Index>
static constexpr decltype(auto) index(Rng &&rng, Index i) noexcept
  requires(std::ranges::sized_range<Rng>)
{
  IV_EXPECT(static_cast<ptrdiff_t>(i) < std::ranges::size(rng));
  return std::begin(std::forward<Rng>(rng))[std::forward<Index>(i)];
}

// http://eel.is/c++draft/container.requirements.general#container.intro.reqmts-2
template <class Rng, class T>
concept container_compatible_range =
    std::ranges::input_range<Rng> &&
    std::convertible_to<std::ranges::range_reference_t<Rng>, T>;

template <typename T, std::size_t N>
concept satify_constexpr = N == 0 || std::is_trivial_v<T>;

template <typename T>
concept lessthan_comparable = requires(const T &a, const T &b) {
  { a < b } -> std::convertible_to<bool>;
};

} // namespace beman::details::inplace_vector

// Types implementing the `inplace_vector`'s storage
namespace beman::details::inplace_vector::storage {

// Storage for zero elements.
template <class T> struct zero_sized {
protected:
  using size_type = uint8_t;
  static constexpr T *storage_data() noexcept { return nullptr; }
  static constexpr size_type storage_size() noexcept { return 0; }
  static constexpr void
  unsafe_set_size([[maybe_unused]] size_t new_size) noexcept {
    IV_EXPECT(new_size == 0 &&
              "tried to change size of empty storage to non-zero value");
  }

public:
  constexpr zero_sized() = default;
  constexpr zero_sized(zero_sized const &) = default;
  constexpr zero_sized &operator=(zero_sized const &) = default;
  constexpr zero_sized(zero_sized &&) = default;
  constexpr zero_sized &operator=(zero_sized &&) = default;
  constexpr ~zero_sized() = default;
};

// Storage for trivial types.
template <class T, size_t N> struct trivial {
  static_assert(std::is_trivial_v<T>,
                "storage::trivial<T, C> requires Trivial<T>");
  static_assert(N != size_t{0}, "N  == 0, use zero_sized");

protected:
  using size_type = smallest_size_t<N>;

private:
  // If value_type is const, then const std::array of non-const elements:
  using array_based_storage =
      std::conditional_t<!std::is_const_v<T>, std::array<T, N>,
                         const std::array<std::remove_const_t<T>, N>>;
  alignas(alignof(T)) array_based_storage storage_data_{};
  size_type storage_size_ = 0;

protected:
  constexpr const T *storage_data() const noexcept {
    return storage_data_.data();
  }
  constexpr T *storage_data() noexcept { return storage_data_.data(); }
  constexpr size_type storage_size() const noexcept { return storage_size_; }
  constexpr void unsafe_set_size(size_t new_size) noexcept {
    IV_EXPECT(size_type(new_size) <= N && "new_size out-of-bounds [0, N]");
    storage_size_ = size_type(new_size);
  }

public:
  constexpr trivial() noexcept = default;
  constexpr trivial(trivial const &) noexcept = default;
  constexpr trivial &operator=(trivial const &) noexcept = default;
  constexpr trivial(trivial &&) noexcept = default;
  constexpr trivial &operator=(trivial &&) noexcept = default;
  constexpr ~trivial() = default;
};

template <class T, size_t N> struct raw_byte_based_storage {
  alignas(T) std::byte _d[sizeof(T) * N];
  constexpr T *storage_data(size_t i) noexcept {
    IV_EXPECT(i < N);
    return reinterpret_cast<T *>(_d) + i;
  }
  constexpr const T *storage_data(size_t i) const noexcept {
    IV_EXPECT(i < N);
    return reinterpret_cast<const T *>(_d) + i;
  }
};

/// Storage for non-trivial elements.
template <class T, size_t N> struct non_trivial {
  static_assert(!std::is_trivial_v<T>,
                "use storage::trivial for Trivial<T> elements");
  static_assert(N != size_t{0}, "use storage::zero for N==0");

protected:
  using size_type = smallest_size_t<N>;

private:
  using byte_based_storage = std::conditional_t<
      !std::is_const_v<T>, raw_byte_based_storage<T, N>,
      const raw_byte_based_storage<std::remove_const_t<T>, N>>;
  byte_based_storage storage_data_{}; // BUGBUG: test SIMD types
  size_type storage_size_ = 0;

protected:
  constexpr const T *storage_data() const noexcept {
    return storage_data_.storage_data(0);
  }
  constexpr T *storage_data() noexcept { return storage_data_.storage_data(0); }
  constexpr size_type storage_size() const noexcept { return storage_size_; }
  constexpr void unsafe_set_size(size_t new_size) noexcept {
    IV_EXPECT(size_type(new_size) <= N && "new_size out-of-bounds [0, N)");
    storage_size_ = size_type(new_size);
  }

public:
  constexpr non_trivial() noexcept = default;
  constexpr non_trivial(non_trivial const &) noexcept = default;
  constexpr non_trivial &operator=(non_trivial const &) noexcept = default;
  constexpr non_trivial(non_trivial &&) noexcept = default;
  constexpr non_trivial &operator=(non_trivial &&) noexcept = default;

  constexpr ~non_trivial()
    requires(std::is_trivially_destructible_v<T>)
  = default;
  constexpr ~non_trivial() {
    std::destroy(storage_data(), storage_data() + storage_size());
  }
};

// Selects the vector storage.
template <class T, size_t N>
using storage_for = std::conditional_t<
    !satify_constexpr<T, N>, non_trivial<T, N>,
    std::conditional_t<N == 0, zero_sized<T>, trivial<T, N>>>;

} // namespace beman::details::inplace_vector::storage

namespace beman {

template <typename IV>
concept has_constexpr_support =
    details::inplace_vector::satify_constexpr<typename IV::value_type,
                                              IV::capacity()>;

/// Dynamically-resizable fixed-N vector with inplace storage.
template <class T, size_t N>
struct inplace_vector
    : private details::inplace_vector::storage::storage_for<T, N> {
private:
  static_assert(std::is_nothrow_destructible_v<T>,
                "T must be nothrow destructible");
  using base_t = details::inplace_vector::storage::storage_for<T, N>;
  using base_t::storage_data;
  using base_t::storage_size;
  using base_t::unsafe_set_size;

public:
  using value_type = T;
  using pointer = T *;
  using const_pointer = const T *;
  using reference = value_type &;
  using const_reference = const value_type &;
  using size_type = size_t;
  using difference_type = std::ptrdiff_t;
  using iterator = pointer;
  using const_iterator = const_pointer;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  // [containers.sequences.inplace_vector.cons], construct/copy/destroy
  constexpr inplace_vector() noexcept = default;

  // iterators
  constexpr iterator begin() noexcept { return storage_data(); }
  constexpr const_iterator begin() const noexcept { return storage_data(); }
  constexpr iterator end() noexcept { return begin() + size(); }
  constexpr const_iterator end() const noexcept { return begin() + size(); }
  constexpr reverse_iterator rbegin() noexcept {
    return reverse_iterator(end());
  }
  constexpr const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }
  constexpr reverse_iterator rend() noexcept {
    return reverse_iterator(begin());
  }
  constexpr const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  constexpr const_iterator cbegin() const noexcept { return storage_data(); }
  constexpr const_iterator cend() const noexcept { return cbegin() + size(); }
  constexpr const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  constexpr const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  [[nodiscard]] constexpr bool empty() const noexcept {
    return storage_size() == 0;
  };
  constexpr size_type size() const noexcept { return storage_size(); }
  static constexpr size_type max_size() noexcept { return N; }
  static constexpr size_type capacity() noexcept { return N; }
  constexpr void reserve(size_type n) {
    if (n > N) [[unlikely]]
      throw std::bad_alloc();
  }
  constexpr void shrink_to_fit() {}

  // element access
  constexpr reference operator[](size_type n) {
    return details::inplace_vector::index(*this, n);
  }
  constexpr const_reference operator[](size_type n) const {
    return details::inplace_vector::index(*this, n);
  }
  constexpr reference front() {
    return details::inplace_vector::index(*this, size_type(0));
  }
  constexpr const_reference front() const {
    return details::inplace_vector::index(*this, size_type(0));
  }
  constexpr reference back() {
    return details::inplace_vector::index(*this, size() - size_type(1));
  }
  constexpr const_reference back() const {
    return details::inplace_vector::index(*this, size() - size_type(1));
  }

  // [containers.sequences.inplace_vector.data], data access
  constexpr T *data() noexcept { return storage_data(); }
  constexpr const T *data() const noexcept { return storage_data(); }

  constexpr friend bool operator==(const inplace_vector &x,
                                   const inplace_vector &y) {
    return x.size() == y.size() && std::ranges::equal(x, y);
  }
  constexpr friend void swap(inplace_vector &x, inplace_vector &y) noexcept(
      N == 0 || (std::is_nothrow_swappable_v<T> &&
                 std::is_nothrow_move_constructible_v<T>)) {
    x.swap(y);
  }

private: // Utilities
  constexpr void
  assert_iterator_in_range([[maybe_unused]] const_iterator it) noexcept {
    IV_EXPECT(begin() <= it && "iterator not in range");
    IV_EXPECT(it <= end() && "iterator not in range");
  }
  constexpr void
  assert_valid_iterator_pair([[maybe_unused]] const_iterator first,
                             [[maybe_unused]] const_iterator last) noexcept {
    IV_EXPECT(first <= last && "invalid iterator pair");
  }
  constexpr void assert_iterator_pair_in_range(const_iterator first,
                                               const_iterator last) noexcept {
    assert_iterator_in_range(first);
    assert_iterator_in_range(last);
    assert_valid_iterator_pair(first, last);
  }
  constexpr void
  unsafe_destroy(T *first,
                 T *last) noexcept(std::is_nothrow_destructible_v<T>) {
    assert_iterator_pair_in_range(first, last);
    if constexpr (N > 0 && !std::is_trivial_v<T>) {
      for (; first != last; ++first)
        first->~T();
    }
  }

public:
  // Implementation

  // [containers.sequences.inplace_vector.modifiers], modifiers

  template <class... Args>
  constexpr T &unchecked_emplace_back(Args &&...args)
    requires(std::constructible_from<T, Args...>)
  {
    IV_EXPECT(size() < capacity() && "inplace_vector out-of-memory");
    std::construct_at(end(), std::forward<Args>(args)...);
    unsafe_set_size(size() + size_type(1));
    return back();
  }

  template <class... Args> constexpr T *try_emplace_back(Args &&...args) {
    if (size() == capacity()) [[unlikely]]
      return nullptr;
    return &unchecked_emplace_back(std::forward<Args>(args)...);
  }

  template <class... Args>
  constexpr T &emplace_back(Args &&...args)
    requires(std::constructible_from<T, Args...>)
  {
    if (!try_emplace_back(std::forward<Args>(args)...)) [[unlikely]]
      throw std::bad_alloc();
    return back();
  }
  constexpr T &push_back(const T &x)
    requires(std::constructible_from<T, const T &>)
  {
    emplace_back(x);
    return back();
  }
  constexpr T &push_back(T &&x)
    requires(std::constructible_from<T, T &&>)
  {
    emplace_back(std::forward<T &&>(x));
    return back();
  }

  constexpr T *try_push_back(const T &x)
    requires(std::constructible_from<T, const T &>)
  {
    return try_emplace_back(x);
  }
  constexpr T *try_push_back(T &&x)
    requires(std::constructible_from<T, T &&>)
  {
    return try_emplace_back(std::forward<T &&>(x));
  }

  constexpr T &unchecked_push_back(const T &x)
    requires(std::constructible_from<T, const T &>)
  {
    return unchecked_emplace_back(x);
  }
  constexpr T &unchecked_push_back(T &&x)
    requires(std::constructible_from<T, T &&>)
  {
    return unchecked_emplace_back(std::forward<T &&>(x));
  }

  template <details::inplace_vector::container_compatible_range<T> R>
  constexpr void append_range(R &&rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<R>>)
  {
    if constexpr (std::ranges::sized_range<R>) {
      if (size() + std::ranges::size(rg) > capacity()) [[unlikely]]
        throw std::bad_alloc();
    }
    for (auto &&e : rg) {
      if (size() == capacity()) [[unlikely]]
        throw std::bad_alloc();
      emplace_back(std::forward<decltype(e)>(e));
    }
  }

  template <details::inplace_vector::container_compatible_range<T> R>
  constexpr std::ranges::borrowed_iterator_t<R> try_append_range(R &&rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<R>>)
  {
    auto it = std::ranges::begin(rg);
    const auto end = std::ranges::end(rg);
    for (; size() != capacity() && it != end; ++it) {
      unchecked_emplace_back(*it);
    }
    return it;
  }

  template <class... Args>
  constexpr iterator emplace(const_iterator position, Args &&...args)
    requires(std::constructible_from<T, Args...> && std::movable<T>)
  {
    assert_iterator_in_range(position);
    auto b = end();
    emplace_back(std::forward<Args>(args)...);
    auto pos = begin() + (position - begin());
    std::rotate(pos, b, end());
    return pos;
  }

  template <class InputIterator>
  constexpr iterator insert(const_iterator position, InputIterator first,
                            InputIterator last)
    requires(std::constructible_from<T, std::iter_reference_t<InputIterator>> &&
             std::movable<T>)
  {
    assert_iterator_in_range(position);
    if constexpr (std::random_access_iterator<InputIterator>) {
      if (size() + static_cast<size_type>(std::distance(first, last)) >
          capacity()) [[unlikely]]
        throw std::bad_alloc{};
    }
    auto b = end();
    for (; first != last; ++first)
      emplace_back(std::move(*first));
    auto pos = begin() + (position - begin());
    std::rotate(pos, b, end());
    return pos;
  }

  template <details::inplace_vector::container_compatible_range<T> R>
  constexpr iterator insert_range(const_iterator position, R &&rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<R>> &&
             std::movable<T>)
  {
    return insert(position, std::begin(rg), std::end(rg));
  }

  constexpr iterator insert(const_iterator position,
                            std::initializer_list<T> il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    return insert_range(position, il);
  }

  constexpr iterator insert(const_iterator position, size_type n, const T &x)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    assert_iterator_in_range(position);
    auto b = end();
    for (size_type i = 0; i < n; ++i)
      emplace_back(x);
    auto pos = begin() + (position - begin());
    std::rotate(pos, b, end());
    return pos;
  }

  constexpr iterator insert(const_iterator position, const T &x)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    return insert(position, 1, x);
  }

  constexpr iterator insert(const_iterator position, T &&x)
    requires(std::constructible_from<T, T &&> && std::movable<T>)
  {
    return emplace(position, std::move(x));
  }

  constexpr inplace_vector(std::initializer_list<T> il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    insert(begin(), il);
  }

  constexpr inplace_vector(size_type n, const T &value)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    insert(begin(), n, value);
  }

  constexpr explicit inplace_vector(size_type n)
    requires(std::constructible_from<T, T &&> && std::default_initializable<T>)
  {
    for (size_type i = 0; i < n; ++i)
      emplace_back(T{});
  }

  template <class InputIterator> // BUGBUG: why not std::ranges::input_iterator?
  constexpr inplace_vector(InputIterator first, InputIterator last)
    requires(std::constructible_from<T, std::iter_reference_t<InputIterator>> &&
             std::movable<T>)
  {
    insert(begin(), first, last);
  }

  template <details::inplace_vector::container_compatible_range<T> R>
  constexpr inplace_vector(beman::from_range_t, R &&rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<R>> &&
             std::movable<T>)
  {
    insert_range(begin(), std::forward<R &&>(rg));
  }

  constexpr iterator erase(const_iterator first, const_iterator last)
    requires(std::movable<T>)
  {
    assert_iterator_pair_in_range(first, last);
    iterator f = begin() + (first - begin());
    if (first != last) {
      unsafe_destroy(std::move(f + (last - first), end(), f), end());
      unsafe_set_size(size() - static_cast<size_type>(last - first));
    }
    return f;
  }

  constexpr iterator erase(const_iterator position)
    requires(std::movable<T>)
  {
    return erase(position, position + 1);
  }

  constexpr void clear() noexcept {
    unsafe_destroy(begin(), end());
    unsafe_set_size(0);
  }

  constexpr void resize(size_type sz, const T &c)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    if (sz == size())
      return;
    else if (sz > N) [[unlikely]]
      throw std::bad_alloc{};
    else if (sz > size())
      insert(end(), sz - size(), c);
    else {
      unsafe_destroy(begin() + sz, end());
      unsafe_set_size(sz);
    }
  }
  constexpr void resize(size_type sz)
    requires(std::constructible_from<T, T &&> && std::default_initializable<T>)
  {
    if (sz == size())
      return;
    else if (sz > N) [[unlikely]]
      throw std::bad_alloc{};
    else if (sz > size())
      while (size() != sz)
        emplace_back(T{});
    else {
      unsafe_destroy(begin() + sz, end());
      unsafe_set_size(sz);
    }
  }

  constexpr reference at(size_type pos) {
    if (pos >= size()) [[unlikely]]
      throw std::out_of_range("inplace_vector::at");
    return details::inplace_vector::index(*this, pos);
  }
  constexpr const_reference at(size_type pos) const {
    if (pos >= size()) [[unlikely]]
      throw std::out_of_range("inplace_vector::at");
    return details::inplace_vector::index(*this, pos);
  }

  constexpr void pop_back() {
    IV_EXPECT(size() > 0 && "pop_back from empty inplace_vector!");
    unsafe_destroy(end() - 1, end());
    unsafe_set_size(size() - 1);
  }

  constexpr inplace_vector(const inplace_vector &x)
    requires(N == 0 || std::is_trivially_copy_constructible_v<T>)
  = default;

  constexpr inplace_vector(const inplace_vector &x)
    requires(N != 0 && !std::is_trivially_copy_constructible_v<T> &&
             std::copyable<T>)
  {
    for (auto &&e : x)
      emplace_back(e);
  }

  constexpr inplace_vector(inplace_vector &&x)
    requires(N == 0 || std::is_trivially_move_constructible_v<T>)
  = default;

  constexpr inplace_vector(inplace_vector &&x)
    requires(N != 0 && !std::is_trivially_move_constructible_v<T> &&
             std::movable<T>)
  {
    for (auto &&e : x)
      emplace_back(std::move(e));
  }

  constexpr inplace_vector &operator=(const inplace_vector &x)
    requires(N == 0 || (std::is_trivially_destructible_v<T> &&
                        std::is_trivially_copy_constructible_v<T> &&
                        std::is_trivially_copy_assignable_v<T>))
  = default;

  constexpr inplace_vector &operator=(const inplace_vector &x)
    requires(N != 0 &&
             !(std::is_trivially_destructible_v<T> &&
               std::is_trivially_copy_constructible_v<T> &&
               std::is_trivially_copy_assignable_v<T>) &&
             std::copyable<T>)
  {
    clear();
    for (auto &&e : x)
      emplace_back(e);
    return *this;
  }

  constexpr inplace_vector &operator=(inplace_vector &&x)
    requires(N == 0 || (std::is_trivially_destructible_v<T> &&
                        std::is_trivially_move_constructible_v<T> &&
                        std::is_trivially_move_assignable_v<T>))
  = default;

  constexpr inplace_vector &operator=(inplace_vector &&x)
    requires(N != 0 &&
             !(std::is_trivially_destructible_v<T> &&
               std::is_trivially_move_constructible_v<T> &&
               std::is_trivially_move_assignable_v<T>) &&
             std::movable<T>)
  {
    clear();
    for (auto &&e : x)
      emplace_back(std::move(e));
    return *this;
  }

  constexpr inplace_vector &operator=(std::initializer_list<T> il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    assign_range(il);
    return *this;
  }

  constexpr void
  swap(inplace_vector &x) noexcept(N == 0 ||
                                   (std::is_nothrow_swappable_v<T> &&
                                    std::is_nothrow_move_constructible_v<T>))
    requires(std::movable<T>)
  {
    auto tmp = std::move(x);
    x = std::move(*this);
    (*this) = std::move(tmp);
  }

  template <class InputIterator>
  constexpr void assign(InputIterator first, InputIterator last)
    requires(std::constructible_from<T, std::iter_reference_t<InputIterator>> &&
             std::movable<T>)
  {
    clear();
    insert(begin(), first, last);
  }
  template <details::inplace_vector::container_compatible_range<T> R>
  constexpr void assign_range(R &&rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<R>> &&
             std::movable<T>)
  {
    assign(std::begin(rg), std::end(rg));
  }
  constexpr void assign(size_type n, const T &u)
    requires(std::constructible_from<T, const T &> && std::movable<T>)
  {
    clear();
    insert(begin(), n, u);
  }
  constexpr void assign(std::initializer_list<T> il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    clear();
    insert_range(begin(), il);
  }

  constexpr friend auto operator<=>(const inplace_vector &x,
                                    const inplace_vector &y)
    requires(beman::details::inplace_vector::lessthan_comparable<T>)
  {
    if constexpr (std::three_way_comparable<T>) {
      return std::lexicographical_compare_three_way(x.begin(), x.end(),
                                                    y.begin(), y.end());
    } else {
      const auto sz = std::min(x.size(), y.size());
      for (std::size_t i = 0; i < sz; ++i) {
        if (x[i] < y[i])
          return std::strong_ordering::less;
        if (y[i] < x[i])
          return std::strong_ordering::greater;
        // [container.opt.reqmts] < must be total ordering relationship
      }

      return x.size() <=> y.size();
    }
  }
};

template <typename T, std::size_t N, typename U = T>
constexpr std::size_t erase(inplace_vector<T, N> &c, const U &value) {
  auto it = std::remove(c.begin(), c.end(), value);
  auto r = std::distance(it, c.end());
  c.erase(it, c.end());
  return r;
}

template <typename T, std::size_t N, typename Predicate>
constexpr std::size_t erase_if(inplace_vector<T, N> &c, Predicate pred) {
  auto it = std::remove_if(c.begin(), c.end(), pred);
  auto r = std::distance(it, c.end());
  c.erase(it, c.end());
  return r;
}

} // namespace beman

#undef IV_EXPECT

#endif // BEMAN_INPLACE_VECTOR_INPLACE_VECTOR_HPP
