#include <array>
#include <beman/inplace_vector/inplace_vector.hpp>
#include <type_traits>

/**
 * These tests are only meant to test that suitable constexpr functions compiles
 * in a constant evaluation environment.
 *
 * The validity of the underlying implementation is tested in the main test
 * case as there's no diverging logic between constexpr code and runtime code.
 */

struct Some {
  constexpr Some() = default;
  constexpr Some(int v) : val(v) {}

  int val;

  constexpr bool operator==(const Some &other) const {
    return val == other.val;
  }
  constexpr auto operator<=>(const Some &other) const {
    return val <=> other.val;
  }
};
static_assert(std::is_trivial_v<Some>);

#define TEST(NAME)                                                             \
  static_assert(std::invoke([]() {                                             \
                  NAME<beman::inplace_vector<int, 20>>();                      \
                  NAME<beman::inplace_vector<Some, 20>>();                     \
                  return true;                                                 \
                }),                                                            \
                "##NAME");

template <typename IV> constexpr void test_constructors() {
  using T = IV::value_type;

  std::array<T, 5> arr;
  arr.fill(T(20));

  {
    IV v;
  }
  {
    IV v(10);
  }
  {
    IV v(5, T(10));
  }
  {
    IV v(arr.begin(), arr.end());
  }
  {
    IV v(beman::from_range_t{}, arr);
  }
  {
    IV other{0, 1};
    IV copy(other);
  }
  {
    IV other{0, 1};
    IV copy(std::move(other));
  }
  {
    IV v({0, 1});
  }
}
TEST(test_constructors);

template <typename IV> constexpr void test_op_eq() {
  using T = IV::value_type;

  {
    IV v, other{0, 1};
    v = other;
  }
  {
    IV v, other{0, 1};
    v = std::move(other);
  }
  {
    IV v;
    v = {0, 1};
  }
}
TEST(test_op_eq);

template <typename IV> constexpr void test_assignment() {
  using T = IV::value_type;

  std::array<T, 5> arr;
  arr.fill(T(20));

  {
    IV v;
    v.assign(arr.begin(), arr.end());
  }
  {
    IV v;
    v.assign_range(arr);
  }
  {
    IV v;
    v.assign(5, T(20));
  }
  {
    IV v;
    v.assign({0, 1, 2});
  }
}
TEST(test_assignment);

template <typename IV> constexpr void test_iterator_access() {
  using T = IV::value_type;

  {
    IV v{0, 1};
    (void)v.begin();
    (void)v.end();
    (void)v.rbegin();
    (void)v.rend();
    (void)v.cbegin();
    (void)v.cend();
    (void)v.crbegin();
    (void)v.crend();
  }
}
TEST(test_iterator_access);

template <typename IV> constexpr void test_size_capacity() {
  using T = IV::value_type;

  {
    IV v{0, 1};
    (void)v.empty();
    (void)v.size();
    (void)v.max_size();
    (void)v.capacity();
  }

  {
    IV v{0, 1};
    v.resize(3);
    v.resize(2, T(20));
  }

  {
    IV v{0, 1};
    v.reserve(5);
    v.shrink_to_fit();
  }
}
TEST(test_size_capacity);

template <typename IV> constexpr void test_element_access() {
  {
    IV v{0, 1};
    (void)v[0];
    (void)v.at(0);
    (void)v.front();
    (void)v.back();
  }

  {
    const IV v{0, 1};
    (void)v[0];
    (void)v.at(0);
    (void)v.front();
    (void)v.back();
  }
}
TEST(test_element_access);

template <typename IV> constexpr void test_data_access() {
  {
    IV v{0, 1};
    v.data();
  }
  {
    const IV v{0, 1};
    v.data();
  }
}
TEST(test_data_access);

template <typename IV> constexpr void test_modifiers() {
  using T = IV::value_type;

  std::array<T, 5> arr;
  arr.fill(T(20));

  {
    IV v;
    v.emplace_back(20);
    v.push_back(arr[0]);
    v.push_back(T(20));
    v.append_range(arr);
    v.pop_back();
  }

  {
    IV v;
    v.try_emplace_back(20);
    v.try_push_back(arr[0]);
    v.try_push_back(T(20));
    // v.try_append_range(arr);
  }

  {
    IV v;
    v.unchecked_emplace_back(20);
    v.unchecked_push_back(arr[0]);
    v.unchecked_push_back(T(20));
  }

  {
    IV v{0, 1};
    v.emplace(v.begin(), 20);
    v.insert(v.begin(), arr[0]);
    v.insert(v.begin(), T(20));
    v.insert(v.begin(), 2, arr[0]);
    v.insert(v.begin(), arr.begin(), arr.end());
    v.insert_range(v.begin(), arr);
    v.insert(v.begin(), {1, 2});
    v.erase(v.begin());
    v.erase(v.begin(), v.begin() + 2);
  }

  {
    IV v, other{0, 1};
    v.swap(other);
  }

  {
    IV v{0, 1};
    v.clear();
  }
}
TEST(test_modifiers);

template <typename IV> constexpr void test_op_comp() {
  IV v{0, 1, 2}, other{3, 4};

  (void)(v == v);
  (void)(v == other);
  (void)(v <=> v);
  (void)(v <=> other);
}
TEST(test_op_comp);

template <typename IV> constexpr void test_erase() {
  IV v{0, 1, 2, 3, 3, 5};
  (void)beman::erase(v, 3);
  (void)beman::erase_if(v, [](auto v) { return v < 3; });
}
TEST(test_erase)

struct Complex {
  int val = 0;

  constexpr bool operator==(const Complex &other) const {
    return val == other.val;
  }
  constexpr auto operator<=>(const Complex &other) const {
    return val <=> other.val;
  }
};
static_assert(!std::is_trivially_default_constructible_v<Complex>);

#define TEST_EMPTY(NAME)                                                       \
  static_assert(std::invoke([]() {                                             \
                  NAME<Complex>();                                             \
                  return true;                                                 \
                }),                                                            \
                "##NAME");

template <typename T> constexpr void speical_test_empty() {
  static_assert(!std::is_trivially_default_constructible_v<T>);
  using IV = beman::inplace_vector<T, 0>;

  std::array<T, 10> arr;
  arr.fill(T{50});

  {
    IV v;
  }
  {
    IV v(0, T{50});
  }
  {
    IV a, b;
    a = b;
    a = IV();
  }
  {
    IV v;
    v.assign(0, T{50});
  }
  {
    IV v;
    (void)v.begin();
    (void)v.end();
    (void)v.rbegin();
    (void)v.rend();
    (void)v.cbegin();
    (void)v.cend();
    (void)v.crbegin();
    (void)v.crend();
  }
  {
    IV v;
    (void)v.empty();
    (void)v.size();
    (void)v.max_size();
    (void)v.capacity();
    v.resize(0);
    v.resize(0, T{40});
    v.reserve(0);
    v.shrink_to_fit();
  }
  {
    IV v;
    v.try_emplace_back(50);
    v.try_push_back(T(50));
    v.try_push_back(arr[0]);
    // v.try_append_range(arr);
    v.clear();
  }
  {
    IV a, b;
    a.swap(b);
  }
  {
    IV a, b;
    (void)(a == b);
    (void)(a <=> b);
  }
}
TEST_EMPTY(speical_test_empty);

int main() {
  // compile means passing
}
