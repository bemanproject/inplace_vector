#include <array>
#include <beman/inplace_vector/inplace_vector.hpp>

#include <iostream>

using namespace beman::inplace_vector;

/*
 * Helper function to print the contents of an inplace_vector.
 */
template <typename T, std::size_t Capacity>
void print_vector(const inplace_vector<T, Capacity> &vec,
                  const std::string_view name) {
  std::cout << name << " [size=" << vec.size()
            << ", capacity=" << vec.capacity() << "]: ";

  for (const auto &value : vec) {
    std::cout << value << ' ';
  }

  std::cout << '\n';
}

/*
 * Helper function to print the contents of an array.
 */
template <typename T, std::size_t Capacity>
void print_array(const std::array<T, Capacity> &array,
                 const std::string_view name) {
  std::cout << name << " [size=" << array.size() << "]: ";

  for (const auto &value : array) {
    std::cout << value << ' ';
  }

  std::cout << '\n';
}

int main() {
  /*
   * std::array
   *
   * Fixed-size container:
   * - Size is always known at compile time.
   * - Elements always exist.
   * - No push_back / emplace_back support.
   */
  std::cout << "----- std::array -----\n";

  std::array<int, 4> arr{1, 2};

  // arr.push_back(3); // ERROR: no push_back in std::array

  arr[2] = 3;
  arr[3] = 4;

  print_array(arr, "std::array");

  /*
   * inplace_vector
   *
   * Fixed-capacity container:
   * - Capacity is fixed at compile time.
   * - Size changes dynamically at runtime.
   * - Supports push_back / emplace_back APIs.
   */
  std::cout << "\n----- inplace_vector -----\n";

  inplace_vector<int, 4> vec;

  vec.unchecked_push_back(1);
  vec.unchecked_push_back(2);

  if (vec.try_push_back(3)) {
    std::cout << "Inserted 3\n";
  }

  if (vec.try_push_back(4)) {
    std::cout << "Inserted 4\n";
  }

  if (!vec.try_push_back(5)) {
    std::cout << "Failed to insert 5: vector is full\n";
  }

  print_vector(vec, "inplace_vector");

  return 0;
}
