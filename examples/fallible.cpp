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
 * Example struct to demonstrate inplace_vector with non-trivial types.
 */
struct Point {
  int x;
  int y;
};

std::ostream &operator<<(std::ostream &os, const Point &p) {
  return os << '(' << p.x << ", " << p.y << ')';
}

int main() {
  /*
   * Checked API
   *
   * try_* operations fail gracefully instead of overflowing capacity.
   */
  std::cout << "----- Checked API -----\n";

  inplace_vector<int, 2> checked;

  if (checked.try_push_back(10)) {
    std::cout << "Inserted 10\n";
  }

  if (checked.try_push_back(20)) {
    std::cout << "Inserted 20\n";
  }

  if (!checked.try_push_back(30)) {
    std::cout << "Failed to insert 30: vector is full\n";
  }

  print_vector(checked, "checked");

  /*
   * Unchecked API
   *
   * unchecked_* operations skip capacity checks and require that
   * the caller guarantees enough remaining space.
   */
  std::cout << "\n----- Unchecked API -----\n";

  inplace_vector<Point, 2> unchecked;

  unchecked.unchecked_emplace_back(1, 2);
  unchecked.unchecked_emplace_back(3, 4);

  print_vector(unchecked, "unchecked");

  /*
   * unchecked.unchecked_emplace_back(5, 6);
   * Undefined behavior: capacity exceeded.
   */

  return 0;
}
