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
   * inplace_vector behaves similarly to std::vector,
   * but stores elements inline with a fixed maximum capacity.
   */
  std::cout << "----- inplace_vector -----\n";

  inplace_vector<Point, 3> points;

  points.push_back({1, 2});
  points.emplace_back(3, 4);

  /*
   * insert() shifts existing elements to make room.
   */
  points.insert(points.begin() + 1, Point{10, 20});

  print_vector(points, "points");

  /*
   * Once size() == capacity(), checked modifying operations
   * such as push_back(), emplace_back(), and insert()
   * cannot grow the container further.
   *
   * In exception-enabled environments:
   *   - std::bad_alloc is thrown.
   *
   * In non-exception environments:
   *   - the implementation asserts.
   */
  std::cout << "\n----- Behavior at Capacity -----\n";
  try {
    points.push_back({5, 6});
  } catch (const std::bad_alloc &) {
    std::cout << "push_back failed: vector is at capacity\n";
  }

  try {
    points.emplace_back(7, 8);
  } catch (const std::bad_alloc &) {
    std::cout << "emplace_back failed: vector is at capacity\n";
  }

  try {
    points.insert(points.begin(), Point{0, 0});
  } catch (const std::bad_alloc &) {
    std::cout << "insert failed: vector is at capacity\n";
  }

  return 0;
}
