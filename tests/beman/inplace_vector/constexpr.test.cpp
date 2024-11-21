#include <algorithm>
#include <beman/inplace_vector/inplace_vector.hpp>
#include <functional>
#include <type_traits>

using namespace beman::inplace_vector;

#define S_ASSERT(EXP)                                                          \
  do {                                                                         \
    if (!(EXP)) {                                                              \
      return false;                                                            \
    }                                                                          \
  } while (0)

struct NonTrivial {
  int z = 0;
};
static_assert(!std::is_trivial_v<NonTrivial>);

template <typename T> constexpr bool test_empty_vec(T &vec) {

  // sizes
  S_ASSERT(vec.max_size() == 0);
  S_ASSERT(vec.capacity() == 0);
  S_ASSERT(vec.size() == 0);
  S_ASSERT(vec.empty());

  // itr
  S_ASSERT(vec.begin() == vec.end());
  S_ASSERT(vec.cbegin() == vec.cend());
  S_ASSERT(vec.rbegin() == vec.rend());
  S_ASSERT(vec.crbegin() == vec.crend());

  // push_back
  S_ASSERT(vec.try_push_back({}) == nullptr);
  S_ASSERT(vec.try_emplace_back() == nullptr);

  return true;
}

static_assert(std::invoke([]() {
                inplace_vector<int, 0> vec;
                test_empty_vec(vec);
                return true;
              }),
              "0 capacity Trivial type");

static_assert(std::invoke([]() {
                inplace_vector<const int, 0> vec;
                test_empty_vec(vec);
                return true;
              }),
              "0 capacity Trivial const type");

static_assert(std::invoke([]() {
                inplace_vector<NonTrivial, 0> vec;
                test_empty_vec(vec);
                return true;
              }),
              "0 capacity Non-trivial type");

static_assert(std::invoke([]() {
                inplace_vector<const NonTrivial, 0> vec;
                test_empty_vec(vec);
                return true;
              }),
              "0 capacity Non-trivial const type");

static_assert(std::invoke([]() {
                // sizes
                {
                  inplace_vector<int, 1> vec;
                  vec.push_back(1);

                  S_ASSERT(vec.max_size() == 1);
                  S_ASSERT(vec.capacity() == 1);
                  S_ASSERT(vec.size() == 1);
                  S_ASSERT(!vec.empty());
                }

                // Access
                {

                  inplace_vector<int, 1> vec;
                  vec.push_back(1);

                  S_ASSERT(vec[0] == 1);
                  S_ASSERT(vec.front() == 1);
                  S_ASSERT(vec.back() == 1);
                }

                // forward itr
                {
                  inplace_vector<int, 1> vec;
                  vec.push_back(1);

                  auto itr = vec.begin();
                  S_ASSERT(*itr == 1);
                  itr += 1;
                  S_ASSERT(itr == vec.end());
                }

                // backward itr
                {
                  inplace_vector<int, 1> vec;
                  vec.push_back(1);

                  auto itr = vec.rbegin();
                  S_ASSERT(*itr == 1);
                  itr += 1;
                  S_ASSERT(itr == vec.rend());
                }

                // try variant
                {
                  inplace_vector<int, 1> vec;
                  S_ASSERT(*vec.try_push_back(1) == 1);
                  S_ASSERT(vec.try_push_back(2) == nullptr);
                }

                return true;
              }),
              "Single push_back");

static_assert(std::invoke([]() {
                // push pop back
                {
                  inplace_vector<int, 5> vec;
                  vec.push_back(1);
                  vec.pop_back();

                  S_ASSERT(vec.empty());
                }

                // resize
                {
                  inplace_vector<int, 5> vec;
                  vec.push_back(1);
                  vec.push_back(2);

                  S_ASSERT(vec.size() == 2);

                  vec.resize(1);
                  S_ASSERT(vec.size() == 1);
                  S_ASSERT(vec.back() == 1);
                }

                return true;
              }),
              "Basic mutation");

// [container.reqmts] General container requirements
using X = inplace_vector<int, 5>;

constexpr bool reqmts_default() {
  {
    X u;
    S_ASSERT(u.empty());
  }
  {
    X u = X();
    S_ASSERT(u.empty());
  }

  return true;
}

static_assert(reqmts_default());

constexpr bool reqmts_copy() {
  constexpr X exp{1, 2, 3};

  X a{1, 2, 3};

  {
    X u(a);
    S_ASSERT(std::ranges::equal(exp, u));
    S_ASSERT(std::ranges::equal(exp, a));
  }
  {
    X u = a;
    S_ASSERT(std::ranges::equal(exp, u));
    S_ASSERT(std::ranges::equal(exp, a));
  }

  return true;
}

static_assert(reqmts_copy());

constexpr bool reqmts_move() {
  /*
   * TODO: Need to keep in check with revision
   *
   * Move semantics
   * A moved-from inplace_vector is left in a valid but unspecified state
   * (option 3 below) unless T is trivially-copyable, in which case the size of
   * the inplace_vector does not change (array semantics, option 2 below). That
   * is:
   *
   * inplace_vector a(10);
   * inplace_vector b(std::move(a));
   * assert(a.size() == 10); // MAY FAIL
   *
   * moves a's elements element-wise into b, and afterwards the size of the
   * moved-from inplace_vector may have changed.
   *
   * This prevents code from relying on the size staying the same (and therefore
   * being incompatible with changing an inplace_vector type back to vector)
   * without incuring the cost of having to clear the inplace_vector.
   *
   * When T is trivially-copyable, array semantics are used to provide trivial
   * move operations.
   */

  {
    constexpr X exp{1, 2, 3};
    X mov_from(exp);
    X u(std::move(mov_from));
    S_ASSERT(std::ranges::equal(exp, u));

    static_assert(std::is_trivially_copyable_v<X::value_type>);
    S_ASSERT(mov_from.size() == exp.size());
  }
  {
    // Note(river): for later non-trivial type implementation, verify:
    // Effects: All existing elements of a are either move assigned to or
    // destroyed.
    constexpr X origin{1, 2, 3};
    constexpr X exp{1, 2};

    X a(origin);
    X mov_from(exp);
    a = std::move(mov_from);

    S_ASSERT(std::ranges::equal(exp, a));
    static_assert(std::is_trivially_copyable_v<X::value_type>);
    S_ASSERT(mov_from.size() == exp.size());
  }

  return true;
}

static_assert(reqmts_move());

// destructor implicilty tested

constexpr bool reqmts_itr() {
  constexpr X exp{1, 2, 3};

  {
    X b = exp;
    auto beg = b.begin();
    auto end = b.end();
    end--;

    S_ASSERT(*b.begin() == 1);
    S_ASSERT(*end == 3);
  }

  {
    X b = exp;
    auto beg = b.cbegin();
    auto end = b.cend();
    end--;

    S_ASSERT(*b.begin() == 1);
    S_ASSERT(*end == 3);
  }

  return true;
}

static_assert(reqmts_itr());

int main() {
  // compile means pass
}
