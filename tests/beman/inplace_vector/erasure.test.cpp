#include <gtest/gtest.h>

#include "gtest_setup.hpp"

namespace {
// 23.3.14.6 Erasure [inplace.vector.erasure]

template <typename Param> class Erasure : public IVBasicTest<Param> {};
TYPED_TEST_SUITE(Erasure, IVAllTypes);

TYPED_TEST(Erasure, ByValue) {
  // template<class T, size_t N, class U = T>
  //   constexpr size_t erase(inplace_vector<T, N>& c, const U& value);
  //
  // Effects: Equivalent to:
  // auto it = remove(c.begin(), c.end(), value);
  // auto r = distance(it, c.end());
  // c.erase(it, c.end());
  // return r;

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  IV device;
  if constexpr (device.capacity() == 0)
    return;

  T duplicates{4612};
  auto uniques = this->unique(device.capacity() / 2);

  for (auto i = 0ul; i < uniques.size(); ++i) {
    device.push_back(uniques[i]);
    if (device.size() != device.capacity())
      device.push_back(duplicates);
  }

  // TODO: uncomment this after erase is implemented
  // beman::erase(device, duplicates);
  // EXPECT_EQ(uniques, device);

  GTEST_SKIP() << "Not implemented";
}

TYPED_TEST(Erasure, ByPred) {
  // template<class T, size_t N, class Predicate>
  // constexpr size_t erase_if(inplace_vector<T, N>& c, Predicate pred);
  //
  // Effects: Equivalent to:
  // auto it = remove_if(c.begin(), c.end(), pred);
  // auto r = distance(it, c.end());
  // c.erase(it, c.end());
  // return r;

  using T = TestFixture::T;
  using IV = TestFixture::IV;

  IV device;
  if constexpr (device.capacity() == 0)
    return;

  for (auto i = 0; i < static_cast<int>(device.capacity()); ++i)
    device.push_back(T{i});

  // TODO: complete this when its implemented
  // beman::erase_if(device,
  //                 [&](auto &v) { return v.value > (device.capacity() / 2);
  //                 });

  GTEST_SKIP() << "Not implemented";
}
}; // namespace
