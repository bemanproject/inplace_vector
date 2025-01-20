#include "beman/inplace_vector/inplace_vector.hpp"
#include <benchmark/benchmark.h>
#include <cstddef>
#include <vector>

using beman::inplace_vector;

template <std::size_t Capacity>
void InplaceVectorPushback(benchmark::State &state) {
  for (auto _ : state) {
    using IV = inplace_vector<int, Capacity>;

    IV v;
    for (typename IV::size_type i = 0; i < Capacity; ++i)
      v.push_back(i);
    benchmark::DoNotOptimize(v);
  }
}

template <std::size_t Capacity> void VetorPushback(benchmark::State &state) {
  for (auto _ : state) {
    using IV = std::vector<int>;

    IV v;
    v.reserve(Capacity);
    for (typename IV::size_type i = 0; i < Capacity; ++i)
      v.push_back(i);
    benchmark::DoNotOptimize(v);
  }
}

BENCHMARK_TEMPLATE(InplaceVectorPushback, 128);
BENCHMARK_TEMPLATE(InplaceVectorPushback, 256);
BENCHMARK_TEMPLATE(InplaceVectorPushback, 512);
BENCHMARK_TEMPLATE(InplaceVectorPushback, 1024);

BENCHMARK_TEMPLATE(VetorPushback, 128);
BENCHMARK_TEMPLATE(VetorPushback, 256);
BENCHMARK_TEMPLATE(VetorPushback, 512);
BENCHMARK_TEMPLATE(VetorPushback, 1024);

BENCHMARK_MAIN();
