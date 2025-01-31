#include "beman/inplace_vector/inplace_vector.hpp"

#include <benchmark/benchmark.h>
#include <cstddef>
#include <iostream>
#include <vector>

using beman::inplace_vector;

template <std::size_t Capacity> void InplaceVector(benchmark::State &state) {
  using IV = inplace_vector<int, Capacity>;
  IV v;
  v.reserve(Capacity);

  while (state.KeepRunningBatch(Capacity)) {
    // while (state.KeepRunning()) {
    v.clear();
    for (typename IV::size_type i = 0; i < Capacity; ++i)
      v.push_back(i);
    benchmark::DoNotOptimize(v);
  }
}

template <std::size_t Capacity> void StdVector(benchmark::State &state) {
  using IV = std::vector<int>;
  IV v;
  v.reserve(Capacity);

  while (state.KeepRunningBatch(Capacity)) {
    // while (state.KeepRunning()) {
    v.clear();
    for (typename IV::size_type i = 0; i < Capacity; ++i)
      v.push_back(i);
    benchmark::DoNotOptimize(v);
  }
}

template <typename V, bool use_reserve> struct Meta {
  using Vector = V;
  constexpr static bool Reserve = use_reserve;
};

#define BENCHMARK_VEC_L1(ARG)                                                  \
  BENCHMARK_TEMPLATE(ARG, 3);                                                  \
  BENCHMARK_TEMPLATE(ARG, 7);                                                  \
  BENCHMARK_TEMPLATE(ARG, 15);                                                 \
  BENCHMARK_TEMPLATE(ARG, 31);                                                 \
  BENCHMARK_TEMPLATE(ARG, 63);                                                 \
  BENCHMARK_TEMPLATE(ARG, 127);                                                \
  BENCHMARK_TEMPLATE(ARG, 255);                                                \
  BENCHMARK_TEMPLATE(ARG, 511);                                                \
  BENCHMARK_TEMPLATE(ARG, 1023);

#define BENCHMARK_VEC_L2_L3(ARG)                                               \
  BENCHMARK_TEMPLATE(ARG, 1 << 10);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 11);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 12);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 14);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 15);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 16);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 17);                                            \
  BENCHMARK_TEMPLATE(ARG, 1 << 18);

BENCHMARK_VEC_L1(InplaceVector);
BENCHMARK_VEC_L1(StdVector);

BENCHMARK_VEC_L2_L3(InplaceVector);
BENCHMARK_VEC_L2_L3(StdVector);

BENCHMARK_MAIN();
