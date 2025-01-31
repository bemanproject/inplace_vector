#include "beman/inplace_vector/inplace_vector.hpp"

#include <array>
#include <benchmark/benchmark.h>
#include <cstddef>
#include <iostream>
#include <limits>
#include <memory>
#include <optional>
#include <random>
#include <vector>

static constexpr std::size_t IDX_SIZE = 4096;

template <typename T> class VectorTest : public benchmark::Fixture {
  void fill_vec(benchmark::State &state) {
    std::random_device dev;
    std::mt19937 rng(dev());
    std::uniform_int_distribution<std::mt19937::result_type> dist(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max());

    vec.reserve(state.range(0));
    for (auto i = 0; i < state.range(0); ++i) {
      vec.push_back(dist(rng));
    }
  }

  void fill_idx(benchmark::State &state) {
    std::random_device dev;
    std::mt19937 rng(dev());
    std::uniform_int_distribution<std::mt19937::result_type> dist(
        0, state.range(0));

    idxs = std::make_unique<int[]>(IDX_SIZE);
    for (auto i = 0; i < IDX_SIZE; ++i) {
      auto idx = dist(rng);
      idxs.get()[i] = idx;
    }
  }

public:
  T vec;

  std::unique_ptr<int[]> idxs;

  void SetUp(::benchmark::State &state) {
    fill_vec(state);
    fill_idx(state);

    // Warm up the cache for idxs
    auto sum = 0;
    for (auto i = 0; i < 128; ++i) {
      sum += idxs[i];
    }
  }

  void TearDown(::benchmark::State &state) { vec = T(); }
};

inline void random_access(benchmark::State &state, int *idxs, auto &vec) {
  while (state.KeepRunningBatch(IDX_SIZE)) {
    // while (state.KeepRunning()) {
    for (auto i = 0; i < IDX_SIZE; ++i) {
      auto idx = idxs[i];
      benchmark::DoNotOptimize(vec[idx]);
      vec[idx] = i;
      benchmark::ClobberMemory();
    }
  }
}

#define BENCHMARK_INPLACE_VEC_RANDOM(SIZE)                                     \
  BENCHMARK_TEMPLATE_DEFINE_F(VectorTest, inplace_vector_random_##SIZE,        \
                              beman::inplace_vector<int, SIZE>)                \
  (benchmark::State & state) { random_access(state, idxs.get(), vec); }        \
  BENCHMARK_REGISTER_F(VectorTest, inplace_vector_random_##SIZE)               \
      ->Arg(SIZE)                                                              \
      ->Name("random inplace_vector");

BENCHMARK_TEMPLATE_DEFINE_F(VectorTest, vector_random,
                            std::vector<int>)(benchmark::State &state) {
  random_access(state, idxs.get(), vec);
}

#define BENCHMARK_VEC_RANDOM(SIZE)                                             \
  BENCHMARK_REGISTER_F(VectorTest, vector_random)                              \
      ->Arg(SIZE)                                                              \
      ->Name("random vector");

#define BENCHMARK_BOTH_RANDOM(SIZE)                                            \
  BENCHMARK_INPLACE_VEC_RANDOM(SIZE);                                          \
  BENCHMARK_VEC_RANDOM(SIZE);

BENCHMARK_BOTH_RANDOM(3);
BENCHMARK_BOTH_RANDOM(7);
BENCHMARK_BOTH_RANDOM(15);
BENCHMARK_BOTH_RANDOM(31);
BENCHMARK_BOTH_RANDOM(63);
BENCHMARK_BOTH_RANDOM(127);
BENCHMARK_BOTH_RANDOM(255);
BENCHMARK_BOTH_RANDOM(511);
BENCHMARK_BOTH_RANDOM(1023);

BENCHMARK_BOTH_RANDOM(1024);
BENCHMARK_BOTH_RANDOM(2048);
BENCHMARK_BOTH_RANDOM(4096);
BENCHMARK_BOTH_RANDOM(16384);
BENCHMARK_BOTH_RANDOM(32768);
BENCHMARK_BOTH_RANDOM(65536);
BENCHMARK_BOTH_RANDOM(131072);
BENCHMARK_BOTH_RANDOM(262144);

BENCHMARK_BOTH_RANDOM(524288);
BENCHMARK_BOTH_RANDOM(1048576);

BENCHMARK_MAIN();
