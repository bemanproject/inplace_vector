include(FetchContent)

# Fetch Google benchmark
FetchContent_Declare(
    benchmark
    GIT_REPOSITORY https://github.com/google/benchmark.git
    GIT_TAG
        c58e6d0710581e3a08d65c349664128a8d9a2461 # v1.9.1
    EXCLUDE_FROM_ALL
)

set(BENCHMARK_ENABLE_GTEST_TESTS OFF)
FetchContent_MakeAvailable(benchmark)

add_executable(beman.inplace_vector.benchmark.push_back pushback_benchmark.cpp)
target_link_libraries(
    beman.inplace_vector.benchmark.push_back
    PRIVATE beman.inplace_vector benchmark::benchmark
)
