<!--
SPDX-License-Identifier: <SPDX License Expression>
-->

# beman.inplace\_vector: Dynamically-resizable vector with fixed capacity

![Library Status](https://raw.githubusercontent.com/bemanproject/beman/refs/heads/main/images/badges/beman_badge-beman_library_under_development.svg)
![Continuous Integration Tests](https://github.com/bemanproject/inplace_vector/actions/workflows/ci_tests.yml/badge.svg)
![Code Format](https://github.com/bemanproject/inplace_vector/actions/workflows/pre-commit-check.yml/badge.svg)

**Implements**: [`inplace_vector` (P0843R14)](https://wg21.link/P0843R14)

**Status**:
[Under development and not yet ready for production use.](https://github.com/bemanproject/beman/blob/main/docs/beman_library_maturity_model.md#under-development-and-not-yet-ready-for-production-use)

## Usage

### Definition in P0843

> `inplace_vector` is a dynamically-resizable array with capacity fixed
at compile time and contiguous inplace storage,
that is, the array elements are stored within the vector object itself.
Its API closely resembles `std::vector<T, A>`,
making it easy to teach and learn,
and the inplace storage guarantee makes it useful in environments in
which dynamic memory allocations are undesired.

#### Note on implementation progress

Current implementation implements all public interfaces defined in the paper.

There have been updates to the spec after the paper getting accepted.
Notably,
[trivial unions (P3074)](https://github.com/cplusplus/draft/pull/7680/files#diff-3b5851f7056b7c68cb3ba2ab51cf0b75a780c6a11e5e181770067700454ace7b)
have made the constexpr unconditional.
constexpr has not yet been fully implemented due to limited compiler support - see
[Alternative implementation using P3074 when compiler support is available](https://github.com/bemanproject/inplace_vector/issues/36).

You can follow [this link](https://eel.is/c++draft/inplace.vector)
to checkout the status of `inplace_vector` in the latest draft.

Contributions are welcome.

### Code example

```cpp
#include <array>
#include <cassert>

#include <beman/inplace_vector/inplace_vector.hpp>

using namespace beman::inplace_vector;

/**
 * Generates fibonacci sequence using inplace_vector.
 * See: https://en.wikipedia.org/wiki/Fibonacci_sequence
 */
template <int Capacity>
constexpr inplace_vector<int, Capacity> fibonacci_to(int num) {
  assert(num < Capacity);

  inplace_vector<int, Capacity> vec;

  constexpr std::array<int, 2> first_two{0, 1};
  for (auto i = 0; i <= num; ++i) {
    auto new_val = i < 2 ? first_two[i] : vec[i - 1] + vec[i - 2];
    vec.push_back(new_val);
  }

  return vec;
}

/*
 * Check the result of the computation at compile time.
 */
constexpr bool check_5() {
  auto got = fibonacci_to<10>(5);
  constexpr inplace_vector<int, 10> correct{0, 1, 1, 2, 3, 5};
  return got == correct;
}

static_assert(check_5());

```

### Note on constexpr support

Since `constexpr` requirements are actively changing,
you can use `beman::has_constexpr_support` to detect if our implementation
provide constexpr support for a specific specialization of `inplace_vector`.

Note this is not part of the standard Library and should not be relied on once
`constexpr` requirement stabilize.

Example Usage:
`static_assert(beman::has_constexpr_support<beman::inplace_vector::inplace_vector<int, 5>>)`.

### Freestanding

`beman::inplace_vector::freestanding::inplace_vector` implements a minimal freestanding version of the specification,
which marks all potentially throwing functions as `= deleted`.
This is useful for platforms without exception support, as it will generate a compile-time error
instead of a potential runtime error when trying to use a throwing function.

``` C++
beman::inplace_vector::inplace_vector<int, 1> iv;
iv.resize(0); // OK
iv.resize(10); // will throw or abort

beman::inplace_vector::freestanding::inplace_vector<int, 1> fs_iv;
fs_iv.resize(0); // will generate a compile-time error
fs_iv.resize(10); // will generate a compile-time error
```

## How to Build

### Compiler support

Building this repository requires **C++20** or later.

We will evaluate the possibility of partial support for C++17
when constexpr is fully supported and tested.

### Dependencies

Current implementation is tested against both GNU gcc and LLVM clang compilers.
More specifically, gcc version 12 to 14, and clang version 17 to 20.
Versions outside of this range will likely work as well,
they are just not tested in our current infrastructure.
We are working on expanding this range of compiler support,
and aim to bring `inplace_vector` to MSVC soon!

### Instructions

#### Using CMake Preset

[CMake Preset](https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html)
is a new CMake functionality that provides one-line configure + test.

You can use `gcc-debug` to setup a debug orienated `inplace_vector` development environment.

```text
$ cmake --workflow --preset gcc-debug
Executing workflow step 1 of 3: configure preset "gcc-debug"

Preset CMake variables:

  BEMAN_BUILDSYS_SANITIZER="MaxSan"
  CMAKE_BUILD_TYPE="Debug"
  CMAKE_CXX_STANDARD="20"
  CMAKE_TOOLCHAIN_FILE="cmake/gnu-toolchain.cmake"

-- The CXX compiler identification is GNU 14.2.0
....
-- Generating done (0.0s)
-- Build files have been written to: /inplace_vector/build/gcc-debug

Executing workflow step 2 of 3: build preset "gcc-debug"

[8/20] Building CXX object tests/beman/inplace_vector/CMakeFiles/beman.inplace_vector.tests.erasure.dir/erasure.test.cpp.o

Executing workflow step 3 of 3: test preset "gcc-debug"

Test project /home/bradwu/Desktop/inplace_vector/build/gcc-debug
      Start  1: beman.inplace_vector.test
 1/54 Test  #1: beman.inplace_vector.test ....................................   Passed    0.03 sec
      Start  2: beman.inplace_vector.ref-test
 2/54 Test  #2: beman.inplace_vector.ref-test ................................   Passed    0.03 sec
      Start  3: ContainerRequirements/*.ValueType
 3/54 Test  #3: ContainerRequirements/*.ValueType ............................   Passed    0.15 sec
      Start  4: ContainerRequirements/*.Reference
...
50/54 Test #50: SizeNCapacity/*.ResizeDown ...................................   Passed    0.05 sec
      Start 51: SizeNCapacity/*.ResizeUp
51/54 Test #51: SizeNCapacity/*.ResizeUp .....................................   Passed    0.05 sec
      Start 52: Data/*.Test
52/54 Test #52: Data/*.Test ..................................................   Passed    0.05 sec
      Start 53: Erasure/*.ByValue
53/54 Test #53: Erasure/*.ByValue ............................................***Skipped   0.04 sec
      Start 54: Erasure/*.ByPred
54/54 Test #54: Erasure/*.ByPred .............................................***Skipped   0.04 sec

100% tests passed, 0 tests failed out of 54

Total Test time (real) =   6.20 sec
```

Note that this workflow compiles the project with sanitizers,
if you wish to playaround with `inplace_vector` without sanitizers,
use `gcc-release`.

#### Manual CMake Build

```text
# Configure build
$ cmake -S . -B build -DCMAKE_CXX_STANDARD=20
-- The CXX compiler identification is GNU 11.4.0
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Configuring done (0.4s)
-- Generating done (0.0s)
-- Build files have been written to: /.../inplace_vector/build

# Build
$ cmake --build build
[ 50%] Building CXX object src/beman/inplace_vector/tests/CMakeFiles/beman.inplace_vector.test.dir/inplace_vector.test.cpp.o
[100%] Linking CXX executable beman.inplace_vector.test
[100%] Built target beman.inplace_vector.test

# Run tests
$ ctest --test-dir build/
Internal ctest changing into directory: /.../inplace_vector/build
Test project /.../inplace_vector/build
    Start 1: beman.inplace_vector.test
1/1 Test #1: beman.inplace_vector.test ........   Passed    0.00 sec

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.01 sec
```

## Development

### Linting

This project use [pre-commit](https://pre-commit.com/) framework for linting.

#### Install pre-commit

```bash
pip3 install pre-commit
```

pre-commit can be configured to automatically triggered before git commit,
to install this functionality, run:

```bash
pre-commit install
```

#### Running pre-commit

```bash
pre-commit run --all-files
```

This will download and check linting rules on all files.
Apart from Markdown files,
`pre-commit` will automatically format the files
to conform with linting rules in place.
