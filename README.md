# beman.inplace_vector: Dynamically-resizable vector with fixed capacity

<!--
SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
-->

<!-- markdownlint-disable-next-line line-length -->
![Library Status](https://raw.githubusercontent.com/bemanproject/beman/refs/heads/main/images/badges/beman_badge-beman_library_under_development.svg) ![Continuous Integration Tests](https://github.com/bemanproject/inplace_vector/actions/workflows/ci_tests.yml/badge.svg) ![Lint Check (pre-commit)](https://github.com/bemanproject/inplace_vector/actions/workflows/pre-commit-check.yml/badge.svg) [![Coverage](https://coveralls.io/repos/github/bemanproject/inplace_vector/badge.svg?branch=main)](https://coveralls.io/github/bemanproject/inplace_vector?branch=main) ![Standard Target](https://github.com/bemanproject/beman/blob/main/images/badges/cpp29.svg)

**Implements**: [`inplace_vector` (P0843R14)](https://wg21.link/P0843R14).

**Status**: [Under development and not yet ready for production use.](https://github.com/bemanproject/beman/blob/main/docs/beman_library_maturity_model.md#under-development-and-not-yet-ready-for-production-use)

## License

`beman.inplace_vector` is licensed under the Apache License v2.0 with LLVM Exceptions.

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

Full runnable examples can be found in [`examples/`](examples/).

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

```c++
beman::inplace_vector::inplace_vector<int, 1> iv;
iv.resize(0); // OK
iv.resize(10); // will throw or abort

beman::inplace_vector::freestanding::inplace_vector<int, 1> fs_iv;
fs_iv.resize(0); // will generate a compile-time error
fs_iv.resize(10); // will generate a compile-time error
```

## Dependencies

### Build Environment

This project requires at least the following to build:

* A C++ compiler that conforms to the C++20 standard or greater
* CMake 3.30 or later
* (Test Only) GoogleTest

You can disable building tests by setting CMake option `BEMAN_INPLACE_VECTOR_BUILD_TESTS` to
`OFF` when configuring the project.

### Supported Platforms

| Compiler   | Version | C++ Standards | Standard Library  |
|------------|---------|---------------|-------------------|
| GCC        | 15-13   | C++26-C++20   | libstdc++         |
| Clang      | 22-19   | C++26-C++20   | libstdc++, libc++ |
| Clang      | 18      | C++26-C++20   | libc++            |
| Clang      | 18      | C++23, C++20  | libstdc++         |
| Clang      | 17      | C++26-C++20   | libc++            |
| Clang      | 17      | C++20         | libstdc++         |
| AppleClang | latest  | C++26-C++20   | libc++            |

## Development

See the [Contributing Guidelines](CONTRIBUTING.md).

## Integrate beman.inplace\_vector into your project

### Build

You can build inplace_vector using a CMake workflow preset:

```bash
cmake --workflow --preset gcc-release
```

To list available workflow presets, you can invoke:

```bash
cmake --list-presets=workflow
```

For details on building beman.inplace_vector without using a CMake preset, refer to the
[Contributing Guidelines](CONTRIBUTING.md).

### Installation

#### Vcpkg

The preferred way to install inplace_vector is via vcpkg. To do so, after installing vcpkg
itself, you need to add support for the Beman project's [vcpkg
registry](https://github.com/bemanproject/vcpkg-registry) by configuring a
`vcpkg-configuration.json` file (which inplace_vector [provides](vcpkg-configuration.json)).

Then, simply run `vcpkg install beman-inplace-vector`.

#### Manual

To install beman.inplace_vector globally after building with the `gcc-release` preset, you can
run:

```bash
sudo cmake --install build/gcc-release
```

Alternatively, to install to a prefix, for example `/opt/beman`, you can run:

```bash
sudo cmake --install build/gcc-release --prefix /opt/beman
```

This will generate the following directory structure:

```txt
/opt/beman
├── include
│   └── beman
│       └── inplace_vector
│           ├── inplace_vector.hpp
│           └── ...
└── lib
    └── cmake
        └── beman.inplace_vector
            ├── beman.inplace_vector-config-version.cmake
            ├── beman.inplace_vector-config.cmake
            └── beman.inplace_vector-targets.cmake
```

### CMake Configuration

If you installed beman.inplace_vector to a prefix, you can specify that prefix to your CMake
project using `CMAKE_PREFIX_PATH`; for example, `-DCMAKE_PREFIX_PATH=/opt/beman`.

You need to bring in the `beman.inplace_vector` package to define the `beman::inplace_vector` CMake
target:

```cmake
find_package(beman.inplace_vector REQUIRED)
```

You will then need to add `beman::inplace_vector` to the link libraries of any libraries or
executables that include `beman.inplace_vector` headers.

```cmake
target_link_libraries(yourlib PUBLIC beman::inplace_vector)
```

### Using beman.inplace\_vector

To use `beman.inplace_vector` in your C++ project,
include an appropriate `beman.inplace_vector` header from your source code.

```c++
#include <beman/inplace_vector/inplace_vector.hpp>
```

> [!NOTE]
>
> `beman.inplace_vector` headers are to be included with the `beman/inplace_vector/` prefix.
> Altering include search paths to spell the include target another way (e.g.
> `#include <inplace_vector.hpp>`) is unsupported.
