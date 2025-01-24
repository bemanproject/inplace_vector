# Inplace Vector Testing Suite

This folder contains tests for `inplace_vector` implementation.

The aim for the test cases are to keep the implementation in-check with its standing in the latest C++ draft.

You can checkout `inplace_vector`'s current state in C++ draft [here](https://eel.is/c++draft/inplace.vector).

## C++ Draft paragraph to test file

`inplace_vector`'s chapter number in the latest C++ draft is **23.3.14**.

### Overview (23.3.14.1)

[Overview](https://eel.is/c++draft/inplace.vector#overview) has 6 clauses.

#### 6.1 Overview

> An inplace_vector is a contiguous container. Its capacity is fixed and its
elements are stored within the inplace_vector object itself.

This is not testable.

#### 6.2 Container Requirements

> An inplace_vector meets all of the requirements of a container ([container.reqmts]),
> of a reversible container ([container.rev.reqmts]), of a contiguous container,
> and of a sequence container, including most of the optional sequence
container requirements ([sequence.reqmts]). The exceptions are the push_front,
> prepend_range, pop_front, and emplace_front member functions,
> which are not provided.
> Descriptions are provided here only for operations on inplace_vector that
> are not described in one of these tables or for operations where there is
> additional semantic information.

See [container_requirements.test.cpp](container_requirements.test.cpp).

#### 6.3 Constexpr Iterator

> For any N, `inplace_vector<T, N>​::​iterator` and
> `inplace_vector<T, N>​::​const_iterator` meet the constexpr iterator
> requirements.

Not tested for now.

#### 6.4 Constexpr member functions

> For any $N>0$, if T is not trivially copyable or
> `is_trivially_default_constructible_v<T>` is false,
> then no `inplace_vector<T, N>` member functions are usable in
> constant expressions.

Not tested for now.

#### 6.5 Bad alloc requirement

> Any member function of `inplace_vector<T, N>` that would cause the size to
> exceed N throws an exception of type bad_alloc.

These are tested with individual functions.

#### 6.6 Triviality

Let IV denote a specialization of `inplace_vector<T, N>`.
> If N is zero, then IV is trivially copyable and empty,
> and std​::​is_trivially_default_constructible_v<IV> is true.
> (Sub-clauses omitted)

See [triviality.test.cpp](triviality.test.cpp)

### Constructors (23.3.14.2)

See [constructors.test.cpp](constructors.test.cpp)

### Size and capacity (23.3.14.3)

See [size_n_data.test.cpp](size_n_data.test.cpp)

### Data (23.3.14.4)

See [size_n_data.test.cpp](size_n_data.test.cpp)

### Modifiers (23.3.14.5)

See [modifiers.test.cpp](modifiers.test.cpp)

### Erasure (23.3.14.6)

See [erasure.test.cpp](erasure.test.cpp)

## Other tests

- [ref_impl.test.cpp](ref_impl.test.cpp):
  Is the test suite imported from reference implementation in
  [P0843R14](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2024/p0843r14.html).
  Originally included [here on godbolt](https://godbolt.org/z/5P78aG5xE).
- [inplace_vector.test.cpp](inplace_vector.test.cpp):
  A minimal test suite by @Hels15 for their implementation.

## Known Issues/ Missed Tests

- Constexpr related functionalities.
- Emplacement minimal copy/ construction.
- Exception safety on mutation.

## Special Thanks

Special thanks to Jan Babst (@jbab) for his contribution at setting up the
Google Test infrastructure.
