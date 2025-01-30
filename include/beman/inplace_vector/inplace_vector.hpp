#pragma once
#pragma GCC diagnostic ignored "-Wunused-parameter"

/*
 * SPDX-FileCopyrightText: Copyright (c) 2023 Gonzalo Brito Gadeschi. All rights
reserved.
 * SPDX-License-Identifier: MIT
 * SPDX-License-Identifier: Apache 2.0 with LLVM exception
 * That is, this project is Dual Licensed.
 *
 * MIT LICENSE:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF Precondition, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * APACHE 2.0 with LLVM Exception:
 ==============================================================================
The LLVM Project is under the Apache License v2.0 with LLVM Exceptions:
==============================================================================

                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

    TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

    1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

    2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

    3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

    4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

    5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

    6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

    7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

    8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

    9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

    END OF TERMS AND CONDITIONS

    APPENDIX: How to apply the Apache License to your work.

      To apply the Apache License to your work, attach the following
      boilerplate notice, with the fields enclosed by brackets "[]"
      replaced with your own identifying information. (Don't include
      the brackets!)  The text should be enclosed in the appropriate
      comment syntax for the file format. We also recommend that a
      file or class name and description of purpose be included on the
      same "printed page" as the copyright notice for easier
      identification within third-party archives.

    Copyright [yyyy] [name of copyright owner]

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


---- LLVM Exceptions to the Apache 2.0 License ----

As an exception, if, as a result of your compiling your source code, portions
of this Software are embedded into an Object form of such source code, you
may redistribute such embedded portions in such Object form without complying
with the conditions of Sections 4(a), 4(b) and 4(d) of the License.

In addition, if you combine or link compiled forms of this Software with
software that is licensed under the GPLv2 ("Combined Software") and if a
court of competent jurisdiction determines that the patent provision (Section
3), the indemnity provision (Section 9) or other Section of the License
conflicts with the conditions of the GPLv2, you may retroactively and
prospectively choose to deem waived or otherwise exclude such Section(s) of
the License, but only in their entirety and only with respect to the Combined
Software.
 */
#include <algorithm> // for rotate...
#include <array>
#include <concepts>   // for lots...
#include <cstddef>    // for size_t
#include <cstdint>    // for fixed-width integer types
#include <cstdio>     // for assertion diagnostics
#include <cstdlib>    // for abort
#include <functional> // for less and equal_to
#include <iterator>   // for reverse_iterator and iterator traits
#include <limits>     // for numeric_limits
#include <memory>     // for destroy
#include <new>        // for operator new
#include <ranges>
#include <stdexcept>   // for length_error
#include <type_traits> // for aligned_storage and all meta-functions

// Optimizer allowed to assume that EXPR evaluates to true
#define __IV_ASSUME(__EXPR)                                                    \
  static_cast<void>((__EXPR) ? void(0) : __builtin_unreachable)

// Assert pretty printer
#define __IV_ASSERT(...)                                                       \
  static_cast<void>((__VA_ARGS__)                                              \
                        ? void(0)                                              \
                        : ::beman::details::inplace_vector::__assert_failure(  \
                              static_cast<const char *>(__FILE__), __LINE__,   \
                              "assertion failed: " #__VA_ARGS__))

// Assert in debug, assume in release.
// #ifdef NDEBUG
// #define __IV_EXPECT(__EXPR) __IV_ASSUME(__EXPR)
// #else
// #define __IV_EXPECT(__EXPR) __IV_ASSERT(__EXPR)
// #endif

// TODO River: Disabled temporarily
#define __IV_EXPECT(__EXPR)

// beman::from_range_t
namespace beman {
struct from_range_t {};
inline constexpr from_range_t from_range;
}; // namespace beman

// Private utilities
namespace beman::details::inplace_vector {

template <class = void>
[[noreturn]]
static constexpr void __assert_failure(char const *__file, int __line,
                                       char const *__msg) {
  if (std::is_constant_evaluated()) {
    throw __msg; // TODO: std lib implementer, do better here
  } else {
    std::fprintf(stderr, "%s(%d): %s\n", __file, __line, __msg);
    std::abort();
  }
}

using namespace beman::details::inplace_vector;

// clang-format off
// Smallest unsigned integer that can represent values in [0, N].
template <size_t N>
using __smallest_size_t
= std::conditional_t<(N < std::numeric_limits<uint8_t>::max()),  uint8_t,
    std::conditional_t<(N < std::numeric_limits<uint16_t>::max()), uint16_t,
    std::conditional_t<(N < std::numeric_limits<uint32_t>::max()), uint32_t,
    std::conditional_t<(N < std::numeric_limits<uint64_t>::max()), uint64_t,
                   size_t>>>>;
// clang-format on

// Index a random-access and sized range doing bound checks in debug builds
template <std::ranges::random_access_range __Rng, std::integral __Index>
static constexpr decltype(auto) __index(__Rng &&__rng, __Index __i) noexcept
  requires(std::ranges::sized_range<__Rng>)
{
  __IV_EXPECT(static_cast<ptrdiff_t>(__i) < std::ranges::size(__rng));
  return std::begin(std::forward<__Rng>(__rng))[std::forward<__Index>(__i)];
}

// http://eel.is/c++draft/container.requirements.general#container.intro.reqmts-2
template <class __Rng, class T>
concept __container_compatible_range =
    std::ranges::input_range<__Rng> &&
    std::convertible_to<std::ranges::range_reference_t<__Rng>, T>;

} // namespace beman::details::inplace_vector

// Types implementing the `inplace_vector`'s storage
namespace beman::details::inplace_vector::storage {

// Storage for zero elements.
template <class T> struct zero_sized {
protected:
  using size_type = uint8_t;
  static constexpr T *__data() noexcept { return nullptr; }
  static constexpr size_type __size() noexcept { return 0; }
  static constexpr void __unsafe_set_size(size_t new_size) noexcept {
    __IV_EXPECT(new_size == 0 &&
                "tried to change size of empty storage to non-zero value");
  }

public:
  constexpr zero_sized() = default;
  constexpr zero_sized(zero_sized const &) = default;
  constexpr zero_sized &operator=(zero_sized const &) = default;
  constexpr zero_sized(zero_sized &&) = default;
  constexpr zero_sized &operator=(zero_sized &&) = default;
  constexpr ~zero_sized() = default;
};

// Storage for trivial types.
template <class T, size_t N> struct trivial {
  static_assert(std::is_trivial_v<T>,
                "storage::trivial<T, C> requires Trivial<T>");
  static_assert(N != size_t{0}, "N  == 0, use zero_sized");

protected:
  using size_type = __smallest_size_t<N>;

private:
  // If value_type is const, then const std::array of non-const elements:
  using array_based_storage =
      std::conditional_t<!std::is_const_v<T>, std::array<T, N>,
                         const std::array<std::remove_const_t<T>, N>>;
  alignas(alignof(T)) array_based_storage storage_data_{};
  size_type storage_size_ = 0;

protected:
  constexpr const T *__data() const noexcept { return storage_data_.data(); }
  constexpr T *__data() noexcept { return storage_data_.data(); }
  constexpr size_type __size() const noexcept { return storage_size_; }
  constexpr void __unsafe_set_size(size_t new_size) noexcept {
    __IV_EXPECT(size_type(new_size) <= N && "new_size out-of-bounds [0, N]");
    storage_size_ = size_type(new_size);
  }

public:
  constexpr trivial() noexcept = default;
  constexpr trivial(trivial const &) noexcept = default;
  constexpr trivial &operator=(trivial const &) noexcept = default;
  constexpr trivial(trivial &&) noexcept = default;
  constexpr trivial &operator=(trivial &&) noexcept = default;
  constexpr ~trivial() = default;
};

template <class T, size_t N> struct raw_byte_based_storage {
  alignas(T) std::byte _d[sizeof(T) * N];
  constexpr T *__data(size_t __i) noexcept {
    __IV_EXPECT(__i < N);
    return reinterpret_cast<T *>(_d) + __i;
  }
  constexpr const T *__data(size_t __i) const noexcept {
    __IV_EXPECT(__i < N);
    return reinterpret_cast<const T *>(_d) + __i;
  }
};

/// Storage for non-trivial elements.
template <class T, size_t N> struct non_trivial {
  static_assert(!std::is_trivial_v<T>,
                "use storage::trivial for Trivial<T> elements");
  static_assert(N != size_t{0}, "use storage::zero for N==0");

protected:
  using size_type = __smallest_size_t<N>;

private:
  using byte_based_storage = std::conditional_t<
      !std::is_const_v<T>, raw_byte_based_storage<T, N>,
      const raw_byte_based_storage<std::remove_const_t<T>, N>>;
  byte_based_storage storage_data_{}; // BUGBUG: test SIMD types
  size_type storage_size_ = 0;

protected:
  constexpr const T *__data() const noexcept { return storage_data_.__data(0); }
  constexpr T *__data() noexcept { return storage_data_.__data(0); }
  constexpr size_type __size() const noexcept { return storage_size_; }
  constexpr void __unsafe_set_size(size_t new_size) noexcept {
    __IV_EXPECT(size_type(new_size) <= N && "new_size out-of-bounds [0, N)");
    storage_size_ = size_type(new_size);
  }

public:
  constexpr non_trivial() noexcept = default;
  constexpr non_trivial(non_trivial const &) noexcept = default;
  constexpr non_trivial &operator=(non_trivial const &) noexcept = default;
  constexpr non_trivial(non_trivial &&) noexcept = default;
  constexpr non_trivial &operator=(non_trivial &&) noexcept = default;

  constexpr ~non_trivial()
    requires(std::is_trivially_destructible_v<T>)
  = default;
  constexpr ~non_trivial() { std::destroy(__data(), __data() + __size()); }
};

// Selects the vector storage.
template <class T, size_t N>
using _t = std::conditional_t<
    N == 0, zero_sized<T>,
    std::conditional_t<std::is_trivial_v<T>, trivial<T, N>, non_trivial<T, N>>>;

} // namespace beman::details::inplace_vector::storage

namespace beman {

/// Dynamically-resizable fixed-N vector with inplace storage.
template <class T, size_t N>
struct inplace_vector : private details::inplace_vector::storage::_t<T, N> {
private:
  static_assert(std::is_nothrow_destructible_v<T>,
                "T must be nothrow destructible");
  using __base_t = details::inplace_vector::storage::_t<T, N>;
  using __self = inplace_vector<T, N>;
  using __base_t::__data;
  using __base_t::__size;
  using __base_t::__unsafe_set_size;

public:
  using value_type = T;
  using pointer = T *;
  using const_pointer = const T *;
  using reference = value_type &;
  using const_reference = const value_type &;
  using size_type = size_t;
  using difference_type = std::ptrdiff_t;
  using iterator = pointer;
  using const_iterator = const_pointer;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  // [containers.sequences.inplace_vector.cons], construct/copy/destroy
  constexpr inplace_vector() noexcept = default;
  // constexpr explicit inplace_vector(size_type __n);
  // constexpr inplace_vector(size_type __n, const T& __value);
  // template <class __InputIterator>  // BUGBUG: why not model input_iterator?
  //   constexpr inplace_vector(__InputIterator __first, __InputIterator
  //   __last);
  // template <details::inplace_vector::__container_compatible_range<T> __R>
  //  constexpr inplace_vector(from_range_t, __R&& __rg);
  // from base-class, trivial if is_trivially_copy_constructible_v<T>:
  //   constexpr inplace_vector(const inplace_vector&);
  // from base-class, trivial if is_trivially_move_constructible_v<T>
  //   constexpr inplace_vector(inplace_vector&&) noexcept(N == 0 ||
  //   std::is_nothrow_move_constructible_v<T>);
  // constexpr inplace_vector(std::initializer_list<T> __il);
  // from base-class, trivial if is_trivially_destructible_v<T>
  //   constexpr ~inplace_vector();
  // from base-class, trivial if is_trivially_destructible_v<T> &&
  // is_trivially_copy_assignable_v<T>
  //   constexpr inplace_vector& operator=(const inplace_vector& __other);
  // from base-class, trivial if is_trivially_destructible_v<T> &&
  // is_trivially_copy_assignable_v<T>
  //   constexpr inplace_vector& operator=(inplace_vector&& __other)
  //   noexcept(N == 0 || is_nothrow_move_assignable_v<T>);
  // template <class __InputIterator> // BUGBUG: why not model input_iterator
  //  constexpr void assign(__InputIterator __first, __InputIterator l__ast);
  // template<details::inplace_vector::__container_compatible_range<T> __R>
  //  constexpr void assign_range(__R&& __rg);
  // constexpr void assign(size_type __n, const T& __u);
  // constexpr void assign(std::initializer_list<T> __il);

  // iterators
  constexpr iterator begin() noexcept { return __data(); }
  constexpr const_iterator begin() const noexcept { return __data(); }
  constexpr iterator end() noexcept { return begin() + size(); }
  constexpr const_iterator end() const noexcept { return begin() + size(); }
  constexpr reverse_iterator rbegin() noexcept {
    return reverse_iterator(end());
  }
  constexpr const_reverse_iterator rbegin() const noexcept {
    return const_reverse_iterator(end());
  }
  constexpr reverse_iterator rend() noexcept {
    return reverse_iterator(begin());
  }
  constexpr const_reverse_iterator rend() const noexcept {
    return const_reverse_iterator(begin());
  }

  constexpr const_iterator cbegin() const noexcept { return __data(); }
  constexpr const_iterator cend() const noexcept { return cbegin() + size(); }
  constexpr const_reverse_iterator crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  constexpr const_reverse_iterator crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }

  [[nodiscard]] constexpr bool empty() const noexcept { return __size() == 0; };
  constexpr size_type size() const noexcept { return __size(); }
  static constexpr size_type max_size() noexcept { return N; }
  static constexpr size_type capacity() noexcept { return N; }
  // constexpr void resize(size_type __sz);
  // constexpr void resize(size_type __sz, const T& __c);
  constexpr void reserve(size_type __n) {
    if (__n > N) [[unlikely]]
      throw std::bad_alloc();
  }
  constexpr void shrink_to_fit() {}

  // element access
  constexpr reference operator[](size_type __n) {
    return details::inplace_vector::__index(*this, __n);
  }
  constexpr const_reference operator[](size_type __n) const {
    return details::inplace_vector::__index(*this, __n);
  }
  // constexpr const_reference at(size_type __n) const;
  // constexpr reference       at(size_type __n);
  constexpr reference front() {
    return details::inplace_vector::__index(*this, size_type(0));
  }
  constexpr const_reference front() const {
    return details::inplace_vector::__index(*this, size_type(0));
  }
  constexpr reference back() {
    return details::inplace_vector::__index(*this, size() - size_type(1));
  }
  constexpr const_reference back() const {
    return details::inplace_vector::__index(*this, size() - size_type(1));
  }

  // [containers.sequences.inplace_vector.data], data access
  constexpr T *data() noexcept { return __data(); }
  constexpr const T *data() const noexcept { return __data(); }

  // [containers.sequences.inplace_vector.modifiers], modifiers
  // template <class... __Args>
  //  constexpr T& emplace_back(__Args&&... __args);
  // constexpr T& push_back(const T& __x);
  // constexpr T& push_back(T&& __x);
  // template<details::inplace_vector::__container_compatible_range<T> __R>
  //  constexpr void append_range(__R&& __rg);
  // constexpr void pop_back();

  // template<class... __Args>
  //  constexpr T* try_emplace_back(__Args&&... __args);
  // constexpr T* try_push_back(const T& __value);
  // constexpr T* try_push_back(T&& __value);

  // template<class... __Args>
  //  constexpr T& unchecked_emplace_back(__Args&&... __args);
  // constexpr T& unchecked_push_back(const T& __value);
  // constexpr T& unchecked_push_back(T&& __value);

  // template <class... __Args>
  //  constexpr iterator emplace(const_iterator __position, __Args&&... __args);
  // constexpr iterator insert(const_iterator __position, const T& __x);
  // constexpr iterator insert(const_iterator __position, T&& __x);
  // constexpr iterator insert(const_iterator __position, size_type __n, const
  // T& __x);
  // template <class __InputIterator>
  //  constexpr iterator insert(const_iterator __position, __InputIterator
  //  __first, __InputIterator __last);
  // template<details::inplace_vector::__container_compatible_range<T> __R>
  //   constexpr iterator insert_range(const_iterator __position, __R&& __rg);
  // constexpr iterator insert(const_iterator __position,
  // std::initializer_list<T>
  // __il); constexpr iterator erase(const_iterator __position); constexpr
  // iterator erase(const_iterator __first, const_iterator __last); constexpr
  // void swap(inplace_vector& __x)
  //  noexcept(N == 0 || (std::is_nothrow_swappable_v<T> &&
  //  std::is_nothrow_move_constructible_v<T>));
  // constexpr void clear() noexcept;

  constexpr friend bool operator==(const inplace_vector &__x,
                                   const inplace_vector &__y) {
    return __x.size() == __y.size() && std::ranges::equal(__x, __y);
  }
  // constexpr friend auto /*synth-three-way-result<T>*/
  //  operator<=>(const inplace_vector& __x, const inplace_vector& __y);
  constexpr friend void swap(inplace_vector &__x, inplace_vector &__y) noexcept(
      N == 0 || (std::is_nothrow_swappable_v<T> &&
                 std::is_nothrow_move_constructible_v<T>)) {
    __x.swap(__y);
  }

private: // Utilities
  constexpr void __assert_iterator_in_range(const_iterator __it) noexcept {
    __IV_EXPECT(begin() <= __it && "iterator not in range");
    __IV_EXPECT(__it <= end() && "iterator not in range");
  }
  constexpr void __assert_valid_iterator_pair(const_iterator __first,
                                              const_iterator __last) noexcept {
    __IV_EXPECT(__first <= __last && "invalid iterator pair");
  }
  constexpr void
  __assert_iterator_pair_in_range(const_iterator __first,
                                  const_iterator __last) noexcept {
    __assert_iterator_in_range(__first);
    __assert_iterator_in_range(__last);
    __assert_valid_iterator_pair(__first, __last);
  }
  constexpr void
  __unsafe_destroy(T *__first,
                   T *__last) noexcept(std::is_nothrow_destructible_v<T>) {
    __assert_iterator_pair_in_range(__first, __last);
    if constexpr (N > 0 && !std::is_trivial_v<T>) {
      for (; __first != __last; ++__first)
        __first->~T();
    }
  }

public:
  // Implementation

  // [containers.sequences.inplace_vector.modifiers], modifiers

  template <class... __Args>
  constexpr T &unchecked_emplace_back(__Args &&...__args)
    requires(std::constructible_from<T, __Args...>)
  {
    __IV_EXPECT(size() < capacity() && "inplace_vector out-of-memory");
    std::construct_at(end(), std::forward<__Args>(__args)...);
    __unsafe_set_size(size() + size_type(1));
    return back();
  }

  template <class... __Args> constexpr T *try_emplace_back(__Args &&...__args) {
    if (size() == capacity()) [[unlikely]]
      return nullptr;
    return &unchecked_emplace_back(std::forward<__Args>(__args)...);
  }

  template <class... __Args>
  constexpr T &emplace_back(__Args &&...__args)
    requires(std::constructible_from<T, __Args...>)
  {
    if (!try_emplace_back(std::forward<__Args>(__args)...)) [[unlikely]]
      throw std::bad_alloc();
    return back();
  }
  constexpr T &push_back(const T &__x)
    requires(std::constructible_from<T, const T &>)
  {
    emplace_back(__x);
    return back();
  }
  constexpr T &push_back(T &&__x)
    requires(std::constructible_from<T, T &&>)
  {
    emplace_back(std::forward<T &&>(__x));
    return back();
  }

  constexpr T *try_push_back(const T &__x)
    requires(std::constructible_from<T, const T &>)
  {
    return try_emplace_back(__x);
  }
  constexpr T *try_push_back(T &&__x)
    requires(std::constructible_from<T, T &&>)
  {
    return try_emplace_back(std::forward<T &&>(__x));
  }

  constexpr T &unchecked_push_back(const T &__x)
    requires(std::constructible_from<T, const T &>)
  {
    return unchecked_emplace_back(__x);
  }
  constexpr T &unchecked_push_back(T &&__x)
    requires(std::constructible_from<T, T &&>)
  {
    return unchecked_emplace_back(std::forward<T &&>(__x));
  }

  template <details::inplace_vector::__container_compatible_range<T> __R>
  constexpr void append_range(__R &&__rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<__R>>)
  {
    if constexpr (std::ranges::sized_range<__R>) {
      if (size() + std::ranges::size(__rg) > capacity()) [[unlikely]]
        throw std::bad_alloc();
    }
    for (auto &&__e : __rg) {
      if (size() == capacity()) [[unlikely]]
        throw std::bad_alloc();
      emplace_back(std::forward<decltype(__e)>(__e));
    }
  }

  template <class... __Args>
  constexpr iterator emplace(const_iterator __position, __Args &&...__args)
    requires(std::constructible_from<T, __Args...> && std::movable<T>)
  {
    __assert_iterator_in_range(__position);
    auto __b = end();
    emplace_back(std::forward<__Args>(__args)...);
    auto __pos = begin() + (__position - begin());
    std::rotate(__pos, __b, end());
    return __pos;
  }

  template <class __InputIterator>
  constexpr iterator insert(const_iterator __position, __InputIterator __first,
                            __InputIterator __last)
    requires(
        std::constructible_from<T, std::iter_reference_t<__InputIterator>> &&
        std::movable<T>)
  {
    __assert_iterator_in_range(__position);
    if constexpr (std::random_access_iterator<__InputIterator>) {
      if (size() + static_cast<size_type>(std::distance(__first, __last)) >
          capacity()) [[unlikely]]
        throw std::bad_alloc{};
    }
    auto __b = end();
    for (; __first != __last; ++__first)
      emplace_back(std::move(*__first));
    auto __pos = begin() + (__position - begin());
    std::rotate(__pos, __b, end());
    return __pos;
  }

  template <details::inplace_vector::__container_compatible_range<T> __R>
  constexpr iterator insert_range(const_iterator __position, __R &&__rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<__R>> &&
             std::movable<T>)
  {
    return insert(__position, std::begin(__rg), std::end(__rg));
  }

  constexpr iterator insert(const_iterator __position,
                            std::initializer_list<T> __il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    return insert_range(__position, __il);
  }

  constexpr iterator insert(const_iterator __position, size_type __n,
                            const T &__x)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    __assert_iterator_in_range(__position);
    auto __b = end();
    for (size_type __i = 0; __i < __n; ++__i)
      emplace_back(__x);
    auto __pos = begin() + (__position - begin());
    std::rotate(__pos, __b, end());
    return __pos;
  }

  constexpr iterator insert(const_iterator __position, const T &__x)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    return insert(__position, 1, __x);
  }

  constexpr iterator insert(const_iterator __position, T &&__x)
    requires(std::constructible_from<T, T &&> && std::movable<T>)
  {
    return emplace(__position, std::move(__x));
  }

  constexpr inplace_vector(std::initializer_list<T> __il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    insert(begin(), __il);
  }

  constexpr inplace_vector(size_type __n, const T &__value)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    insert(begin(), __n, __value);
  }

  constexpr explicit inplace_vector(size_type __n)
    requires(std::constructible_from<T, T &&> && std::default_initializable<T>)
  {
    for (size_type __i = 0; __i < __n; ++__i)
      emplace_back(T{});
  }

  template <
      class __InputIterator> // BUGBUG: why not std::ranges::input_iterator?
  constexpr inplace_vector(__InputIterator __first, __InputIterator __last)
    requires(
        std::constructible_from<T, std::iter_reference_t<__InputIterator>> &&
        std::movable<T>)
  {
    insert(begin(), __first, __last);
  }

  template <details::inplace_vector::__container_compatible_range<T> __R>
  constexpr inplace_vector(beman::from_range_t, __R &&__rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<__R>> &&
             std::movable<T>)
  {
    insert_range(begin(), std::forward<__R &&>(__rg));
  }

  constexpr iterator erase(const_iterator __first, const_iterator __last)
    requires(std::movable<T>)
  {
    __assert_iterator_pair_in_range(__first, __last);
    iterator __f = begin() + (__first - begin());
    if (__first != __last) {
      __unsafe_destroy(std::move(__f + (__last - __first), end(), __f), end());
      __unsafe_set_size(size() - static_cast<size_type>(__last - __first));
    }
    return __f;
  }

  constexpr iterator erase(const_iterator __position)
    requires(std::movable<T>)
  {
    return erase(__position, __position + 1);
  }

  constexpr void clear() noexcept {
    __unsafe_destroy(begin(), end());
    __unsafe_set_size(0);
  }

  constexpr void resize(size_type __sz, const T &__c)
    requires(std::constructible_from<T, const T &> && std::copyable<T>)
  {
    if (__sz == size())
      return;
    else if (__sz > N) [[unlikely]]
      throw std::bad_alloc{};
    else if (__sz > size())
      insert(end(), __sz - size(), __c);
    else {
      __unsafe_destroy(begin() + __sz, end());
      __unsafe_set_size(__sz);
    }
  }
  constexpr void resize(size_type __sz)
    requires(std::constructible_from<T, T &&> && std::default_initializable<T>)
  {
    if (__sz == size())
      return;
    else if (__sz > N) [[unlikely]]
      throw std::bad_alloc{};
    else if (__sz > size())
      while (size() != __sz)
        emplace_back(T{});
    else {
      __unsafe_destroy(begin() + __sz, end());
      __unsafe_set_size(__sz);
    }
  }

  constexpr reference at(size_type __pos) {
    if (__pos >= size()) [[unlikely]]
      throw std::out_of_range("inplace_vector::at");
    return details::inplace_vector::__index(*this, __pos);
  }
  constexpr const_reference at(size_type __pos) const {
    if (__pos >= size()) [[unlikely]]
      throw std::out_of_range("inplace_vector::at");
    return details::inplace_vector::__index(*this, __pos);
  }

  constexpr void pop_back() {
    __IV_EXPECT(size() > 0 && "pop_back from empty inplace_vector!");
    __unsafe_destroy(end() - 1, end());
    __unsafe_set_size(size() - 1);
  }

  constexpr inplace_vector(const inplace_vector &__x)
    requires(N == 0 || std::is_trivially_copy_constructible_v<T>)
  = default;

  constexpr inplace_vector(const inplace_vector &__x)
    requires(N != 0 && !std::is_trivially_copy_constructible_v<T> &&
             std::copyable<T>)
  {
    for (auto &&__e : __x)
      emplace_back(__e);
  }

  constexpr inplace_vector(inplace_vector &&__x)
    requires(N == 0 || std::is_trivially_move_constructible_v<T>)
  = default;

  constexpr inplace_vector(inplace_vector &&__x)
    requires(N != 0 && !std::is_trivially_move_constructible_v<T> &&
             std::movable<T>)
  {
    for (auto &&__e : __x)
      emplace_back(std::move(__e));
  }

  constexpr inplace_vector &operator=(const inplace_vector &__x)
    requires(N == 0 || (std::is_trivially_destructible_v<T> &&
                        std::is_trivially_copy_constructible_v<T> &&
                        std::is_trivially_copy_assignable_v<T>))
  = default;

  constexpr inplace_vector &operator=(const inplace_vector &__x)
    requires(N != 0 &&
             !(std::is_trivially_destructible_v<T> &&
               std::is_trivially_copy_constructible_v<T> &&
               std::is_trivially_copy_assignable_v<T>) &&
             std::copyable<T>)
  {
    clear();
    for (auto &&__e : __x)
      emplace_back(__e);
    return *this;
  }

  constexpr inplace_vector &operator=(inplace_vector &&__x)
    requires(N == 0 || (std::is_trivially_destructible_v<T> &&
                        std::is_trivially_move_constructible_v<T> &&
                        std::is_trivially_move_assignable_v<T>))
  = default;

  constexpr inplace_vector &operator=(inplace_vector &&__x)
    requires(N != 0 &&
             !(std::is_trivially_destructible_v<T> &&
               std::is_trivially_move_constructible_v<T> &&
               std::is_trivially_move_assignable_v<T>) &&
             std::movable<T>)
  {
    clear();
    for (auto &&__e : __x)
      emplace_back(std::move(__e));
    return *this;
  }

  constexpr void
  swap(inplace_vector &__x) noexcept(N == 0 ||
                                     (std::is_nothrow_swappable_v<T> &&
                                      std::is_nothrow_move_constructible_v<T>))
    requires(std::movable<T>)
  {
    auto tmp = std::move(__x);
    __x = std::move(*this);
    (*this) = std::move(tmp);
  }

  template <class __InputIterator>
  constexpr void assign(__InputIterator __first, __InputIterator __last)
    requires(
        std::constructible_from<T, std::iter_reference_t<__InputIterator>> &&
        std::movable<T>)
  {
    clear();
    insert(begin(), __first, __last);
  }
  template <details::inplace_vector::__container_compatible_range<T> __R>
  constexpr void assign_range(__R &&__rg)
    requires(std::constructible_from<T, std::ranges::range_reference_t<__R>> &&
             std::movable<T>)
  {
    assign(std::begin(__rg), std::end(__rg));
  }
  constexpr void assign(size_type __n, const T &__u)
    requires(std::constructible_from<T, const T &> && std::movable<T>)
  {
    clear();
    insert(begin(), __n, __u);
  }
  constexpr void assign(std::initializer_list<T> __il)
    requires(std::constructible_from<
                 T, std::ranges::range_reference_t<std::initializer_list<T>>> &&
             std::movable<T>)
  {
    clear();
    insert_range(begin(), __il);
  }

  constexpr friend int /*synth-three-way-result<T>*/
  operator<=>(const inplace_vector & __x, const inplace_vector & __y) {
    if (__x.size() < __y.size())
      return -1;
    if (__x.size() > __y.size())
      return +1;

    bool __all_equal = true;
    bool __all_less = true;
    for (size_type __i = 0; __i < __x.size(); ++__i) {
      if (__x[__i] < __y[__i])
        __all_equal = false;
      if (__x[__i] == __y[__i])
        __all_less = false;
    }

    if (__all_equal)
      return 0;
    if (__all_less)
      return -1;
    return 1;
  }
};

} // namespace beman

// undefine all the internal macros
#undef __IV_ASSUME
#undef __IV_ASSERT
#undef __IV_EXPECT
