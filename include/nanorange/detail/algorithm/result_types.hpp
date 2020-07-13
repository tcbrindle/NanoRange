// nanorange/detail/algorithm/result_types.hpp
//
// Copyright (c) 2020 Boris Staletic (boris dot staletic at gmail dot com)
// Copyright (c) 2020 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ALGORITHM_RETURN_TYPES
#define NANORANGE_DETAIL_ALGORITHM_RETURN_TYPES

#include <nanorange/detail/macros.hpp>
#include <nanorange/detail/concepts/core.hpp>

#include <type_traits>

NANO_BEGIN_NAMESPACE

template <typename I, typename F>
struct in_fun_result {
    NANO_NO_UNIQUE_ADDRESS I in;
    NANO_NO_UNIQUE_ADDRESS F fun;

    template <typename I2, typename F2,
              std::enable_if_t<convertible_to<const I&, I2> &&
                               convertible_to<const F&, F2>, int> = 0>
    constexpr operator in_fun_result<I2, F2>() const &
    {
        return {in, fun};
    }

    template <typename I2, typename F2,
        std::enable_if_t<convertible_to<I, I2> &&
                         convertible_to<F, F2>, int> = 0>
    constexpr operator in_fun_result<I2, F2>() &&
    {
        return {std::move(in), std::move(fun)};
    }
};

template <typename I1, typename I2>
struct in_in_result {
    NANO_NO_UNIQUE_ADDRESS I1 in1;
    NANO_NO_UNIQUE_ADDRESS I2 in2;

    template <typename II1, typename II2,
        std::enable_if_t<convertible_to<const I1&, II1> &&
                         convertible_to<const I2&, II2>, int> = 0>
    constexpr operator in_in_result<II1, II2>() const &
    {
        return {in1, in2};
    }

    template <typename II1, typename II2,
        std::enable_if_t<convertible_to<I1, II1> &&
                         convertible_to<I2, II2>, int> = 0>
    constexpr operator in_in_result<II1, II2>() &&
    {
        return {std::move(in1), std::move(in2)};
    }
};

template <typename I, typename O>
struct in_out_result {
    NANO_NO_UNIQUE_ADDRESS I in;
    NANO_NO_UNIQUE_ADDRESS O out;

    template <typename I2, typename O2,
              std::enable_if_t<convertible_to<const I&, I2> &&
                               convertible_to<const O&, O2>, int> = 0>
    constexpr operator in_out_result<I2, O2>() const &
    {
        return {in, out};
    }

    template <typename I2, typename O2,
              std::enable_if_t<convertible_to<I, I2> &&
                               convertible_to<O, O2>, int> = 0>
    constexpr operator in_out_result<I2, O2>() &&
    {
        return {std::move(in), std::move(out)};
    }
};

template <typename I1, typename I2, typename O>
struct in_in_out_result {
    NANO_NO_UNIQUE_ADDRESS I1 in1;
    NANO_NO_UNIQUE_ADDRESS I2 in2;
    NANO_NO_UNIQUE_ADDRESS O out;

    template <typename II1, typename II2, typename O2,
              std::enable_if_t<convertible_to<const I1&, II1> &&
                               convertible_to<const I2&, II2> &&
                               convertible_to<const O&, O2>, int> = 0>
    constexpr operator in_in_out_result<II1, II2, O2>() const &
    {
        return {in1, in2, out};
    }

    template <typename II1, typename II2, typename O2,
              std::enable_if_t<convertible_to<I1, II1> &&
                               convertible_to<I2, II2> &&
                               convertible_to<O, O2>, int> = 0>
    constexpr operator in_in_out_result<II1, II2, O2>() &&
    {
        return {std::move(in1), std::move(in2), std::move(out)};
    }
};

template <typename I, typename O1, typename O2>
struct in_out_out_result {
    NANO_NO_UNIQUE_ADDRESS I in;
    NANO_NO_UNIQUE_ADDRESS O1 out1;
    NANO_NO_UNIQUE_ADDRESS O2 out2;

    template <typename II, typename OO1, typename OO2,
              std::enable_if_t<convertible_to<const I&, II> &&
                               convertible_to<const O1&, OO1> &&
                               convertible_to<const O2&, OO2>, int> = 0>
    constexpr operator in_out_out_result<II, OO1, OO2>() const &
    {
        return {in, out1, out2};
    }

    template <typename II, typename OO1, typename OO2,
        std::enable_if_t<convertible_to<I, II> &&
                         convertible_to<O1, OO1> &&
                         convertible_to<O2, OO2>, int> = 0>
    constexpr operator in_out_out_result<II, OO1, OO2>() &&
    {
        return {std::move(in), std::move(out1), std::move(out2)};
    }
};

template <typename T>
struct [[nodiscard]] min_max_result {
    NANO_NO_UNIQUE_ADDRESS T min;
    NANO_NO_UNIQUE_ADDRESS T max;

    template <typename T2,
              std::enable_if_t<convertible_to<const T&, T2>, int> = 0>
    constexpr operator min_max_result<T2>() const &
    {
        return {min, max};
    }

    template <typename T2,
              std::enable_if_t<convertible_to<T, T2>, int> = 0>
    constexpr operator min_max_result<T2>() &&
    {
        return {std::move(min), std::move(max)};
    }
};

template <typename I>
struct in_found_result {
    NANO_NO_UNIQUE_ADDRESS I in;
    bool found;
    template<class I2,
             std::enable_if_t< convertible_to<const I&, I2>, int> = 0>
    constexpr operator in_found_result<I2>() const & {
      return {in, found};
    }
    template<class I2,
             std::enable_if_t< convertible_to<const I&, I2>, int> = 0>
    constexpr operator in_found_result<I2>() && {
      return {std::move(in), found};
    }
};

NANO_END_NAMESPACE

#endif
