// nanorange/detail/functional/comparisons.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_COMPARISONS_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_COMPARISONS_HPP_INCLUDED

#include <nanorange/detail/concepts/comparison.hpp>
#include <nanorange/detail/concepts/core.hpp>

#include <functional>

NANO_BEGIN_NAMESPACE

// [range.comparisons]

// TODO: Constrained versions of the rest of these
template <typename = void, typename = void>
struct equal_to;

template <typename T>
struct equal_to<T, std::enable_if_t<EqualityComparable<T>>> : std::equal_to<T> {
};

template <>
struct equal_to<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u)
        -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

using std::greater;
using std::greater_equal;
using std::less;
using std::less_equal;
using std::not_equal_to;

NANO_END_NAMESPACE

#endif