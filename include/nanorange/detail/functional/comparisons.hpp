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
#include <utility>

NANO_BEGIN_NAMESPACE

// [range.comparisons]

struct equal_to {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<equality_comparable_with<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct not_equal_to {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<equality_comparable_with<T, U>, bool>
    {
        return !ranges::equal_to{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct less {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<totally_ordered_with<T, U>, bool>
    {
        return std::less<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct greater {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<totally_ordered_with<T, U>, bool>
    {
        return ranges::less{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

struct greater_equal {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
    -> std::enable_if_t<totally_ordered_with<T, U>, bool>
    {
        return !ranges::less{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct less_equal {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<totally_ordered_with<T, U>, bool>
    {
        return !ranges::less{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

NANO_END_NAMESPACE

#endif