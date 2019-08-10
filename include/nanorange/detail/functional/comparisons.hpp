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

namespace detail {

template <typename = void, typename = void>
struct less_helper;

template <>
struct less_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(std::less<>{}(std::forward<T>(t), std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return std::less<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct less_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(less_helper<>{}(t, u)))
    {
        return less_helper<>{}(t, u);
    }
};

template <typename, typename = void>
struct less_equal_helper;

template <>
struct less_equal_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(!less_helper<>{}(std::forward<U>(u),
                                           std::forward<T>(t))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return !less_helper<>{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct less_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(u, t)))
    {
        return !less_helper<>{}(u, t);
    }
};

template <typename, typename = void>
struct greater_equal_helper;

template <>
struct greater_equal_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(less_helper<>{}(std::forward<T>(t),
                                          std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return !less_helper<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct greater_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(t, u)))
    {
        return !less_helper<>{}(t, u);
    }
};

} // namespace detail

struct equal_to {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct not_equal_to {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return !ranges::equal_to{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct less {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return std::less<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

struct greater {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return ranges::less{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

template <typename T = void>
struct greater_equal : detail::greater_equal_helper<T> {
};

template <typename T = void>
struct less_equal : detail::less_equal_helper<T> {
};

NANO_END_NAMESPACE

#endif