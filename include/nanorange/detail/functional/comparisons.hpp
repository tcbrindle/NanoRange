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
namespace detail {

template <typename = void, typename = void>
struct equal_to_helper;

template <typename T>
struct equal_to_helper<T, std::enable_if_t<EqualityComparable<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(equal_to_helper<>{}(t, u)))
    {
        return equal_to_helper<>{}(t, u);
    }
};

template <>
struct equal_to_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(std::equal_to<>{}(std::forward<T>(t),
                                            std::forward<U>(u))))
            -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename, typename = void>
struct not_equal_to_helper;

template <typename T>
struct not_equal_to_helper<T, std::enable_if_t<EqualityComparable<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!equal_to_helper<>{}(t, u)))
    {
        return !equal_to_helper<>{}(t, u);
    }
};

template <>
struct not_equal_to_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(!equal_to_helper<>{}(std::forward<T>(t),
                                               std::forward<U>(u))))
            -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return !equal_to_helper<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename = void, typename = void>
struct less_helper;

template <typename T>
struct less_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(less_helper<>{}(t, u)))
    {
        return less_helper<>{}(t, u);
    }
};

template <>
struct less_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(std::less<>(std::forward<T>(t), std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return std::less<>(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename, typename = void>
struct greater_helper;

template <typename T>
struct greater_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(less_helper<>{}(u, t)))
    {
        return less_helper<>{}(u, t);
    }
};

template <>
struct greater_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(less_helper<>{}(std::forward<T>(t),
                                          std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return less_helper<>{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

template <typename, typename = void>
struct less_equal_helper;

template <typename T>
struct less_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(u, t)))
    {
        return !less_helper<>{}(u, t);
    }
};

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

template <typename, typename = void>
struct greater_equal_helper;

template <typename T>
struct greater_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(t, u)))
    {
        return !less_helper<>{}(t, u);
    }
};

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

} // namespace detail

template <typename T = void>
struct equal_to : detail::equal_to_helper<T> {
};

template <typename T = void>
struct not_equal_to : detail::not_equal_to_helper<T> {
};

template <typename T = void>
struct less : detail::less_helper<T> {
};

template <typename T = void>
struct greater : detail::greater_helper<T> {
};

template <typename T = void>
struct greater_equal : detail::greater_equal_helper<T> {
};

template <typename T = void>
struct less_equal : detail::less_equal_helper<T> {
};

NANO_END_NAMESPACE

#endif