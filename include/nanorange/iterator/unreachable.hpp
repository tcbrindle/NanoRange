// nanorange/iterator/unreachable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_UNREACHABLE_HPP_INCLUDED
#define NANORANGE_ITERATOR_UNREACHABLE_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.unreachable.sentinels]

struct unreachable_sentinel_t {
    template<typename I>
    friend constexpr std::enable_if_t<weakly_incrementable<I>, bool>
    operator==(const I &, unreachable_sentinel_t) noexcept
    {
        return false;
    }

    template<typename I>
    friend constexpr std::enable_if_t<weakly_incrementable<I>, bool>
    operator==(unreachable_sentinel_t, const I &) noexcept
    {
        return false;
    }

    template<typename I>
    friend constexpr std::enable_if_t<weakly_incrementable<I>, bool>
    operator!=(const I &, unreachable_sentinel_t) noexcept
    {
        return true;
    }

    template<typename I>
    friend constexpr std::enable_if_t<weakly_incrementable<I>, bool>
    operator!=(unreachable_sentinel_t, const I &) noexcept
    {
        return true;
    }
};

inline constexpr unreachable_sentinel_t unreachable_sentinel{};

NANO_END_NAMESPACE

#endif
