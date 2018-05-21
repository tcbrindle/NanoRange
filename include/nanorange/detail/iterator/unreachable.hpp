// nanorange/detail/iterator/unreachable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_UNREACHABLE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_UNREACHABLE_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.unreachable.sentinels]

class unreachable {
};

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool> operator==(const I&,
                                                         unreachable) noexcept
{
    return false;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool> operator==(unreachable,
                                                         const I&) noexcept
{
    return false;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool> operator!=(const I&,
                                                         unreachable) noexcept
{
    return true;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool> operator!=(unreachable,
                                                         const I&) noexcept
{
    return true;
}

NANO_END_NAMESPACE

#endif
