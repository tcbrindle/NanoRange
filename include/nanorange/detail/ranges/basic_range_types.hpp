// nanorange/detail/ranges/basic_range_types.hpp
//
// Copyright (c) 2020 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_BASIC_RANGE_TYPES_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_BASIC_RANGE_TYPES_HPP_INCLUDED

#include <nanorange/detail/ranges/begin_end.hpp>
#include <nanorange/detail/ranges/range_concept.hpp>

NANO_BEGIN_NAMESPACE

template <typename T>
using iterator_t = decltype(ranges::begin(std::declval<T&>()));

template <typename R>
using sentinel_t =
    std::enable_if_t<range<R>, decltype(ranges::end(std::declval<R&>()))>;

template <typename R>
using range_difference_t =
    std::enable_if_t<range<R>, iter_difference_t<iterator_t<R>>>;

template <typename R>
using range_value_t = std::enable_if_t<range<R>, iter_value_t<iterator_t<R>>>;

template <typename R>
using range_reference_t =
    std::enable_if_t<range<R>, iter_reference_t<iterator_t<R>>>;

template <typename R>
using range_rvalue_reference_t =
    std::enable_if_t<range<R>, iter_rvalue_reference_t<iterator_t<R>>>;

NANO_END_NAMESPACE

#endif
