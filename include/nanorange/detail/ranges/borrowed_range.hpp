// nanorange/detail/ranges/borrowed_range.hpp
//
// Copyright (c) 2020 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_BORROWED_RANGE_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_BORROWED_RANGE_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

NANO_BEGIN_NAMESPACE

template <typename>
inline constexpr bool enable_borrowed_range = false;

NANO_END_NAMESPACE

#endif
