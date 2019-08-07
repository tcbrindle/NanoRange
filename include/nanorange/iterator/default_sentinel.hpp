// nanorange/iterator/default_sentinel.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_DEFAULT_SENTINEL_HPP_INCLUDED
#define NANORANGE_ITERATOR_DEFAULT_SENTINEL_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

NANO_BEGIN_NAMESPACE

struct default_sentinel_t {};

inline constexpr default_sentinel_t default_sentinel{};

NANO_END_NAMESPACE

#endif
