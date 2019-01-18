// nanorange/detail/functional/decay_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_DECAY_COPY_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_DECAY_COPY_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <type_traits>
#include <utility>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
constexpr std::decay_t<T> decay_copy(T &&t) noexcept(
    noexcept(static_cast<std::decay_t<T>>(std::forward<T>(t))))
{
    return std::forward<T>(t);
}

} // namespace detail

NANO_END_NAMESPACE

#endif
