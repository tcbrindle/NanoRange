// nanorange/detail/concepts/movable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_MOVABLE_HPP
#define NANORANGE_DETAIL_CONCEPTS_MOVABLE_HPP

#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/concepts/swappable.hpp>

// Movable is listed as an object concept, but is required for the definition
// of Boolean, so we treat it specially

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
auto Movable_fn(long) -> std::false_type;

template <typename T>
auto Movable_fn(int) -> detail::enable_if_t<
        std::is_object<T>::value &&
        MoveConstructible<T> &&
        Assignable<T&, T> &&
        Swappable<T>,
                std::true_type>;

}

template <typename T>
NANO_CONCEPT Movable = decltype(detail::Movable_fn<T>(0))::value;

NANO_END_NAMESPACE

#endif
