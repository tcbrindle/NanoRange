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

template <typename T>
NANO_CONCEPT Movable = std::is_object<T>::value&& MoveConstructible<T>&&
    Assignable<detail::lref_t<T>, T>&& Swappable<T>;

NANO_END_NAMESPACE

#endif
