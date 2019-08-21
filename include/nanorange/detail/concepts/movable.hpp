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

// [concept.movable]
namespace detail {

struct movable_concept {
    template <typename T>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        std::is_object_v<T> && move_constructible<T> &&
        assignable_from<T&, T> && swappable<T>,
        std::true_type>;
};
}

template <typename T>
NANO_CONCEPT movable = decltype(detail::movable_concept::test<T>(0))::value;

NANO_END_NAMESPACE

#endif
