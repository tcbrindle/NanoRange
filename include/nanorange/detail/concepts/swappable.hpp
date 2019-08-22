// nanorange/detail/concepts/swappable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_SWAPPABLE_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_SWAPPABLE_HPP_INCLUDED

#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/swap.hpp>

NANO_BEGIN_NAMESPACE

// [concept.swappable]
namespace detail {

struct swappable_concept {
    template <typename T>
    auto requires_(T& a, T& b) -> decltype(ranges::swap(a, b));
};

}

template <typename T>
NANO_CONCEPT swappable = detail::requires_<detail::swappable_concept, T>;

namespace detail {

struct swappable_with_concept {
    template <typename T, typename U>
    auto requires_(T&& t, U&& u) -> decltype(
        ranges::swap(std::forward<T>(t), std::forward<T>(t)),
        ranges::swap(std::forward<U>(u), std::forward<U>(u)),
        ranges::swap(std::forward<T>(t), std::forward<U>(u)),
        ranges::swap(std::forward<U>(u), std::forward<T>(t))
    );

    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        common_reference_with<
            const std::remove_reference_t<T>&,
            const std::remove_reference_t<U>&> &&
        detail::requires_<swappable_with_concept, T, U>,
        std::true_type>;

};

} // namespace detail

template <typename T, typename U>
NANO_CONCEPT swappable_with =
    decltype(detail::swappable_with_concept::test<T, U>(0))::value;

NANO_END_NAMESPACE

#endif
