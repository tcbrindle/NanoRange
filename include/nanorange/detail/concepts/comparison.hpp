// nanorange/detail/concepts/comparison.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED

#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/concepts/movable.hpp>
#include <nanorange/detail/concepts/swappable.hpp>

NANO_BEGIN_NAMESPACE

// [concept.boolean]
namespace detail {

struct boolean_concept {
    template <typename B>
    auto requires_(const std::remove_reference_t<B>& b1,
                   const std::remove_reference_t<B>& b2, const bool a)
        -> decltype(
            requires_expr<convertible_to<decltype(b1), bool>>{},
            requires_expr<convertible_to<decltype(!b1), bool>>{},
            requires_expr<same_as<decltype(b1 && b2), bool>>{},
            requires_expr<same_as<decltype(b1 && a ), bool>>{},
            requires_expr<same_as<decltype( a && b2), bool>>{},
            requires_expr<same_as<decltype(b1 || b2), bool>>{},
            requires_expr<same_as<decltype(b1 || a ), bool>>{},
            requires_expr<same_as<decltype( a || b2), bool>>{},
            requires_expr<convertible_to<decltype(b1 == b2), bool>>{},
            requires_expr<convertible_to<decltype(b1 == a ), bool>>{},
            requires_expr<convertible_to<decltype( a == b2), bool>>{},
            requires_expr<convertible_to<decltype(b1 != b2), bool>>{},
            requires_expr<convertible_to<decltype(b1 != a ), bool>>{},
            requires_expr<convertible_to<decltype( a != b2), bool>>{});
};

} // namespace detail

template <typename B>
NANO_CONCEPT boolean = movable<remove_cvref_t<B>> &&
    detail::requires_<detail::boolean_concept, B>;

// [concept.equalitycomparable]
namespace detail {

struct weakly_equality_comparable_with_concept {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            requires_expr<boolean<decltype(t == u)>>{},
            requires_expr<boolean<decltype(t != u)>>{},
            requires_expr<boolean<decltype(u == t)>>{},
            requires_expr<boolean<decltype(u != t)>>{});
};

template <typename T, typename U>
NANO_CONCEPT weakly_equality_comparable_with =
    requires_<weakly_equality_comparable_with_concept, T, U>;

} // namespace detail

template <typename T>
NANO_CONCEPT equality_comparable = detail::weakly_equality_comparable_with<T, T>;

namespace detail {

struct equality_comparable_with_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        equality_comparable<T> && equality_comparable<U> &&
        common_reference_with<const std::remove_reference_t<T>&,
                              const std::remove_reference_t<U>&> &&
        equality_comparable<
            common_reference_t<
                const std::remove_reference_t<T>&,
                const std::remove_reference_t<U>&>> &&
        weakly_equality_comparable_with<T, U>,
        std::true_type>;
};

}

template <typename T, typename U>
NANO_CONCEPT equality_comparable_with =
    decltype(detail::equality_comparable_with_concept::test<T, U>(0))::value;

// [concepts.totallyordered]
namespace detail {

struct totally_ordered_concept {
    template <typename T>
    auto requires_(const std::remove_reference_t<T>& a,
                   const std::remove_reference_t<T>& b) -> decltype(
        requires_expr<boolean<decltype(a < b)>>{},
        requires_expr<boolean<decltype(a > b)>>{},
        requires_expr<boolean<decltype(a <= b)>>{},
        requires_expr<boolean<decltype(a >= b)>>{});
};

} // namespace detail

template <typename T>
NANO_CONCEPT totally_ordered = equality_comparable<T>&&
    detail::requires_<detail::totally_ordered_concept, T>;

namespace detail {

struct totally_ordered_with_concept {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u) -> decltype(
        requires_expr<boolean<decltype(t <  u)>>{},
        requires_expr<boolean<decltype(t >  u)>>{},
        requires_expr<boolean<decltype(t <= u)>>{},
        requires_expr<boolean<decltype(t >= u)>>{},
        requires_expr<boolean<decltype(u <  t)>>{},
        requires_expr<boolean<decltype(u >  t)>>{},
        requires_expr<boolean<decltype(u <= t)>>{},
        requires_expr<boolean<decltype(u >= t)>>{}
    );

    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        totally_ordered<T> && totally_ordered<U> &&
        totally_ordered<
            common_reference_t<
                const std::remove_reference_t<T>&,
                const std::remove_reference_t<U>&>> &&
        equality_comparable_with<T, U> &&
        detail::requires_<totally_ordered_with_concept, T, U>,
        std::true_type>;
};

} // namespace detail

template <typename T, typename U>
NANO_CONCEPT totally_ordered_with =
    decltype(detail::totally_ordered_with_concept::test<T, U>(0))::value;

NANO_END_NAMESPACE

#endif
