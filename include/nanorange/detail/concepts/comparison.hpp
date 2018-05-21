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

// [concepts.lib.compare.boolean]

namespace detail {

struct Boolean_req {
    template <typename B>
    auto requires_(const std::remove_reference_t<B>& b1,
                   const std::remove_reference_t<B>& b2, const bool a)
        -> decltype(valid_expr(
            requires_expr<
                ConvertibleTo<const std::remove_reference_t<B&>, bool>>{},
            !b1, requires_expr<ConvertibleTo<decltype(!b1), bool>>{}, b1&& a,
            requires_expr<Same<decltype(b1 && a), bool>>{}, b1 || a,
            requires_expr<Same<decltype(b1 || a), bool>>{}, b1&& b2,
            requires_expr<Same<decltype(b1 && b2), bool>>{}, a&& b2,
            requires_expr<Same<decltype(a && b2), bool>>{}, b1 || b2,
            requires_expr<Same<decltype(b1 || b2), bool>>{}, a || b2,
            requires_expr<Same<decltype(a || b2), bool>>{}, b1 == b2,
            requires_expr<ConvertibleTo<decltype(b1 == b2), bool>>{}, b1 == a,
            requires_expr<ConvertibleTo<decltype(b1 == a), bool>>{}, a == b2,
            requires_expr<ConvertibleTo<decltype(a == b2), bool>>{}, b1 != b2,
            requires_expr<ConvertibleTo<decltype(b1 != b2), bool>>{}, b1 != a,
            requires_expr<ConvertibleTo<decltype(b1 != a), bool>>{}, a != b2,
            requires_expr<ConvertibleTo<decltype(a != b2), bool>>{}));
};

} // namespace detail

template <typename B>
NANO_CONCEPT Boolean = Movable<detail::remove_cvref_t<B>>&&
    detail::requires_<detail::Boolean_req, B>;

// [concepts.lib.compare.equalitycomparable]

namespace detail {

struct WeaklyEqualityComparableWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            valid_expr(t == u, requires_expr<Boolean<decltype(t == u)>>{},
                       t != u, requires_expr<Boolean<decltype(t != u)>>{},
                       u == t, requires_expr<Boolean<decltype(u == t)>>{},
                       u != t, requires_expr<Boolean<decltype(u != t)>>{}));
};

template <typename T, typename U>
NANO_CONCEPT WeaklyEqualityComparableWith =
    requires_<WeaklyEqualityComparableWith_req, T, U>;

} // namespace detail

template <typename T>
NANO_CONCEPT EqualityComparable = detail::WeaklyEqualityComparableWith<T, T>;

template <typename T, typename U>
NANO_CONCEPT EqualityComparableWith =
    EqualityComparable<T>&& EqualityComparable<U>&&
        CommonReference<detail::clref_t<std::remove_reference_t<T>>,
                        detail::clref_t<std::remove_reference_t<U>>>&&
            EqualityComparable<detail::checked_common_ref_t<
                detail::clref_t<std::remove_reference_t<T>>,
                detail::clref_t<std::remove_reference_t<U>>>>&&
                detail::WeaklyEqualityComparableWith<T, U>;

// [concepts.lib.compare.stricttotallyordered]

namespace detail {

struct StrictTotallyOrdered_req {
    template <typename T>
    auto requires_(const std::remove_reference_t<T>& a,
                   const std::remove_reference_t<T>& b)
        -> decltype(
            valid_expr(a<b, requires_expr<Boolean<decltype(a < b)>>{}, a> b,
                       requires_expr<Boolean<decltype(a > b)>>{}, a <= b,
                       requires_expr<Boolean<decltype(a <= b)>>{}, a >= b,
                       requires_expr<Boolean<decltype(a >= b)>>{}));
};

} // namespace detail

template <typename T>
NANO_CONCEPT StrictTotallyOrdered = EqualityComparable<T>&&
    detail::requires_<detail::StrictTotallyOrdered_req, T>;

namespace detail {

struct StrictTotallyOrderedWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            valid_expr(t<u, requires_expr<Boolean<decltype(t < u)>>{}, t> u,
                       requires_expr<Boolean<decltype(t > u)>>{}, t <= u,
                       requires_expr<Boolean<decltype(t <= u)>>{}, t >= u,
                       requires_expr<Boolean<decltype(t >= u)>>{},
                       u<t, requires_expr<Boolean<decltype(u < t)>>{}, u> t,
                       requires_expr<Boolean<decltype(u > t)>>{}, u <= t,
                       requires_expr<Boolean<decltype(u <= t)>>{}, u >= t,
                       requires_expr<Boolean<decltype(u >= t)>>{}));
};

} // namespace detail

template <typename T, typename U>
NANO_CONCEPT StrictTotallyOrderedWith =
    StrictTotallyOrdered<T>&& StrictTotallyOrdered<U>&&
        CommonReference<detail::clref_t<std::remove_reference_t<T>>,
                        detail::clref_t<std::remove_reference_t<U>>>&&
            StrictTotallyOrdered<detail::checked_common_ref_t<
                detail::clref_t<std::remove_reference_t<T>>,
                detail::clref_t<std::remove_reference_t<T>>>>&&
                EqualityComparableWith<T, U>&& detail::requires_<
                    detail::StrictTotallyOrderedWith_req, T, U>;

NANO_END_NAMESPACE

#endif
