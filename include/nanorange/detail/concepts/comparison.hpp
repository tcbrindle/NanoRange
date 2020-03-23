// nanorange/detail/concepts/comparison.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED

#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/concepts/swappable.hpp>

NANO_BEGIN_NAMESPACE

// [concept.boolean_testable]
namespace detail {

template <typename T>
NANO_CONCEPT boolean_testable_impl = convertible_to<T, bool>;

struct boolean_testable_concept {
    template <typename T>
    auto requires_(T&& t) ->
        requires_expr<boolean_testable_impl<decltype(!std::forward<T>(t))>>;
};

template <typename T>
NANO_CONCEPT boolean_testable =
    boolean_testable_impl<T> &&
    detail::requires_<boolean_testable_concept, T>;

} // namespace detail

// [concept.equalitycomparable]
namespace detail {

struct weakly_equality_comparable_with_concept {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            requires_expr<boolean_testable<decltype(t == u)>>{},
            requires_expr<boolean_testable<decltype(t != u)>>{},
            requires_expr<boolean_testable<decltype(u == t)>>{},
            requires_expr<boolean_testable<decltype(u != t)>>{});
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

struct partially_ordered_with_concept {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
    -> decltype(requires_expr<boolean_testable<decltype(t < u)>>{},
        requires_expr<boolean_testable<decltype(t > u)>>{},
        requires_expr<boolean_testable<decltype(t <= u)>>{},
        requires_expr<boolean_testable<decltype(t >= u)>>{},
        requires_expr<boolean_testable<decltype(u < t)>>{},
        requires_expr<boolean_testable<decltype(u > t)>>{},
        requires_expr<boolean_testable<decltype(u <= t)>>{},
        requires_expr<boolean_testable<decltype(u >= t)>>{});
};

template <typename T, typename U>
NANO_CONCEPT partially_ordered_with =
    detail::requires_<detail::partially_ordered_with_concept, T, U>;

}

template <typename T>
NANO_CONCEPT totally_ordered =
    equality_comparable<T> && detail::partially_ordered_with<T, T>;

namespace detail {

struct totally_ordered_with_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        totally_ordered<T> && totally_ordered<U> &&
        equality_comparable_with<T, U> &&
        totally_ordered<
            common_reference_t<
                const std::remove_reference_t<T>&,
                const std::remove_reference_t<U>&>> &&
        partially_ordered_with<T, U>,
        std::true_type>;
};

} // namespace detail

template <typename T, typename U>
NANO_CONCEPT totally_ordered_with =
    decltype(detail::totally_ordered_with_concept::test<T, U>(0))::value;

NANO_END_NAMESPACE

#endif
