// nanorange/detail/concepts/core.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>
#include <nanorange/type_traits.hpp>

#include <utility>

NANO_BEGIN_NAMESPACE

// [concept.same]
template <typename T, typename U>
NANO_CONCEPT same_as = std::is_same_v<T, U>;

// [concept.derived]
namespace detail {

struct derived_from_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename Derived, typename Base>
    static auto test(int) -> std::enable_if_t<
        std::is_base_of_v<Base, Derived> &&
        std::is_convertible_v<const volatile Derived*, const volatile Base*>,
        std::true_type>;

};

}

template <typename Derived, typename Base>
NANO_CONCEPT derived_from =
    decltype(detail::derived_from_concept::test<Derived, Base>(0))::value;

// [concept.convertible]
namespace detail {

struct convertible_to_concept {
    template <typename From, typename To>
    auto requires_(From (&f)()) -> decltype(static_cast<To>(f()));
};

} // namespace detail

template <typename From, typename To>
NANO_CONCEPT convertible_to =
    std::is_convertible_v<From, To> &&
    detail::requires_<detail::convertible_to_concept, From, To>;

// [concept.commonref]
namespace detail {

struct common_reference_with_concept {
    template <typename T, typename U>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        same_as<common_reference_t<T, U>, common_reference_t<U, T>> &&
            convertible_to<T, common_reference_t<T, U>> &&
            convertible_to<U, common_reference_t<T, U>>,
        std::true_type>;
};

}

template <typename T, typename U>
NANO_CONCEPT common_reference_with =
    decltype(detail::common_reference_with_concept::test<T, U>(0))::value;

// [concepts.common]
namespace detail {

struct common_with_concept {
    template <typename T, typename U>
    auto requires_() -> decltype(
        static_cast<common_type_t<T, U>>(std::declval<T>()),
        static_cast<common_type_t<T, U>>(std::declval<U>())
    );

    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U>
    static auto test(int) -> std::enable_if_t<
        same_as<common_type_t<T, U>, common_type_t<U, T>> &&
        detail::requires_<common_with_concept, T, U> &&
        common_reference_with<
            std::add_lvalue_reference_t<const T>,
            std::add_lvalue_reference_t<const U>> &&
        common_reference_with<
            std::add_lvalue_reference_t<common_type_t<T, U>>,
            common_reference_t<
                std::add_lvalue_reference_t<const T>,
                std::add_lvalue_reference_t<const U>>>,
        std::true_type>;

};

}

template <typename T, typename U>
NANO_CONCEPT common_with =
    decltype(detail::common_with_concept::test<T, U>(0))::value;

// [concept.arithmetic]
template <typename T>
NANO_CONCEPT integral = std::is_integral_v<T>;

template <typename T>
NANO_CONCEPT signed_integral = integral<T> && std::is_signed_v<T>;

template <typename T>
NANO_CONCEPT unsigned_integral = integral<T> && !signed_integral<T>;

// [concept.assignable]

namespace detail {

struct assignable_from_concept {
    template <typename LHS, typename RHS>
    auto requires_(LHS lhs, RHS&& rhs) -> decltype(
        requires_expr<same_as<decltype(lhs = std::forward<RHS>(rhs)), LHS>>{});

    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename LHS, typename RHS>
    static auto test(int) -> std::enable_if_t<
        std::is_lvalue_reference_v<LHS> &&
        common_reference_with<
            const std::remove_reference_t<LHS>&,
            const std::remove_reference_t<RHS>&> &&
        detail::requires_<assignable_from_concept, LHS, RHS>,
        std::true_type>;
};

} // namespace detail

template <typename LHS, typename RHS>
NANO_CONCEPT assignable_from =
    decltype(detail::assignable_from_concept::test<LHS, RHS>(0))::value;

// [concept.destructible]
template <typename T>
NANO_CONCEPT destructible = std::is_nothrow_destructible_v<T>;

// [concept.constructible]
template <typename T, typename... Args>
NANO_CONCEPT constructible_from =
    destructible<T> && std::is_constructible_v<T, Args...>;

// [concept.defaultconstructible]
template <typename T>
NANO_CONCEPT default_constructible = constructible_from<T>;

// [concept.moveconstructible]
template <typename T>
NANO_CONCEPT move_constructible =
    constructible_from<T, T> && convertible_to<T, T>;

// [concept.copyconstructible]
namespace detail {

struct copy_constructible_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        move_constructible<T> &&
        constructible_from<T, T&> && convertible_to<T&, T> &&
        constructible_from<T, const T&> && convertible_to<const T&, T> &&
        constructible_from<T, const T> && convertible_to<const T, T>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT copy_constructible =
    decltype(detail::copy_constructible_concept::test<T>(0))::value;

NANO_END_NAMESPACE

#endif
