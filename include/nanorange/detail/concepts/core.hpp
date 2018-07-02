// nanorange/detail/concepts/core.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED

#include <nanorange/detail/concepts/concept_def.hpp>
#include <nanorange/detail/macros.hpp>
#include <nanorange/type_traits.hpp>

#include <functional>

NANO_BEGIN_NAMESPACE

// [concepts.lib.corelang.same]
NANO_CONCEPT_DEF(
    template (class T, class U)
    concept Same,
        std::is_same<T, U>::value
)

// [concepts.lib.corelang.derived]
NANO_CONCEPT_DEF(
    template (class Derived, class Base)
    concept DerivedFrom,
        std::is_base_of<Base, Derived>::value &&
        std::is_convertible<const volatile Derived*, const volatile Base*>::value
)

// [concepts.lib.corelang.convertibleto]
NANO_CONCEPT_DEF(
    template (class From, class To)
    concept ConvertibleTo,
        requires(From (&f)()) (
            static_cast<To>(f())
        ) &&
        std::is_convertible<From, To>::value
)

// [concepts.lib.corelang.commonref]
NANO_CONCEPT_DEF(
    template (class T, class U)
    concept CommonReference,
        Same<common_reference_t<T, U>, common_reference_t<U, T>> &&
        ConvertibleTo<T, common_reference_t<T, U>> &&
        ConvertibleTo<U, common_reference_t<T, U>>
)

// [concepts.lib.corelang.common]
NANO_CONCEPT_DEF(
    template (class T, class U)
    concept Common,
        Same<common_type_t<T, U>, common_type_t<U, T>> &&
        ConvertibleTo<T, common_type_t<T, U>> &&
        ConvertibleTo<U, common_type_t<T, U>> &&
        CommonReference<std::add_lvalue_reference_t<const T>,
                        std::add_lvalue_reference_t<const U>> &&
        CommonReference<
            std::add_lvalue_reference_t<common_type_t<T, U>>,
            common_reference_t<
                std::add_lvalue_reference_t<const T>,
                std::add_lvalue_reference_t<const U>>>
)

// [concepts.lib.corelang.integral]
NANO_CONCEPT_DEF(
    template (class T)
    concept Integral,
        std::is_integral<T>::value
)

// [concepts.lib.corelang.signedintegral]
NANO_CONCEPT_DEF(
    template (class T)
    concept SignedIntegral,
        Integral<T> && std::is_signed<T>::value
)

// [concepts.lib.corelang.unsignedintegral]
NANO_CONCEPT_DEF(
    template (class T)
    concept UnsignedIntegral,
        Integral<T> && !SignedIntegral<T>
)

// [concepts.lib.corelang.assignable]
NANO_CONCEPT_DEF(
    template (class LHS, class RHS)
    concept Assignable,
        requires (LHS lhs, RHS&& rhs) (
            lhs = std::forward<RHS>(rhs),
            requires_<Same<decltype(lhs = std::forward<RHS>(rhs)), LHS>>
        ) &&
        std::is_lvalue_reference<LHS>::value &&
        CommonReference<const std::remove_reference_t<LHS>&,
                        const std::remove_reference_t<RHS>&>
)

namespace detail {

struct Assignable_req {
    template <typename LHS, typename RHS>
    auto requires_(LHS lhs, RHS&& rhs) -> decltype(valid_expr(
        lhs = std::forward<RHS>(rhs),
        requires_expr<Same<decltype(lhs = std::forward<RHS>(rhs)), LHS>>{}));
};

template <typename LHS, typename RHS>
auto Assignable_fn(long) -> std::false_type;

template <typename LHS, typename RHS>
auto Assignable_fn(int) -> std::enable_if_t<
        std::is_lvalue_reference<LHS>::value &&
        CommonReference<const std::remove_reference_t<LHS>&,
                        const std::remove_reference_t<RHS>&> &&
        requires_v<Assignable_req, LHS, RHS>,
                std::true_type>;

} // namespace detail

//template <typename LHS, typename RHS>
//NANO_CONCEPT Assignable = decltype(detail::Assignable_fn<LHS, RHS>(0))::value;

// [concepts.lib.corelang.destructible]
NANO_CONCEPT_DEF(
    template (class T)
    concept Destructible,
        std::is_nothrow_destructible<T>::value
)

// [concepts.lib.corelang.constructible]
NANO_CONCEPT_DEF(
    template (class T, class... Args)
    (concept Constructible)(T, Args...),
        Destructible<T> &&
        std::is_constructible<T, Args...>::value
)

// [concepts.lib.corelang.defaultconstructible]
NANO_CONCEPT_DEF(
    template (class T)
    concept DefaultConstructible,
        Constructible<T>
)

// [concepts.lib.corelang.moveconstructible]
NANO_CONCEPT_DEF(
    template (class T)
    concept MoveConstructible,
        Constructible<T, T> &&
        ConvertibleTo<T, T>
)

// [concepts.lib.corelang.copyconstructible]
NANO_CONCEPT_DEF(
    template (class T)
    concept CopyConstructible,
        MoveConstructible<T> &&
        Constructible<T, T&> && ConvertibleTo<T&, T> &&
        Constructible<T, const T&> && ConvertibleTo<const T&, T> &&
        Constructible<T, const T> && ConvertibleTo<const T, T>
)

NANO_END_NAMESPACE

#endif
