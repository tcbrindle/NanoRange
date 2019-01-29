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

// [concepts.lib.corelang.same]
template <typename T, typename U>
NANO_CONCEPT Same = std::is_same<T, U>::value;

// [concepts.lib.corelang.derived]

// FIXME: Spec doesn't use remove_reference_t here, not sure if it should
template <typename Derived, typename Base>
NANO_CONCEPT DerivedFrom = std::is_base_of<Base, Derived>::value&&
    std::is_convertible<const volatile std::remove_reference_t<Derived>*,
                        const volatile std::remove_reference_t<Base>*>::value;

// [concepts.lib.corelang.convertibleto]
namespace detail {

struct ConvertibleTo_req {
    template <typename From, typename To>
    auto requires_(From (&f)()) -> decltype(static_cast<To>(f()));
};

} // namespace detail

// [concepts.lib.corelang.convertibleto]
template <typename From, typename To>
NANO_CONCEPT ConvertibleTo = std::is_convertible<From, To>::value&&
    detail::requires_<detail::ConvertibleTo_req, From, To>;

// [concepts.lib.corelang.commonref]
namespace detail {

template <typename T, typename U>
auto CommonReference_fn(long) -> std::false_type;

template <typename T, typename U>
auto CommonReference_fn(int) -> detail::enable_if_t<
        Same<common_reference_t<T, U>, common_reference_t<U, T>> &&
        ConvertibleTo<T, common_reference_t<T, U>> &&
        ConvertibleTo<U, common_reference_t<T, U>>,
                std::true_type>;
}


template <typename T, typename U>
NANO_CONCEPT CommonReference = decltype(detail::CommonReference_fn<T, U>(0))::value;

// [concepts.lib.corelang.common]
namespace detail {

template <typename T, typename U>
auto Common_fn(long) -> std::false_type;

template <typename T, typename U>
auto Common_fn(int) -> detail::enable_if_t<
    Same<common_type_t<T, U>, common_type_t<U, T>> &&
    ConvertibleTo<T, common_type_t<T, U>> &&
    ConvertibleTo<U, common_type_t<T, U>> &&
    CommonReference<
        std::add_lvalue_reference_t<const T>,
        std::add_lvalue_reference_t<const U>> &&
    CommonReference<
        std::add_lvalue_reference_t<common_type_t<T, U>>,
        common_reference_t<
            std::add_lvalue_reference_t<const T>,
            std::add_lvalue_reference_t<const U>>>,
    std::true_type>;

}


template <typename T, typename U>
NANO_CONCEPT Common = decltype(detail::Common_fn<T, U>(0))::value;

// [concepts.lib.corelang.integral]
template <typename T>
NANO_CONCEPT Integral = std::is_integral<T>::value;

// [concepts.lib.corelang.signedintegral]
template <typename T>
NANO_CONCEPT SignedIntegral = Integral<T>&& std::is_signed<T>::value;

// [concepts.lib.corelang.unsignedintegral]
template <typename T>
NANO_CONCEPT UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

// [concepts.lib.corelang.assignable]

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
auto Assignable_fn(int) -> detail::enable_if_t<
        std::is_lvalue_reference<LHS>::value &&
        CommonReference<const std::remove_reference_t<LHS>&,
                        const std::remove_reference_t<RHS>&> &&
        requires_<Assignable_req, LHS, RHS>,
                std::true_type>;

} // namespace detail

template <typename LHS, typename RHS>
NANO_CONCEPT Assignable = decltype(detail::Assignable_fn<LHS, RHS>(0))::value;

// [concepts.lib.corelang.destructible]
template <typename T>
NANO_CONCEPT Destructible = std::is_nothrow_destructible<T>::value;

// [concepts.lib.corelang.constructible]
template <typename T, typename... Args>
NANO_CONCEPT Constructible =
    Destructible<T>&& std::is_constructible<T, Args...>::value;

// [concepts.lib.corelang.defaultconstructible]
template <typename T>
NANO_CONCEPT DefaultConstructible = Constructible<T>;

// [concepts.lib.corelang.moveconstructible]
template <typename T>
NANO_CONCEPT MoveConstructible = Constructible<T, T>&& ConvertibleTo<T, T>;

// [concepts.lib.corelang.copyconstructible]
namespace detail {

template <typename T>
auto CopyConstructible_fn(long) -> std::false_type;

template <typename T>
auto CopyConstructible_fn(int) -> detail::enable_if_t<
        MoveConstructible<T> &&
        Constructible<T, T&> && ConvertibleTo<T&, T> &&
        Constructible<T, const T&> && ConvertibleTo<const T&, T> &&
        Constructible<T, const T> && ConvertibleTo<const T, T>,
                std::true_type>;


}

template <typename T>
NANO_CONCEPT CopyConstructible = decltype(detail::CopyConstructible_fn<T>(0))::value;

NANO_END_NAMESPACE

#endif
