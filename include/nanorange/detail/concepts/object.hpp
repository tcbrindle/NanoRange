// nanorange/detail/concepts/object.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_OBJECT_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_OBJECT_HPP_INCLUDED

#include <nanorange/detail/concepts/comparison.hpp>
#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/concepts/movable.hpp>
#include <nanorange/detail/functional/invoke.hpp>

NANO_BEGIN_NAMESPACE

// [concepts.lib.object.copyable]
namespace detail {

template <typename>
auto Copyable_fn(long) -> std::false_type;

template <typename T>
auto Copyable_fn(int) -> std::enable_if_t<
        CopyConstructible<T> &&
        Movable<T> &&
        Assignable<T&, const T&>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT Copyable = decltype(detail::Copyable_fn<T>(0))::value;

// [concepts.lib.object.semiregular]
template <typename T>
NANO_CONCEPT Semiregular = Copyable<T>&& DefaultConstructible<T>;

// [concepts.lib.object.regular]
template <typename T>
NANO_CONCEPT Regular = Semiregular<T>&& EqualityComparable<T>;

namespace detail {

struct Invocable_req {
    /*template <typename F, typename... Args>
    auto requires_v(F&& f, Args&&... args) -> decltype(
        nano::invoke(std::forward<F>(f), std::forward<Args>(args)...)
    );*/
    // FIXME: Clang really doesn't like the above, work out why
    template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) -> invoke_result_t<F, Args...>;
};

} // namespace detail

template <typename F, typename... Args>
NANO_CONCEPT Invocable = detail::requires_v<detail::Invocable_req, F, Args...>;

template <typename F, typename... Args>
NANO_CONCEPT RegularInvocable = Invocable<F, Args...>;

namespace detail {

template <typename, typename...>
auto Predicate_fn(long) -> std::false_type;

template <typename F, typename... Args>
auto Predicate_fn(int) -> std::enable_if_t<
        RegularInvocable<F, Args...> &&
        Boolean<invoke_result_t<F, Args...>>,
        std::true_type>;

}

template <typename F, typename... Args>
NANO_CONCEPT Predicate = decltype(detail::Predicate_fn<F, Args...>(0))::value;

namespace detail {

template <typename, typename, typename>
auto Relation_fn(long) -> std::false_type;

template <typename R, typename T, typename U>
auto Relation_fn(int) -> std::enable_if_t<
        Predicate<R, T, T> && Predicate<R, U, U> &&
        CommonReference<const std::remove_reference_t<T>&,
                        const std::remove_reference_t<U>&> &&
        Predicate<R,
                  common_reference_t<const std::remove_reference_t<T>&,
                                     const std::remove_reference_t<U>&>,
                  common_reference_t<const std::remove_reference_t<T>&,
                                     const std::remove_reference_t<U>&>> &&
        Predicate<R, T, U> && Predicate<R, U, T>,
                std::true_type>;

}

template <typename R, typename T, typename U>
NANO_CONCEPT Relation = decltype(detail::Relation_fn<R, T, U>(0))::value;

template <typename R, typename T, typename U>
NANO_CONCEPT StrictWeakOrder = Relation<R, T, U>;

NANO_END_NAMESPACE

#endif
