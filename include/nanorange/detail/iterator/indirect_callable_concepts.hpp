// nanorange/detail/iterator/indirect_callable_concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.indirectcallable.indirectinvocable]

template <typename T>
using iter_common_reference_t = std::enable_if_t<Readable<T>,
        common_reference_t<iter_reference_t<T>, iter_value_t<T>&>>;

namespace detail {

template <typename, typename>
auto IndirectUnaryInvocable_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectUnaryInvocable_fn(int) -> std::enable_if_t<
        Readable<I> &&
        copy_constructible<F> &&
        Invocable<F&, iter_value_t<I>&> &&
        Invocable<F&, iter_reference_t<I>> &&
        Invocable<F&, iter_common_reference_t<I>> &&
        common_reference_with<
                invoke_result_t<F&, iter_value_t<I>&>,
                invoke_result_t<F&, iter_reference_t<I>&>>,
            std::true_type>;

}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryInvocable =
        decltype(detail::IndirectUnaryInvocable_fn<F, I>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectRegularUnaryInvocable_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectRegularUnaryInvocable_fn(int) -> std::enable_if_t<
        Readable<I> &&
        copy_constructible<F> &&
        RegularInvocable<F&, iter_value_t<I>&> &&
        RegularInvocable<F&, iter_reference_t<I>> &&
        RegularInvocable<F&, iter_common_reference_t<I>> &&
        common_reference_with<
            invoke_result_t<F&, iter_value_t<I>&>,
            invoke_result_t<F&, iter_reference_t<I>&>>,
        std::true_type>;

}


template <typename F, typename I>
NANO_CONCEPT IndirectRegularUnaryInvocable =
        decltype(detail::IndirectRegularUnaryInvocable_fn<F, I>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectUnaryPredicate_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectUnaryPredicate_fn(int) -> std::enable_if_t<
        Readable<I> &&
        copy_constructible<F> &&
        Predicate<F&, iter_value_t<I>&> &&
        Predicate<F&, iter_reference_t<I>> &&
        Predicate<F&, iter_common_reference_t<I>>,
            std::true_type>;

}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryPredicate =
        decltype(detail::IndirectUnaryPredicate_fn<F, I>(0))::value;

namespace detail {

template <typename F, typename I1, typename I2>
auto IndirectRelation_fn(long) -> std::false_type;

template <typename F, typename I1, typename I2>
auto IndirectRelation_fn(int) -> std::enable_if_t<
        Readable<I1> && Readable<I2> && copy_constructible<F> &&
        Relation<F&, iter_value_t<I1>&, iter_value_t<I2>&>&&
        Relation<F&, iter_value_t<I1>&, iter_reference_t<I2>>&&
        Relation<F&, iter_reference_t<I1>, iter_value_t<I2>&>&&
        Relation<F&, iter_reference_t<I1>, iter_reference_t<I2>>&&
        Relation<F&,
            iter_common_reference_t<I1>,
            iter_common_reference_t<I2>>,
    std::true_type>;

}


template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectRelation =
    decltype(detail::IndirectRelation_fn<F, I1, I2>(0))::value;


namespace detail {

template <typename, typename, typename>
auto IndirectStrictWeakOrder_fn(long) -> std::false_type;

template <typename F, typename I1, typename I2>
auto IndirectStrictWeakOrder_fn(int) -> std::enable_if_t<
        Readable<I1> &&
        Readable<I2> &&
        StrictWeakOrder<F&, iter_value_t<I1>&, iter_value_t<I2>&> &&
        StrictWeakOrder<F&, iter_value_t<I1>&, iter_reference_t<I2>> &&
        StrictWeakOrder<F&, iter_reference_t<I1>, iter_value_t<I2>&> &&
        StrictWeakOrder<F&, iter_reference_t<I1>, iter_reference_t<I2>> &&
        StrictWeakOrder<F&, iter_common_reference_t<I1>, iter_common_reference_t<I2>>,
    std::true_type>;

}

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectStrictWeakOrder =
        decltype(detail::IndirectStrictWeakOrder_fn<F, I1, I2>(0))::value;

namespace detail {

template <bool...>
struct all_readable_helper;

template <>
struct all_readable_helper<> : std::true_type {};

template <bool First, bool... Rest>
struct all_readable_helper<First, Rest...>
    : std::conditional_t<First, all_readable_helper<Rest...>, std::false_type> {};

template <typename... Is>
constexpr bool all_readable_v = all_readable_helper<Readable<Is>...>::value;

} // namespace detail

template <typename F, typename... Is>
using indirect_result_t = std::enable_if_t<
        detail::all_readable_v<Is...> &&
        Invocable<F, iter_reference_t<Is>...>,
        invoke_result_t<F, iter_reference_t<Is>...>>;

// range.commonalgoreq.indirectlymovable]

namespace detail {

template <typename, typename>
auto IndirectlyMovable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyMovable_fn(int) -> std::enable_if_t<
        Readable<In> &&
        Writable<Out, iter_rvalue_reference_t<In>>,
    std::true_type>;


}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovable =
        decltype(detail::IndirectlyMovable_fn<In, Out>(0))::value;

namespace detail {

template <typename In, typename Out>
auto IndirectlyMovableStorable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyMovableStorable_fn(int) -> std::enable_if_t<
        IndirectlyMovable<In, Out> &&
        Writable<Out, iter_value_t<In>> &&
        Movable<iter_value_t<In>> &&
        constructible_from<iter_value_t<In>, iter_rvalue_reference_t<In>> &&
        assignable_from<iter_value_t<In>&, iter_rvalue_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovableStorable =
        decltype(detail::IndirectlyMovableStorable_fn<In, Out>(0))::value;

// range.commonalgoreq.indirectlycopyable

namespace detail {

template <typename, typename>
auto IndirectlyCopyable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyCopyable_fn(int) -> std::enable_if_t<
        Readable<In> &&
        Writable<Out, iter_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyable =
    decltype(detail::IndirectlyCopyable_fn<In, Out>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectlyCopyableStorable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyCopyableStorable_fn(int) -> std::enable_if_t<
        IndirectlyCopyable<In, Out> &&
        Writable<Out, const iter_value_t<In>&> &&
        Copyable<iter_value_t<In>> &&
        constructible_from<iter_value_t<In>, iter_reference_t<In>> &&
        assignable_from<iter_value_t<In>&, iter_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyableStorable =
        decltype(detail::IndirectlyCopyableStorable_fn<In, Out>(0))::value;

NANO_END_NAMESPACE

#endif
