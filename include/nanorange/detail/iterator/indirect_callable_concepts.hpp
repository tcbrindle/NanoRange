// nanorange/detail/iterator/indirect_callable_concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>

NANO_BEGIN_NAMESPACE

template <typename T>
using iter_common_reference_t = std::enable_if_t<readable<T>,
        common_reference_t<iter_reference_t<T>, iter_value_t<T>&>>;

// [iterator.concept.indirectinvocable]
namespace detail {

struct indirect_unary_invocable_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename F, typename I>
    static auto test(int) -> std::enable_if_t<
        readable<I> &&
        copy_constructible<F> &&
        invocable<F&, iter_value_t<I>&> &&
        invocable<F&, iter_reference_t<I>> &&
        invocable<F&, iter_common_reference_t<I>> &&
        common_reference_with<
            invoke_result_t<F&, iter_value_t<I>&>,
            invoke_result_t<F&, iter_reference_t<I>&>>,
        std::true_type>;
};

}

template <typename F, typename I>
NANO_CONCEPT indirect_unary_invocable =
        decltype(detail::indirect_unary_invocable_concept::test<F, I>(0))::value;

namespace detail {

struct indirect_regular_unary_invocable_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename F, typename I>
    static auto test(int) -> std::enable_if_t<
        readable<I> &&
        copy_constructible<F> &&
        regular_invocable<F&, iter_value_t<I>&> &&
        regular_invocable<F&, iter_reference_t<I>> &&
        regular_invocable<F&, iter_common_reference_t<I>> &&
        common_reference_with<
            invoke_result_t<F&, iter_value_t<I>&>,
            invoke_result_t<F&, iter_reference_t<I>&>>,
        std::true_type>;
};

}

template <typename F, typename I>
NANO_CONCEPT indirect_regular_unary_invocable =
        decltype(detail::indirect_regular_unary_invocable_concept::test<F, I>(0))::value;

namespace detail {

struct indirect_unary_predicate_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename F, typename I>
    static auto test(int) -> std::enable_if_t<
        readable<I> &&
        copy_constructible<F> &&
        predicate<F&, iter_value_t<I>&> &&
        predicate<F&, iter_reference_t<I>> &&
        predicate<F&, iter_common_reference_t<I>>,
        std::true_type>;
};

}

template <typename F, typename I>
NANO_CONCEPT indirect_unary_predicate =
        decltype(detail::indirect_unary_predicate_concept::test<F, I>(0))::value;

namespace detail {

struct indirect_relation_concept {
    template <typename F, typename I1, typename I2>
    static auto test(long) -> std::false_type;

    template <typename F, typename I1, typename I2>
    static auto test(int) -> std::enable_if_t<
        readable<I1> && readable<I2> &&
        copy_constructible<F> &&
        relation<F&, iter_value_t<I1>&, iter_value_t<I2>&> &&
        relation<F&, iter_value_t<I1>&, iter_reference_t<I2>> &&
        relation<F&, iter_reference_t<I1>, iter_value_t<I2>&> &&
        relation<F&, iter_reference_t<I1>, iter_reference_t<I2>> &&
        relation<F&, iter_common_reference_t<I1>, iter_common_reference_t<I2>>,
        std::true_type>;
};

}

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT indirect_relation =
    decltype(detail::indirect_relation_concept::test<F, I1, I2>(0))::value;

namespace detail {

struct indirect_strict_weak_order_concept {
    template <typename, typename, typename>
    static auto test(long) -> std::false_type;

    template <typename F, typename I1, typename I2>
    static auto test(int) -> std::enable_if_t<
        readable<I1> && readable<I2> &&
        copy_constructible<F> &&
        strict_weak_order<F&, iter_value_t<I1>&, iter_value_t<I2>&> &&
        strict_weak_order<F&, iter_value_t<I1>&, iter_reference_t<I2>> &&
        strict_weak_order<F&, iter_reference_t<I1>, iter_value_t<I2>&> &&
        strict_weak_order<F&, iter_reference_t<I1>, iter_reference_t<I2>> &&
        strict_weak_order<F&, iter_common_reference_t<I1>, iter_common_reference_t<I2>>,
        std::true_type>;
};

}

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT indirect_strict_weak_order =
    decltype(detail::indirect_strict_weak_order_concept::test<F, I1, I2>(0))::value;

template <typename F, typename... Is>
using indirect_result_t = std::enable_if_t<
    (readable<Is> && ... ) && invocable<F, iter_reference_t<Is>...>,
    invoke_result_t<F, iter_reference_t<Is>...>>;

// [alg.req.ind.move]

namespace detail {

struct indirectly_movable_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename In, typename Out>
    static auto test(int) -> std::enable_if_t<
        readable<In> &&
        writable<Out, iter_rvalue_reference_t<In>>,
        std::true_type>;
};

}

template <typename In, typename Out>
NANO_CONCEPT indirectly_movable =
    decltype(detail::indirectly_movable_concept::test<In, Out>(0))::value;

namespace detail {

struct indirectly_movable_storable_concept {
    template <typename In, typename Out>
    static auto test(long) -> std::false_type;

    template <typename In, typename Out>
    static auto test(int) -> std::enable_if_t<
        indirectly_movable<In, Out> &&
        writable<Out, iter_value_t<In>> &&
        movable<iter_value_t<In>> &&
        constructible_from<iter_value_t<In>, iter_rvalue_reference_t<In>> &&
        assignable_from<iter_value_t<In>&, iter_rvalue_reference_t<In>>,
        std::true_type>;
};

}

template <typename In, typename Out>
NANO_CONCEPT indirectly_movable_storable =
    decltype(detail::indirectly_movable_storable_concept::test<In, Out>(0))::value;

// [alg.req.ind.copy]
namespace detail {

struct indirectly_copyable_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename In, typename Out>
    static auto test(int) -> std::enable_if_t<
        readable<In> &&
        writable<Out, iter_reference_t<In>>,
        std::true_type>;
};

}

template <typename In, typename Out>
NANO_CONCEPT indirectly_copyable =
    decltype(detail::indirectly_copyable_concept::test<In, Out>(0))::value;

namespace detail {

struct indirectly_copyable_storable_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename In, typename Out>
    static auto test(int) -> std::enable_if_t<
        indirectly_copyable<In, Out> &&
        writable<Out, const iter_value_t<In>&> &&
        copyable<iter_value_t<In>> &&
        constructible_from<iter_value_t<In>, iter_reference_t<In>> &&
        assignable_from<iter_value_t<In>&, iter_reference_t<In>>,
        std::true_type>;
};

}

template <typename In, typename Out>
NANO_CONCEPT indirectly_copyable_storable =
    decltype(detail::indirectly_copyable_storable_concept::test<In, Out>(0))::value;

NANO_END_NAMESPACE

#endif
