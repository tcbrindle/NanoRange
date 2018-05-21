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

namespace detail {

template <typename, typename = void>
struct iter_common_ref_helper {
};

template <typename T>
struct iter_common_ref_helper<T, std::enable_if_t<Readable<T>>> {
    using type = common_reference_t<reference_t<T>, value_type_t<T>&>;
};

} // namespace detail

template <typename I>
using iter_common_reference_t =
    typename detail::iter_common_ref_helper<I>::type;

namespace detail {

template <typename I>
using checked_iter_common_ref_t = test_t<iter_common_reference_t, I>;
}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryInvocable = Readable<I>&& CopyConstructible<F>&&
    Invocable<detail::lref_t<F>, detail::checked_value_type_t<I>&>&&
        Invocable<detail::lref_t<F>, detail::checked_reference_t<I>&>&&
            Invocable<detail::lref_t<F>, detail::checked_iter_common_ref_t<I>>&&
                CommonReference<
                    detail::checked_invoke_result_t<
                        detail::lref_t<F>, detail::checked_value_type_t<I>&>,
                    detail::checked_invoke_result_t<
                        detail::lref_t<F>, detail::checked_reference_t<I>>>;

template <typename F, typename I>
NANO_CONCEPT IndirectRegularUnaryInvocable = Readable<I>&& CopyConstructible<
    F>&& RegularInvocable<detail::lref_t<F>, detail::checked_value_type_t<I>&>&&
    RegularInvocable<detail::lref_t<F>, detail::checked_reference_t<I>&>&&
        RegularInvocable<detail::lref_t<F>,
                         detail::checked_iter_common_ref_t<I>>&&
            CommonReference<
                detail::checked_invoke_result_t<
                    detail::lref_t<F>, detail::checked_value_type_t<I>&>,
                detail::checked_invoke_result_t<
                    detail::lref_t<F>, detail::checked_reference_t<I>>>;

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryPredicate = Readable<I>&& CopyConstructible<F>&&
    Predicate<detail::lref_t<F>, detail::checked_value_type_t<I>&>&&
        Predicate<detail::lref_t<F>, detail::checked_reference_t<I>>&&
            Predicate<detail::lref_t<F>, detail::checked_iter_common_ref_t<I>>;

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectRelation =
    Readable<I1>&& Readable<I2>&& CopyConstructible<F>&&
        Relation<detail::lref_t<F>, detail::checked_value_type_t<I1>&,
                 detail::checked_value_type_t<I2>&>&&
            Relation<detail::lref_t<F>, detail::checked_value_type_t<I1>&,
                     detail::checked_reference_t<I2>>&&
                Relation<detail::lref_t<F>, detail::checked_reference_t<I1>,
                         detail::checked_value_type_t<I2>&>&&
                    Relation<detail::lref_t<F>, detail::checked_reference_t<I1>,
                             detail::checked_reference_t<I2>>&&
                        Relation<detail::lref_t<F>,
                                 detail::checked_iter_common_ref_t<I1>,
                                 detail::checked_iter_common_ref_t<I2>>;

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectStrictWeakOrder =
    Readable<I1>&& Readable<I2>&& CopyConstructible<F>&& StrictWeakOrder<
        detail::lref_t<F>, detail::checked_value_type_t<I1>&,
        detail::checked_value_type_t<I2>&>&&
        StrictWeakOrder<detail::lref_t<F>, detail::checked_value_type_t<I1>&,
                        detail::checked_reference_t<I2>>&&
            StrictWeakOrder<detail::lref_t<F>, detail::checked_reference_t<I1>,
                            detail::checked_value_type_t<I2>&>&&
                StrictWeakOrder<detail::lref_t<F>,
                                detail::checked_reference_t<I1>,
                                detail::checked_reference_t<I2>>&&
                    StrictWeakOrder<detail::lref_t<F>,
                                    detail::checked_iter_common_ref_t<I1>,
                                    detail::checked_iter_common_ref_t<I2>>;

template <typename, typename...>
struct indirect_result;

namespace detail {

template <typename Void, typename, typename...>
struct indirect_result_helper {
};

template <bool...>
struct all_readable_helper;

template <>
struct all_readable_helper<> : std::true_type {
};

template <bool First, bool... Rest>
struct all_readable_helper<First, Rest...>
    : std::conditional_t<First, all_readable_helper<Rest...>, std::false_type> {
};

template <typename... Is>
constexpr bool all_readable = all_readable_helper<Readable<Is>...>::value;

template <typename F, typename... Is>
struct indirect_result_helper<
    std::enable_if_t<all_readable<Is...> && Invocable<F, reference_t<Is>...>>,
    F, Is...> : invoke_result<F, reference_t<Is>...> {
    using type = invoke_result_t<F, reference_t<Is>...>;
};

} // namespace detail

template <typename F, typename... Is>
struct indirect_result : detail::indirect_result_helper<void, F, Is...> {
};

template <typename F, typename... Is>
using indirect_result_t = typename indirect_result<F, Is...>::type;

namespace detail {

template <typename F, typename... Is>
using checked_indirect_result_t = test_t<indirect_result_t, F, Is...>;
}

// range.commonalgoreq.indirectlymovable]

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovable =
    Readable<In>&& Writable<Out, detail::checked_rvalue_ref_t<In>>;

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovableStorable = IndirectlyMovable<In, Out>&&
    Writable<Out, detail::checked_value_type_t<In>>&&
        Movable<detail::checked_value_type_t<In>>&&
            Constructible<detail::checked_value_type_t<In>,
                          detail::checked_rvalue_ref_t<In>>&&
                Assignable<detail::checked_value_type_t<In>&,
                           detail::checked_rvalue_ref_t<In>>;

// range.commonalgoreq.indirectlycopyable

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyable =
    Readable<In>&& Writable<Out, detail::checked_reference_t<Out>>;

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyableStorable = IndirectlyCopyable<In, Out>&&
    Writable<Out, const detail::checked_value_type_t<In>&>&&
        Copyable<detail::checked_value_type_t<In>>&&
            Constructible<detail::checked_value_type_t<In>,
                          detail::checked_rvalue_ref_t<In>>&&
                Assignable<detail::checked_value_type_t<In>&,
                           detail::checked_reference_t<In>>;

NANO_END_NAMESPACE

#endif
