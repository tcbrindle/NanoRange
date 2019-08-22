// nanorange/detail/iterator/algorithm_requirements.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ALGORITHM_REQUIREMENTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ALGORITHM_REQUIREMENTS_HPP_INCLUDED

#include <nanorange/detail/functional/comparisons.hpp>
#include <nanorange/detail/functional/identity.hpp>
#include <nanorange/detail/iterator/indirect_callable_concepts.hpp>
#include <nanorange/detail/iterator/iter_swap.hpp>
#include <nanorange/detail/iterator/projected.hpp>

NANO_BEGIN_NAMESPACE


namespace detail {

struct indirectly_swappable_concept {
    template <typename I1, typename I2>
    auto requires_(I1& i1, I2& i2) -> decltype(
        ranges::iter_swap(i1, i1),
        ranges::iter_swap(i2, i2),
        ranges::iter_swap(i1, i2),
        ranges::iter_swap(i2, i1));
};

} // namespace detail

template <typename I1, typename I2 = I1>
NANO_CONCEPT indirectly_swappable =
    readable<I1> && readable<I2> &&
    detail::requires_<detail::indirectly_swappable_concept, I1, I2>;

// [alg.req.ind.cmp]

template <typename I1, typename I2, typename R,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT indirectly_comparable =
    indirect_relation<R, projected<I1, P1>, projected<I2, P2>>;

// [alg.req.permutable]

template <typename I>
NANO_CONCEPT permutable =
    forward_iterator<I> &&
    indirectly_movable_storable<I, I> &&
    indirectly_swappable<I, I>;

// [alg.req.mergeable]

template <typename I1, typename I2, typename Out, typename R = ranges::less,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT mergeable =
    input_iterator<I1> &&
    input_iterator<I2> &&
    weakly_incrementable<Out> &&
    indirectly_copyable<I1, Out> &&
    indirectly_copyable<I2, Out> &&
    indirect_strict_weak_order<R, projected<I1, P1>, projected<I2, P2>>;

// [alg.req.sortable]

template <typename I, typename R = ranges::less, typename P = identity>
NANO_CONCEPT sortable =
    permutable<I> &&
    indirect_strict_weak_order<R, projected<I, P>>;

NANO_END_NAMESPACE

#endif
