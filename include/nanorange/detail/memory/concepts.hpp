// nanorange/detail/memory/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/range/concepts.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename>
auto NoThrowInputIterator_fn(long) -> std::false_type;

template <typename I>
auto NoThrowInputIterator_fn(int) -> std::enable_if_t<
    InputIterator<I> &&
    std::is_lvalue_reference<iter_reference_t<I>>::value &&
    Same<remove_cvref_t<iter_reference_t<I>>, iter_value_t<I>>,
    std::true_type>;

template <typename I>
NANO_CONCEPT NoThrowInputIterator =
    decltype(NoThrowInputIterator_fn<I>(0))::value;

template <typename S, typename I>
NANO_CONCEPT NoThrowSentinel = Sentinel<S, I>;

template <typename>
auto NoThrowInputRange_fn(long) -> std::false_type;

template <typename Rng>
auto NoThrowInputRange_fn(int) -> std::enable_if_t<
    Range<Rng> &&
    NoThrowInputIterator<iterator_t<Rng>> &&
    NoThrowSentinel<sentinel_t<Rng>, iterator_t<Rng>>,
    std::true_type>;

template <typename Rng>
NANO_CONCEPT NoThrowInputRange =
    decltype(NoThrowInputRange_fn<Rng>(0))::value;

template <typename I>
NANO_CONCEPT NoThrowForwardIterator =
    NoThrowInputIterator<I> &&
    ForwardIterator<I> &&
    NoThrowSentinel<I, I>;

template <typename>
auto NoThrowForwardRange_fn(long) -> std::false_type;

template <typename Rng>
auto NoThrowForwardRange_fn(int) -> std::enable_if_t<
    NoThrowInputRange<Rng> &&
    NoThrowForwardIterator<iterator_t<Rng>>,
    std::true_type>;

template <typename Rng>
NANO_CONCEPT NoThrowForwardRange =
    decltype(NoThrowForwardRange_fn<Rng>(0))::value;

}

NANO_END_NAMESPACE

#endif
