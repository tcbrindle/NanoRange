// nanorange/algorithm/none_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_NONE_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_NONE_OF_HPP_INCLUDED

#include <nanorange/algorithm/any_of.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.alg.none_of]

namespace detail {

struct none_of_fn {

    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(first, last, pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(nano::begin(rng), nano::end(rng),
                                pred, proj);
    }
};

} // namespace detail

NANO_DEFINE_CPO(detail::none_of_fn, none_of)

NANO_END_NAMESPACE

#endif