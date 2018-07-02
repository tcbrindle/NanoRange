// nanorange/algorithm/stl/stable_partition.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_STABLE_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_STABLE_PARTITION_HPP_INCLUDED

#include <nanorange/ranges.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_partition_fn {
    template <typename I, typename Pred>
    std::enable_if_t<
        BidirectionalIterator<I> &&
        Cpp98Iterator<I> &&
        IndirectUnaryPredicate<Pred, I>, I>
    operator()(I first, I last, Pred pred) const
    {
        return std::stable_partition(std::move(first), std::move(last),
                                     std::ref(pred));
    }

    template <typename Rng, typename Pred>
    std::enable_if_t<
        BidirectionalRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
         IndirectUnaryPredicate<Pred, iterator_t<Rng>>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred) const
    {
        return std::stable_partition(nano::begin(rng), nano::end(rng),
                                     std::ref(pred));
    }
};

}

NANO_DEFINE_CPO(detail::stable_partition_fn, stable_partition)

NANO_END_NAMESPACE

#endif
