// nanorange/algorithm/stl/partition_point.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_PARTITION_POINT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_PARTITION_POINT_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_point_fn {
    template <typename I, typename Pred>
    std::enable_if_t<
        ForwardIterator<I> &&
        Cpp98Iterator<I> &&
        IndirectUnaryPredicate<Pred, I>, I>
    operator()(I first, I last, Pred pred) const
    {
        return std::partition_point(std::move(first), std::move(last),
                                    std::ref(pred));
    }

    template <typename Rng, typename Pred>
    std::enable_if_t<
        ForwardRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
        IndirectUnaryPredicate<Pred, iterator_t<Rng>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred) const
    {
        return std::partition_point(nano::begin(rng), nano::end(rng),
                                    std::ref(pred));
    }
};

}

NANO_INLINE_VAR(detail::partition_point_fn, partition_point)

NANO_END_NAMESPACE

#endif
