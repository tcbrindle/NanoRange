// nanorange/algorithm/partition.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if_not(std::move(first), last, pred, proj);

        if (first == last) {
            return first;
        }

        auto n = nano::next(first);

        while (n != last) {
            if (nano::invoke(pred, nano::invoke(proj, *n))) {
                nano::iter_swap(n, first);
                ++first;
            }
            ++n;
        }

        return first;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(std::move(first), std::move(last),
                                  pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(nano::begin(rng), nano::end(rng),
                                  pred, proj);
    }
};

}

NANO_DEFINE_CPO(detail::partition_fn, partition)

NANO_END_NAMESPACE

#endif
