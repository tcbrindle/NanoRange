// nanorange/algorithm/partition.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr subrange<I> impl(I first, S last, Pred& pred, Proj& proj)
    {
        I it = nano::find_if_not(first, last, pred, proj);

        if (it == last) {
            return {std::move(it), std::move(nano::next(first, last))};
        }

        auto n = nano::next(it);

        while (n != last) {
            if (nano::invoke(pred, nano::invoke(proj, *n))) {
                nano::iter_swap(n, it);
                ++it;
            }
            ++n;
        }

        return {std::move(it), std::move(n)};
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_iterator<I> && sentinel_for<S, I> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>, subrange<I>>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(std::move(first), std::move(last),
                                  pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_range<Rng> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        borrowed_subrange_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(nano::begin(rng), nano::end(rng),
                                  pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::partition_fn, partition)

NANO_END_NAMESPACE

#endif
