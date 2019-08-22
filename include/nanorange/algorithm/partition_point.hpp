// nanorange/algorithm/partition_point.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2

//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015


//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//

#ifndef NANORANGE_ALGORITHM_PARTITION_POINT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_POINT_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_point_fn {
private:
    friend struct lower_bound_fn;
    friend struct upper_bound_fn;

    template <typename I, typename Pred, typename Proj>
    static constexpr I impl_n(I first, iter_difference_t<I> n, Pred& pred,
                              Proj& proj)
    {
        while (n != 0) {
            const auto half = n/2;

            auto middle = nano::next(first, half);

            if (nano::invoke(pred, nano::invoke(proj, *middle))) {
                first = std::move(++middle);
                n -= half + 1;
            } else {
                n = half;
            }
        }

        return first;
    }

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr std::enable_if_t<sized_sentinel_for<S, I>, I>
    impl(I first, S last, Pred& pred, Proj& proj)
    {
        const auto n = nano::distance(first, std::move(last));
        return partition_point_fn::impl_n(std::move(first), n, pred, proj);
    }

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr std::enable_if_t<!sized_sentinel_for<S, I>, I>
    impl(I first, S last, Pred& pred, Proj& proj)
    {
        // Probe exponentially for either end-of-range or an iterator
        // that is past the partition point (i.e., does not satisfy pred).
        iter_difference_t<I> n{1};

        while (true) {
            auto m = first;
            auto d = nano::advance(m, n, last);
            if (m == last || !nano::invoke(pred, nano::invoke(proj, *m))) {
                n -= d;
                return partition_point_fn::impl_n(std::move(first), n,
                                                  pred, proj);
            }
            first = std::move(m);
            n *= 2;
        }
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    std::enable_if_t<forward_iterator<I> && sentinel_for<S, I> &&
                         indirect_unary_predicate<Pred, projected<I, Proj>>, I>
    constexpr operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return partition_point_fn::impl(std::move(first), std::move(last),
                                        pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    std::enable_if_t<
        forward_range<Rng> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return partition_point_fn::impl(nano::begin(rng), nano::end(rng),
                                        pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::partition_point_fn, partition_point)

NANO_END_NAMESPACE

#endif
