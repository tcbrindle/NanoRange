// nanorange/algorithm/prev_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Taken from Range-V3
//
// Copyright Eric Niebler 2014-2018
//
//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_PREV_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PREV_PERMUTATION_HPP_INCLUDED

#include <nanorange/algorithm/reverse.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct prev_permutation_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr bool impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return false;
        }

        const I last_it = nano::next(first, last);
        I i = last_it;

        if (first == --i) {
            return false;
        }

        while (true) {
            I ip1 = i;

            if (nano::invoke(comp, nano::invoke(proj, *ip1),
                             nano::invoke(proj, *--i))) {
                I j = last_it;

                while (!nano::invoke(comp, nano::invoke(proj, *--j),
                                     nano::invoke(proj, *i)));

                nano::iter_swap(i, j);
                nano::reverse(ip1, last_it);
                return true;
            }

            if (i == first) {
                nano::reverse(first, last_it);
                return false;
            }
        }
    }


public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return prev_permutation_fn::impl(std::move(first), std::move(last),
                                         comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>, bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return prev_permutation_fn::impl(nano::begin(rng), nano::end(rng),
                                         comp, proj);
    }
};

}

NANO_DEFINE_CPO(detail::prev_permutation_fn, prev_permutation)

NANO_END_NAMESPACE

#endif
