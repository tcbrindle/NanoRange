// nanorange/algorithm/stable_sort.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//

//
//  Copyright (c) 1994
//  Hewlett-Packard Company
//
//  Permission to use, copy, modify, distribute and sell this software
//  and its documentation for any purpose is hereby granted without fee,
//  provided that the above copyright notice appear in all copies and
//  that both that copyright notice and this permission notice appear
//  in supporting documentation.  Hewlett-Packard Company makes no
//  representations about the suitability of this software for any
//  purpose.  It is provided "as is" without express or implied warranty.
//
//  Copyright (c) 1996
//  Silicon Graphics Computer Systems, Inc.
//
//  Permission to use, copy, modify, distribute and sell this software
//  and its documentation for any purpose is hereby granted without fee,
//  provided that the above copyright notice appear in all copies and
//  that both that copyright notice and this permission notice appear
//  in supporting documentation.  Silicon Graphics makes no
//  representations about the suitability of this software for any
//  purpose.  It is provided "as is" without express or implied warranty.


#ifndef NANORANGE_ALGORITHM_STABLE_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STABLE_SORT_HPP_INCLUDED

#include <nanorange/algorithm/inplace_merge.hpp>
#include <nanorange/detail/algorithm/pdqsort.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_sort_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static void inplace_stable_sort(I first, I last, Comp& comp, Proj& proj)
    {
        if (last - first < 15) {
            detail::insertion_sort(std::move(first), std::move(last), comp, proj);
        } else {
            I middle = first + iter_difference_t<I>(last - first) / 2;
            inplace_stable_sort(first, middle, comp, proj);
            inplace_stable_sort(middle, last, comp, proj);
            inplace_merge_fn::impl_slow(first, middle, last,
                                        middle - first, last - middle,
                                        comp, proj);
        }
    }

    template <typename I, typename Comp, typename Proj>
    static void impl(I first, I last, Comp& comp, Proj& proj)
    {
        inplace_stable_sort(std::move(first), std::move(last), comp, proj);
    }

public:
    template <typename I, typename S, typename Comp = less<>, typename Proj = identity>
    std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto ilast = nano::next(first, last);
        impl(std::move(first), ilast, comp, proj);
        return ilast;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        auto first = nano::begin(rng);
        const auto last = nano::next(first, nano::end(rng));
        impl(std::move(first), last, comp, proj);
        return last;
    }
};

}

NANO_INLINE_VAR(detail::stable_sort_fn, stable_sort)

NANO_END_NAMESPACE

#endif
