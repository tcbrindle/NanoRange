// nanorange/algorithm/stl/next_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_NEXT_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_NEXT_PERMUTATION_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct next_permutation_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalIterator<I> &&
        Cpp98Iterator<I> &&
        Sortable<I, Comp>, bool>
    operator()(I first, I last, Comp comp = Comp{})
    {
        return std::next_permutation(std::move(first), std::move(last),
                                     std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>, bool>
    operator()(Rng&& rng, Comp comp = Comp{})
    {
        return std::next_permutation(nano::begin(rng), nano::end(rng),
                                     std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::next_permutation_fn, next_permutation)

NANO_END_NAMESPACE

#endif
