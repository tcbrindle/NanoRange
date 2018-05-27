// nanorange/algorithm/stl/inplace_merge.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_INPLACE_MERGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_INPLACE_MERGE_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct inplace_merge_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalIterator<I> &&
        Cpp98Iterator<I> &&
        Sortable<I, Comp>>
    operator()(I first, I middle, I last, Comp comp = Comp{})
    {
        std::inplace_merge(std::move(first), std::move(middle),
                           std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, iterator_t<Rng> middle, Comp comp = Comp{})
    {
        std::inplace_merge(nano::begin(rng), std::move(middle),
                           nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::inplace_merge_fn, inplace_merge)

NANO_END_NAMESPACE

#endif
