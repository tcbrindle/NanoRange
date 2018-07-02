// nanorange/algorithm/sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SORT_HPP_INCLUDED

#include <nanorange/detail/algorithm/pdqsort.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct sort_fn {
    template <typename I, typename S, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        I last_it = nano::next(first, last);
        detail::pdqsort(std::move(first), last_it, comp, proj);
        return last_it;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        iterator_t<Rng> last_it = nano::next(nano::begin(rng), nano::end(rng));
        detail::pdqsort(nano::begin(rng), last_it, comp, proj);
        return last_it;
    }
};

}

NANO_DEFINE_CPO(detail::sort_fn, sort)

NANO_END_NAMESPACE

#endif
