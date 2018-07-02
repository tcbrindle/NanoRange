// nanorange/algorithm/sort_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SORT_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SORT_HEAP_HPP_INCLUDED

#include <nanorange/algorithm/pop_heap.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct sort_heap_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n < 2) {
            return first + n;
        }

        for (auto i = n; i > 1; --i) {
            pop_heap_fn::impl(first, i, comp, proj);
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> && Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return sort_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<RandomAccessRange<Rng> &&
                                   Sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return sort_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                  proj);
    }
};

} // namespace detail

NANO_DEFINE_CPO(detail::sort_heap_fn, sort_heap)

NANO_END_NAMESPACE

#endif
