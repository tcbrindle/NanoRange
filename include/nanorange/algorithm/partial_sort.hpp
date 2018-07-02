// nanorange/algorithm/partial_sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Copyright Eric Niebler 2014
// Copyright Casey Carter 2015
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)



#ifndef NANORANGE_ALGORITHM_PARTIAL_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTIAL_SORT_HPP_INCLUDED

#include <nanorange/algorithm/make_heap.hpp>
#include <nanorange/algorithm/sort_heap.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct partial_sort_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, I middle, S last, Comp& comp, Proj& proj)
    {
        nano::make_heap(first, middle, comp, proj);
        const auto len = nano::distance(first, middle);
        I i = middle;

        while (i != last) {
            if (nano::invoke(comp, nano::invoke(proj, *i), nano::invoke(proj, *first))) {
                nano::iter_swap(i, first);
                detail::sift_down_n(first, len, first, comp, proj);
            }
            ++i;
        }
        nano::sort_heap(first, middle, comp, proj);
        return i;
    }

public:
    template <typename I, typename S, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, I>
    operator()(I first, I middle, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return partial_sort_fn::impl(std::move(first), std::move(middle),
                                     std::move(last), comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>, safe_iterator_t<Rng>>
    operator()(Rng&& rng, iterator_t<Rng> middle, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return partial_sort_fn::impl(nano::begin(rng), std::move(middle),
                                     nano::end(rng), comp, proj);
    }
};

}

NANO_DEFINE_CPO(detail::partial_sort_fn, partial_sort)

NANO_END_NAMESPACE

#endif
