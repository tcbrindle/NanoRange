// nanorange/algorithm/pop_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_POP_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_POP_HEAP_HPP_INCLUDED

#include <nanorange/detail/algorithm/heap_sift.hpp>
#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct pop_heap_fn {
private:
    friend struct sort_heap_fn;

    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n > 1) {
            nano::iter_swap(first, first + (n - 1));
            detail::sift_down_n(first, n - 1, first, comp, proj);
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = ranges::less,
              typename Proj = identity>
    constexpr std::enable_if_t<random_access_iterator<I> && sentinel_for<S, I> &&
                                   sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return pop_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<random_access_range<Rng> &&
                                   sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return pop_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                 proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::pop_heap_fn, pop_heap)

NANO_END_NAMESPACE

#endif
