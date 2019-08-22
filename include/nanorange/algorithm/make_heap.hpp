// nanorange/algorithm/make_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MAKE_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MAKE_HEAP_HPP_INCLUDED

#include <nanorange/detail/algorithm/heap_sift.hpp>
#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct make_heap_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n > 1) {
            // start from the first parent, there is no need to consider
            // children
            for (auto start = (n - 2) / 2; start >= 0; --start) {
                detail::sift_down_n(first, n, first + start, comp, proj);
            }
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
        return make_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<random_access_range<Rng> &&
                                   sortable<iterator_t<Rng>, Comp>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return make_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                  proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::make_heap_fn, make_heap)

NANO_END_NAMESPACE

#endif
