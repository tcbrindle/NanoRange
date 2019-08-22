// nanorange/algorithm/push_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PUSH_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PUSH_HEAP_HPP_INCLUDED

#include <nanorange/detail/algorithm/heap_sift.hpp>
#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct push_heap_fn {
    template <typename I, typename S, typename Comp = ranges::less,
              typename Proj = identity>
    constexpr std::enable_if_t<random_access_iterator<I> && sentinel_for<S, I> &&
                                   sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        detail::sift_up_n(first, n, comp, proj);
        return first + n;
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<random_access_range<Rng> &&
                                   sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(rng);
        detail::sift_up_n(nano::begin(rng), n, comp, proj);
        return nano::begin(rng) + n;
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::push_heap_fn, push_heap)

NANO_END_NAMESPACE

#endif
