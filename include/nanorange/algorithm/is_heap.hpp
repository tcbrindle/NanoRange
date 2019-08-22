// nanorange/algorithm/is_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_HEAP_HPP_INCLUDED

#include <nanorange/algorithm/is_heap_until.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_heap_fn {
    template <typename I, typename S, typename Comp = ranges::less,
              typename Proj = identity>
    std::enable_if_t<random_access_iterator<I> && sentinel_for<S, I> &&
                         indirect_strict_weak_order<Comp, projected<I, Proj>>,
                     bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return is_heap_until_fn::impl(std::move(first), n, comp, proj) == last;
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<
        random_access_range<Rng> &&
            indirect_strict_weak_order<Comp, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_heap_until_fn::impl(nano::begin(rng), nano::distance(rng),
                                      comp, proj) == nano::end(rng);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_heap_fn, is_heap)

NANO_END_NAMESPACE

#endif
