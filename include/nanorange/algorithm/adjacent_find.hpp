// nanorange/algorithm/adjacent_find.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ADJACENT_FIND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ADJACENT_FIND_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

// [range.alg.adjacent.find]

namespace detail {

struct adjacent_find_fn {
private:
    friend struct unique_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I next = first;
        ++next;

        while (next != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first),
                             nano::invoke(proj, *next))) {
                return first;
            }
            ++first;
            ++next;
        }

        return next;
    }

public:
    template <typename I, typename S, typename Proj = identity,
              typename Pred = ranges::equal_to>
    constexpr std::enable_if_t<forward_iterator<I> && sentinel_for<S, I> &&
                                   indirect_relation<Pred, projected<I, Proj>>,
                               I>
    operator()(I first, S last, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return adjacent_find_fn::impl(std::move(first), std::move(last),
                                      pred, proj);
    }

    template <typename Rng, typename Proj = identity,
              typename Pred = ranges::equal_to>
    constexpr std::enable_if_t<
        forward_range<Rng> &&
            indirect_relation<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return adjacent_find_fn::impl(nano::begin(rng), nano::end(rng),
                                      pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::adjacent_find_fn, adjacent_find)

NANO_END_NAMESPACE

#endif
