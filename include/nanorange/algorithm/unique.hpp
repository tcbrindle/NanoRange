// nanorange/algorithm/unique.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <nanorange/algorithm/adjacent_find.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct unique_fn {
private:
    template <typename I, typename S, typename R, typename Proj>
    static constexpr I impl(I first, S last, R& comp, Proj& proj)
    {
        first = adjacent_find_fn::impl(std::move(first), last, comp, proj);

        if (first == last) {
            return first;
        }

        for (I n = next(first, 2, last); n != last; ++n) {
            if (!nano::invoke(comp, nano::invoke(proj, *first),
                              nano::invoke(proj, *n))) {
                *++first = iter_move(n);
            }
        }

        return ++first;
    }

public:
    template <typename I, typename S, typename R = equal_to<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectRelation<R, projected<I, Proj>> &&
        Permutable<I>, I>
    operator()(I first, S last, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(std::move(first), std::move(last),
                               comp, proj);
    }

    template <typename Rng, typename R = equal_to<>, typename Proj = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng> &&
            IndirectRelation<R, projected<iterator_t<Rng>, Proj>> &&
            Permutable<iterator_t<Rng>>,
            safe_iterator_t<Rng>>
    operator()(Rng&& rng, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(nano::begin(rng), nano::end(rng),
                               comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::unique_fn, unique)

NANO_END_NAMESPACE

#endif
