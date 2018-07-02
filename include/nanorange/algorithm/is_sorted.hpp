// nanorange/algorithm/is_sorted.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_SORTED_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_SORTED_HPP_INCLUDED

#include <nanorange/algorithm/is_sorted_until.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_sorted_fn {
    template <typename I, typename S, typename Comp = less<>,
            typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>, bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(std::move(first), last,
                                        comp, proj) == last;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(nano::begin(rng), nano::end(rng),
                                        comp, proj) == nano::end(rng);
    }
};

}

NANO_DEFINE_CPO(detail::is_sorted_fn, is_sorted)

NANO_END_NAMESPACE

#endif
