// nanorange/algorithm/equal_range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_EQUAL_RANGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_EQUAL_RANGE_HPP_INCLUDED

#include <nanorange/algorithm/lower_bound.hpp>
#include <nanorange/algorithm/upper_bound.hpp>
#include <nanorange/view/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct equal_range_fn {
private:
    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr subrange<I> impl(I first, S last, const T& value,
                                      Comp& comp, Proj& proj)
    {
        return {lower_bound_fn::impl(first, last, value, comp, proj),
                upper_bound_fn::impl(first, last, value, comp, proj)};
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    subrange<I>>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return equal_range_fn::impl(std::move(first), std::move(last),
                                    value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    safe_subrange_t<Rng>>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return equal_range_fn::impl(nano::begin(rng), nano::end(rng),
                                    value, comp, proj);
    }
};

}

NANO_DEFINE_CPO(detail::equal_range_fn, equal_range)

NANO_END_NAMESPACE

#endif
