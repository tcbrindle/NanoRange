// nanorange/algorithm/binary_search.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_BINARY_SEARCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_BINARY_SEARCH_HPP_INCLUDED

#include <nanorange/algorithm/lower_bound.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct binary_search_fn {
private:
    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr bool impl(I first, S last, const T& value, Comp& comp,
                               Proj& proj)
    {
        first = lower_bound_fn::impl(std::move(first), last, value, comp, proj);
        return (first != last && !nano::invoke(comp, value, nano::invoke(proj, *first)));
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
       ForwardIterator<I> &&
       Sentinel<S, I> &&
       IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    bool>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return binary_search_fn::impl(std::move(first), std::move(last),
                                      value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    bool>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return binary_search_fn::impl(nano::begin(rng), nano::end(rng),
                                      value, comp, proj);
    }
};

}

NANO_DEFINE_CPO(detail::binary_search_fn, binary_search)

NANO_END_NAMESPACE

#endif
