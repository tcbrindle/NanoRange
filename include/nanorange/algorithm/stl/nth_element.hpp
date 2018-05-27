// nanorange/algorithm/stl/nth_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_NTH_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_NTH_ELEMENT_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct nth_element_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessIterator<I> &&
        detail::Cpp98Iterator<I> &&
        Sortable<I, Comp>>
    operator()(I first, I nth, I last, Comp comp = Comp{})
    {
        std::nth_element(std::move(first), std::move(nth),
                         std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
        CommonRange<Rng> &&
        detail::Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, iterator_t<Rng> nth, Comp comp = Comp{})
    {
        std::nth_element(nano::begin(rng), std::move(nth),
                         nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::nth_element_fn, nth_element)

NANO_END_NAMESPACE

#endif
