// nanorange/algorithm/stl/sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_SORT_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct sort_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessIterator<I> &&
        detail::Cpp98Iterator<I> &&
        Sortable<I, Comp>>
    operator()(I first, I last, Comp comp = Comp{})
    {
        std::sort(std::move(first), std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
        CommonRange<Rng> &&
        detail::Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, Comp comp = Comp{})
    {
        std::sort(nano::begin(rng), nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::sort_fn, sort)

NANO_END_NAMESPACE

#endif
