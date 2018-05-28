// nanorange/algorithm/stl/is_heap_until.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_IS_HEAP_UNTIL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_IS_HEAP_UNTIL_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_heap_until_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessIterator<I> &&
        Cpp98Iterator<I> &&
        IndirectStrictWeakOrder<Comp, I>, I>
    operator()(I first, I last, Comp comp = Comp{}) const
    {
        std::is_heap_until(std::move(first), std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
        IndirectStrictWeakOrder<Comp, iterator_t<Rng>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}) const
    {
        std::is_heap_until(nano::begin(rng), nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::is_heap_until_fn, is_heap_until)

NANO_END_NAMESPACE

#endif
