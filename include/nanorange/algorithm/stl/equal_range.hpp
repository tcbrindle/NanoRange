// nanorange/algorithm/stl/equal_range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_EQUAL_RANGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_EQUAL_RANGE_HPP_INCLUDED

#include <nanorange/range.hpp>
#include <nanorange/view/subrange.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct equal_range_fn {
    template <typename I, typename T, typename Comp = less<>>
    std::enable_if_t<
        ForwardIterator<I> &&
        detail::Cpp98Iterator<I> &&
        IndirectStrictWeakOrder<Comp, const T*, I>,
    subrange<I>>
    operator()(I first, I last, const T& value, Comp comp = Comp{}) const
    {
        auto pair =  std::equal_range(std::move(first), std::move(last),
                                value, std::ref(comp));

        // FIXME: Subrange "PairLike" constructor
        return {std::move(pair.first), std::move(pair.second)};
    }

    template <typename Rng, typename T, typename Comp = less<>>
    std::enable_if_t<
            ForwardRange<Rng> &&
            CommonRange<Rng> &&
    detail::Cpp98Iterator<iterator_t<Rng>> &&
    IndirectStrictWeakOrder<Comp, const T*, iterator_t<Rng>>,
    safe_subrange_t<Rng>>
    operator()(Rng&& rng, const T& value, Comp comp = Comp{}) const
    {
        auto pair =  std::equal_range(nano::begin(rng), nano::end(rng),
                                value, std::ref(comp));
        // FIXME: Subrange's PairLike constructor is broken
        return {std::move(pair.first), std::move(pair.second)};
    }
};

}

NANO_INLINE_VAR(detail::equal_range_fn, equal_range)

NANO_END_NAMESPACE

#endif
