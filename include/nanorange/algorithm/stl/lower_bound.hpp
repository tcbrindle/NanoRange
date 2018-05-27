// nanorange/algorithm/stl/lower_bound.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_LOWER_BOUND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_LOWER_BOUND_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct lower_bound_fn {
    template <typename I, typename T, typename Comp = less<>>
    std::enable_if_t<
        ForwardIterator<I> &&
        detail::Cpp98Iterator<I> &&
        IndirectStrictWeakOrder<Comp, const T*, I>,
        I>
    operator()(I first, I last, const T& value, Comp comp = Comp{})
    {
        return std::lower_bound(std::move(first), std::move(last),
                                value, std::ref(comp));
    }

    template <typename Rng, typename T, typename Comp = less<>>
    std::enable_if_t<
        ForwardRange<Rng> &&
        CommonRange<Rng> &&
        detail::Cpp98Iterator<iterator_t<Rng>> &&
        IndirectStrictWeakOrder<Comp, const T*, iterator_t<Rng>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& value, Comp comp = Comp{})
    {
        return std::lower_bound(nano::begin(rng), nano::end(rng),
                                value, std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::lower_bound_fn, lower_bound)

NANO_END_NAMESPACE

#endif
