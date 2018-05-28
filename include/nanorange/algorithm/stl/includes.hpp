// nanorange/algorithm/stl/includes.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_INCLUDES_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_INCLUDES_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct includes_fn {
    template <typename I1, typename I2, typename Comp>
    std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<I1, I1> &&
        Cpp98Iterator<I1> &&
        InputIterator<I2> &&
        Sentinel<I2, I2> &&
        Cpp98Iterator<I2> &&
        IndirectStrictWeakOrder<Comp, I1, I2>, bool>
    operator()(I1 first1, I1 last1, I2 first2, I2 last2, Comp comp = Comp{})
    {
        return std::includes(std::move(first1), std::move(last1),
                             std::move(first2), std::move(last2),
                             std::ref(comp));
    }

    template <typename Rng1, typename Rng2, typename Comp>
    std::enable_if_t<
        InputRange<Rng1> &&
        CommonRange<Rng1> &&
        Cpp98Iterator<iterator_t<Rng1>> &&
        InputRange<Rng2> &&
        CommonRange<Rng2> &&
        Cpp98Iterator<iterator_t<Rng2>> &&
        IndirectStrictWeakOrder<Comp, iterator_t<Rng1>, iterator_t<Rng2>>, bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Comp comp = Comp{})
    {
        return std::includes(nano::begin(rng1), nano::end(rng1),
                             nano::begin(rng2), nano::end(rng2),
                             std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::includes_fn, includes)

NANO_END_NAMESPACE

#endif
