// nanorange/algorithm/stl/merge.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_MERGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_MERGE_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct merge_fn {
    template <typename I1, typename I2, typename O, typename Comp = less<>>
    std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<I1, I1> &&
        detail::Cpp98Iterator<I1> &&
        InputIterator<I2> &&
        Sentinel<I1, I1> &&
        detail::Cpp98Iterator<I2> &&
        WeaklyIncrementable<O> &&
        detail::Cpp98Iterator<O> &&
        Mergeable<I1, I2, O, Comp>, O>
    operator()(I1 first1, I1 last1, I2 first2, I2 last2, O result,
               Comp comp = Comp{}) const
    {
        return std::merge(std::move(first1), std::move(last1),
                          std::move(first2), std::move(last2),
                          std::move(result), std::ref(comp));
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>>
    std::enable_if_t<
        InputRange<Rng1> &&
        CommonRange<Rng1> &&
        detail::Cpp98Iterator<iterator_t<Rng1>> &&
        InputRange<Rng2> &&
        CommonRange<Rng2> &&
        detail::Cpp98Iterator<iterator_t<Rng2>> &&
        WeaklyIncrementable<O> &&
        detail::Cpp98Iterator<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp>, O>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{}) const
    {
        return std::merge(nano::begin(rng1), nano::end(rng1),
                          nano::begin(rng2), nano::end(rng2),
                          std::move(result), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::merge_fn, merge)

NANO_END_NAMESPACE

#endif
