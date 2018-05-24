// nanorange/detail/algorithm/stl/find_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ALGORITHM_STL_FIND_END_HPP_INCLUDED
#define NANORANGE_DETAIL_ALGORITHM_STL_FIND_END_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

NANO_BEGIN_NAMESPACE

// [ranges.alg.find.end]

template <typename I1, typename I2, typename Pred = equal_to<>>
std::enable_if_t<ForwardIterator<I1> && ForwardIterator<I2> &&
                 detail::Cpp98Iterator<I1> &&
                 detail::Cpp98Iterator<I2> &&
                 IndirectRelation<Pred, I2, I1>,
                 I1>
find_end(I1 first1, I1 last1, I2 first2, I2 last2, Pred pred = Pred{})
{
    return std::find_end(std::move(first1), std::move(last1), std::move(first2),
                         std::move(last2), std::move(pred));
}

template <typename Rng1, typename Rng2, typename Pred = equal_to<>>
std::enable_if_t<ForwardRange<Rng1> && ForwardRange<Rng2> &&
                 CommonRange<Rng1> && CommonRange<Rng2> &&
                 detail::Cpp98Iterator<iterator_t<Rng1>> &&
                 detail::Cpp98Iterator<iterator_t<Rng2>> &&
                 IndirectRelation<Pred, iterator_t<Rng2>, iterator_t<Rng1>>,
                 safe_iterator_t<Rng1>>
find_end(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{})
{
    return std::find_end(nano::begin(rng1), nano::end(rng1), nano::begin(rng2),
                         nano::end(rng2), std::move(pred));
}

NANO_END_NAMESPACE

#endif
