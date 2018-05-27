// nanorange/algorithm/stl/partial_sort_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_PARTIAL_SORT_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_PARTIAL_SORT_COPY_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct partial_sort_copy_fn {
    template <typename I1, typename I2, typename Comp = less<>>
    std::enable_if_t<
        InputIterator<I1> &&
        detail::Cpp98Iterator<I1> &&
        RandomAccessIterator<I2> &&
        detail::Cpp98Iterator<I2> &&
        IndirectlyCopyable<I1, I2> &&
        Sortable<I2, Comp> &&
        IndirectStrictWeakOrder<Comp, I1, I2>, I2>
    operator()(I1 first, I1 last, I2 result_first, I2 result_last, Comp comp = Comp{})
    {
        return std::partial_sort_copy(std::move(first), std::move(last),
                                      std::move(result_first), std::move(result_last),
                                      std::ref(comp));
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>>
    std::enable_if_t<
        InputRange<Rng1> &&
        CommonRange<Rng1> &&
        detail::Cpp98Iterator<iterator_t<Rng1>> &&
        RandomAccessRange<Rng2> &&
        CommonRange<Rng2> &&
        detail::Cpp98Iterator<iterator_t<Rng2>> &&
        IndirectlyCopyable<iterator_t<Rng1>, iterator_t<Rng2>> &&
        Sortable<iterator_t<Rng2>, Comp> &&
        IndirectStrictWeakOrder<Comp, iterator_t<Rng1>, iterator_t<Rng2>>,
    safe_iterator_t<Rng2>>
    operator()(Rng1&& rng, Rng2&& result_rng, Comp comp = Comp{})
    {
        return std::partial_sort_copy(nano::begin(rng), nano::end(rng),
                                      nano::begin(result_rng), nano::end(result_rng),
                                      std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::partial_sort_copy_fn, partial_sort_copy)

NANO_END_NAMESPACE

#endif
