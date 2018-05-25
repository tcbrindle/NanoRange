// nanorange/algorithm/is_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED

#include <nanorange/range.hpp>

// TODO: Reimplement

#include <algorithm>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_permutation_fn {

    // Four-legged
    template <typename I1, typename I2, typename Pred = equal_to<>>
    std::enable_if_t<
        ForwardIterator<I1> &&
        Cpp98Iterator<I1> &&
        ForwardIterator<I2> &&
        Cpp98Iterator<I1> &&
        IndirectlyComparable<I1, I2, Pred>,
        bool>
    operator()(I1 first1, I1 last1, I2 first2, I2 last2, Pred pred = Pred{}) const
    {
        return std::is_permutation(std::move(first1), std::move(last1),
                                   std::move(first2), std::move(last2),
                                   std::ref(pred));
    }

    // Two ranges
    template <typename Rng1, typename Rng2, typename Pred = equal_to<>>
    std::enable_if_t<
            ForwardRange<Rng1> &&
            CommonRange<Rng1> &&
            Cpp98Iterator<iterator_t<Rng1>> &&
            ForwardRange<Rng2> &&
            CommonRange<Rng2> &&
            Cpp98Iterator<iterator_t<Rng2>> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred>,
            bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{}) const
    {
        return std::is_permutation(nano::begin(rng1), nano::end(rng1),
                                   nano::begin(rng2), nano::end(rng2),
                                   std::ref(pred));
    }

    // Three-legged
    template <typename I1, typename I2, typename Pred = equal_to<>>
    NANO_DEPRECATED
    std::enable_if_t<
            ForwardIterator<I1> &&
                    Cpp98Iterator<I1> &&
                    ForwardIterator<I2> &&
                    Cpp98Iterator<I1> &&
                    IndirectlyComparable<I1, I2, Pred>,
            bool>
    operator()(I1 first1, I1 last1, I2 first2, Pred pred = Pred{}) const
    {
        return std::is_permutation(std::move(first1), std::move(last1),
                                   std::move(first2), std::ref(pred));
    }

    // Range and a half
    template <typename Rng1, typename I2, typename Pred = equal_to<>>
    NANO_DEPRECATED
    std::enable_if_t<
            ForwardRange<Rng1> &&
            CommonRange<Rng1> &&
            Cpp98Iterator<iterator_t<Rng1>> &&
            ForwardIterator<I2> &&
            Cpp98Iterator<I2> &&
            IndirectlyComparable<iterator_t<Rng1>, I2, Pred>,
            bool>
    operator()(Rng1&& rng1, I2 first2, Pred pred = Pred{}) const
    {
        return std::is_permutation(nano::begin(rng1), nano::end(rng1),
                                   std::move(first2), std::ref(pred));
    }
};

}

NANO_INLINE_VAR(detail::is_permutation_fn, is_permutation)

NANO_END_NAMESPACE

#endif
