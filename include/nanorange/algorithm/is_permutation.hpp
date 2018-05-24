// nanorange/algorithm/is_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED

#include <nanorange/range.hpp>
#include <nanorange/algorithm/count.hpp>
#include <nanorange/algorithm/find.hpp>
#include <nanorange/algorithm/mismatch.hpp>

#include <tuple>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_permutation_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr bool impl4(I1 first1, S1 last1, I2 first2, S2 last2,
                                Pred pred, Proj1 proj1, Proj2 proj2)
    {
        for (auto it = first1; it != last1; ++it) {
            bool seen = false;
            for (auto it2 = first1; !seen && it2 != last1; ++it2) {
                if (nano::invoke(pred, nano::invoke(proj1, *it), nano::invoke(proj1, *it2))) {
                    seen = true;
                }
            }
            if (seen) {
                continue;
            }

            difference_type_t<I2> count1{};
            for (auto it2 = first2; it2 != last2; ++it2) {
                if (nano::invoke(pred, nano::invoke(proj1, *it), nano::invoke(proj2, *it2))) {
                    ++count1;
                }
            }
            if (count1 == 0) {
                return false;
            }

            difference_type_t<I1> count2{1};
            for (auto it2 = nano::next(it); it2 != last1; ++it2) {
                if (nano::invoke(pred, nano::invoke(proj1, *it), nano::invoke(proj2, *it2))) {
                    ++count2;
                }
            }
            if (count1 != count2) {
                return false;
            }
        }

        return true;
    }

    template <typename I1, typename S1, typename I2, typename Pred,
        typename Proj1, typename Proj2>
    static constexpr bool impl3(I1 first1, S1 last1, I2 first2,
                                Pred pred, Proj1 proj1, Proj2 proj2)
    {
        bool done = true;
        while (done && first1 != last1) {
            if (!nano::invoke(pred, nano::invoke(proj1, *first1), nano::invoke(proj2, *first2))) {
                done = false;
                break;
            }
            ++first1;
        }

        if (done) {
            return true;
        }

        auto last2 = std::next(first2, nano::distance(first1, last1));

        return is_permutation_fn::impl4(std::move(first1), std::move(last1),
        std::move(first2), std::move(last2), std::move(pred), std::move(proj1), std::move(proj2));
    }

public:
    // Four-legged
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr
        std::enable_if_t<ForwardIterator<I1> && Sentinel<S1, I1> &&
                             ForwardIterator<I2> && Sentinel<S2, I2> &&
                             IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
                         bool>
        operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
                   Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if /*constexpr*/ (SizedSentinel<S1, I1> && SizedSentinel<S2, I2>) {
            if (nano::distance(first1, last1) != nano::distance(first2, last2)) {
                return false;
            }
            return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                            std::move(first2), std::move(pred),
                                            std::move(proj1), std::move(proj2));
        }

        return is_permutation_fn::impl4(std::move(first1), std::move(last1),
                                        std::move(first2), std::move(last2),
                                        std::move(pred), std::move(proj1),
                                        std::move(proj2));
    }

    // Three-legged
    template <typename I1, typename S1, typename I2,
        typename Pred = equal_to<>, typename Proj1 = identity,
        typename Proj2 = identity>
            NANO_DEPRECATED
    constexpr
    std::enable_if_t<ForwardIterator<I1> && Sentinel<S1, I1> &&
                     ForwardIterator<I2> &&
                     IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                        std::move(first2), std::move(pred),
                                        std::move(proj1), std::move(proj2));

    }

    // Two ranges
    template <typename Rng1, typename Rng2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng1> && ForwardRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred,
                                 Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if (SizedRange<Rng1> && SizedRange<Rng2>) {
            if (nano::distance(rng1) != nano::distance(rng2)) {
                return false;
            }

            return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                            nano::begin(rng2), std::move(pred),
                                            std::move(proj1), std::move(proj2));
        }

        return is_permutation_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                        nano::begin(rng2), nano::end(rng2),
                                        std::move(pred), std::move(proj1),
                                        std::move(proj2));
    }

    // Range and a half
    template <typename Rng1, typename I2, typename Pred = equal_to<>,
        typename Proj1 = identity, typename Proj2 = identity>
            NANO_DEPRECATED
    constexpr std::enable_if_t<
        ForwardRange<Rng1> && ForwardIterator<I2> &&
        IndirectlyComparable<iterator_t<Rng1>, I2, Pred,
            Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, I2 first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const {
        return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                        std::move(first2), std::move(pred),
                                        std::move(proj1), std::move(proj2));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_permutation_fn, is_permutation);

NANO_END_NAMESPACE

#endif
