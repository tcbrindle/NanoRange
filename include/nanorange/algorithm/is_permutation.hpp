// nanorange/algorithm/is_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED

#include <nanorange/algorithm/any_of.hpp>
#include <nanorange/algorithm/count.hpp>
#include <nanorange/algorithm/mismatch.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_permutation_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr bool process_tail(I1 first1, S1 last1, I2 first2, S2 last2,
                                       Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        for (auto it = first1; it != last1; ++it) {
            const auto comp = [&pred, val = nano::invoke(proj1, *it)]
                    (const auto& t) { return nano::invoke(pred, t, val); };

            // Check whether we have already seen this value
            if (any_of_fn::impl(first1, it, comp, proj1)) {
                continue;
            }

            // Count how many times *it appears in range2
            const auto count1 = count_if_fn::impl(first2, last2, comp, proj2);

            // If we have a count of zero, we know the ranges are different
            if (count1 == 0) {
                return false;
            }

            // Count how many times *it appears in the remainder of range1
            // (we can start from one)
            const auto count2 = iter_difference_t<I1>{1} +
                count_if_fn::impl(nano::next(it), last1, comp, proj1);

            if (count1 != count2) {
                return false;
            }
        }

        return true;
    }

    template <typename I1, typename S1, typename I2, typename Pred,
        typename Proj1, typename Proj2>
    static constexpr bool impl3(I1 first1, S1 last1, I2 first2,
                                Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        // Strip equal prefixes from both ranges
        auto result = mismatch_fn::impl3(std::move(first1), last1,
                                         std::move(first2),
                                         pred, proj1, proj2);
        first1 = std::move(result).in1;
        first2 = std::move(result).in2;

        if (first1 == last1) {
            return true;
        }

        // If we have only one value left in range1, it can't be in range2
        const auto d = nano::distance(first1, last1);
        if (d == 1) {
            return false;
        }

        auto last2 = nano::next(first2, d);

        return is_permutation_fn::process_tail(std::move(first1), std::move(last1),
                                               std::move(first2), std::move(last2),
                                               pred, proj1, proj2);
    }

    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2>
    static constexpr bool impl4(I1 first1, S1 last1, I2 first2, S2 last2,
                                Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        // Strip equal prefixes from both ranges
        auto result = mismatch_fn::impl4(std::move(first1), last1,
                                         std::move(first2), last2,
                                         pred, proj1, proj2);
        first1 = std::move(result).in1;
        first2 = std::move(result).in2;

        // If we have reached the end of both ranges, they were the same
        if (first1 == last1 && first2 == last2) {
            return true;
        }

        // If we have different numbers of elements left in the ranges,
        // they are not permutations of one another
        if (nano::distance(first1, last1) != nano::distance(first2, last2)) {
            return false;
        }

        return is_permutation_fn::process_tail(std::move(first1), std::move(last1),
                                               std::move(first2), std::move(last2),
                                               pred, proj1, proj2);
    }

public:
    // Four-legged
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = ranges::equal_to, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr
        std::enable_if_t<
        forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2> &&
            sentinel_for<S2, I2> &&
            indirectly_comparable<I1, I2, Pred, Proj1, Proj2>,
                         bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if constexpr (sized_sentinel_for<S1, I1> &&
                      sized_sentinel_for<S2, I2>) {
            if (nano::distance(first1, last1) != nano::distance(first2, last2)) {
                return false;
            }
            return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                            std::move(first2), pred,
                                            proj1, proj2);
        }

        return is_permutation_fn::impl4(std::move(first1), std::move(last1),
                                        std::move(first2), std::move(last2),
                                        pred, proj1, proj2);
    }

    // Three-legged
    template <typename I1, typename S1, typename I2,
        typename Pred = ranges::equal_to, typename Proj1 = identity,
        typename Proj2 = identity>
    NANO_DEPRECATED
    constexpr
    std::enable_if_t<
        forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2> &&
            indirectly_comparable<I1, I2, Pred, Proj1, Proj2>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                        std::move(first2), pred,
                                        proj1, proj2);

    }

    // Two ranges
    template <typename Rng1, typename Rng2, typename Pred = ranges::equal_to,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        forward_range<Rng1> && forward_range<Rng2> &&
            indirectly_comparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred,
                                 Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if (sized_range<Rng1> && sized_range<Rng2>) {
            if (nano::distance(rng1) != nano::distance(rng2)) {
                return false;
            }

            return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                            nano::begin(rng2), pred,
                                            proj1, proj2);
        }

        return is_permutation_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                        nano::begin(rng2), nano::end(rng2),
                                        pred, proj1, proj2);
    }

    // Range and a half
    template <typename Rng1, typename I2, typename Pred = ranges::equal_to,
        typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED
    constexpr std::enable_if_t<
        forward_range<Rng1> && forward_iterator<std::decay_t<I2>> &&
        !range<I2> &&
            indirectly_comparable<iterator_t<Rng1>, I2, Pred, Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const {
        return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                        std::forward<I2>(first2), pred,
                                        proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_permutation_fn, is_permutation)

NANO_END_NAMESPACE

#endif
