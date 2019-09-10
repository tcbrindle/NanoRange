// nanorange/algorithm/mismatch.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MISMATCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MISMATCH_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

// [range.mismatch]

template <typename I1, typename I2>
struct mismatch_result {
    NANO_NO_UNIQUE_ADDRESS I1 in1;
    NANO_NO_UNIQUE_ADDRESS I2 in2;

    template <typename II1, typename II2,
        std::enable_if_t<convertible_to<const I1&, II1> &&
                         convertible_to<const I2&, II2>, int> = 0>
    constexpr operator mismatch_result<II1, II2>() const &
    {
        return {in1, in2};
    }

    template <typename II1, typename II2,
        std::enable_if_t<convertible_to<I1, II1> &&
                         convertible_to<I2, II2>, int> = 0>
    constexpr operator mismatch_result<II1, II2>() &&
    {
        return {std::move(in1), std::move(in2)};
    }
};

namespace detail {

struct mismatch_fn {
private:
    friend struct is_permutation_fn;

    template <typename I1, typename S1, typename I2, typename Proj1,
              typename Proj2, typename Pred>
    static constexpr mismatch_result<I1, I2>
    impl3(I1 first1, S1 last1, I2 first2, Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 &&
               nano::invoke(pred, nano::invoke(proj1, *first1),
                            nano::invoke(proj2, *first2))) {
            ++first1;
            ++first2;
        }

        return {first1, first2};
    }

    template <typename I1, typename S1, typename I2, typename S2,
              typename Proj1, typename Proj2, typename Pred>
    static constexpr mismatch_result<I1, I2>
    impl4(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
          Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2 &&
               nano::invoke(pred, nano::invoke(proj1, *first1),
                            nano::invoke(proj2, *first2))) {
            ++first1;
            ++first2;
        }

        return {first1, first2};
    }

public:
    // three legged
    template <typename I1, typename S1, typename I2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = ranges::equal_to>
    NANO_DEPRECATED constexpr std::enable_if_t<
        input_iterator<I1> && sentinel_for<S1, I1> &&
            input_iterator<std::decay_t<I2>> &&
        !input_range<I1> &&
            indirect_relation<Pred, projected<I1, Proj1>, projected<std::decay_t<I2>, Proj2>>,
        mismatch_result<I1, std::decay_t<I2>>>
    operator()(I1 first1, S1 last1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl3(std::move(first1), std::move(last1),
                                  std::forward<I2>(first2), pred,
                                  proj1, proj2);
    }

    // range and a half
    template <typename Rng1, typename I2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = ranges::equal_to>
    NANO_DEPRECATED constexpr std::enable_if_t<
        input_range<Rng1> && input_iterator<std::decay_t<I2>> &&
                !input_range<I2> &&
            indirect_relation<Pred, projected<iterator_t<Rng1>, Proj1>,
                             projected<std::decay_t<I2>, Proj2>>,
        mismatch_result<safe_iterator_t<Rng1>, std::decay_t<I2>>>
    operator()(Rng1&& rng1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                  std::forward<I2>(first2), pred,
                                  proj1, proj2);
    }

    // four legged
    template <typename I1, typename S1, typename I2, typename S2,
              typename Proj1 = identity, typename Proj2 = identity,
              typename Pred = ranges::equal_to>
    constexpr std::enable_if_t<
        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2> &&
            sentinel_for<S2, I2> &&
            indirect_relation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
        mismatch_result<I1, I2>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl4(std::move(first1), std::move(last1),
                                  std::move(first2), std::move(last2),
                                  pred, proj1, proj2);
    }

    // two ranges
    template <typename Rng1, typename Rng2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = ranges::equal_to>
    constexpr std::enable_if_t<
        input_range<Rng1> && input_range<Rng2> &&
            indirect_relation<Pred, projected<iterator_t<Rng1>, Proj1>,
                             projected<iterator_t<Rng2>, Proj2>>,
        mismatch_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                  nano::begin(rng2), nano::end(rng2),
                                  pred, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::mismatch_fn, mismatch)

NANO_END_NAMESPACE

#endif
