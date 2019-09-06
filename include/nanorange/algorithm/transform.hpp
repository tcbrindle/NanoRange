// nanorange/algorithm/transform.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_TRANSFORM_HPP_INCLUDED
#define NANORANGE_ALGORITHM_TRANSFORM_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using unary_transform_result = copy_result<I, O>;

template <typename I1, typename I2, typename O>
struct binary_transform_result {
    NANO_NO_UNIQUE_ADDRESS I1 in1;
    NANO_NO_UNIQUE_ADDRESS I2 in2;
    NANO_NO_UNIQUE_ADDRESS O out;

    template <typename II1, typename II2, typename O2,
              std::enable_if_t<convertible_to<const I1&, II1> &&
                               convertible_to<const I2&, II2> &&
                               convertible_to<const O&, O2>, int> = 0>
    constexpr operator binary_transform_result<II1, II2, O2>() const &
    {
        return {in1, in2, out};
    }

    template <typename II1, typename II2, typename O2,
              std::enable_if_t<convertible_to<I1, II1> &&
                               convertible_to<I2, II2> &&
                               convertible_to<O, O2>, int> = 0>
    constexpr operator binary_transform_result<II1, II2, O2>() &&
    {
        return {std::move(in1), std::move(in2), std::move(out)};
    }
};


namespace detail {

struct transform_fn {
private:
    template <typename I, typename S, typename O, typename F, typename Proj>
    static constexpr unary_transform_result<I, O>
    unary_impl(I first, S last, O result, F& op, Proj& proj)
    {
        while (first != last) {
            *result = nano::invoke(op, nano::invoke(proj, *first));
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I1, typename S1, typename I2, typename O, typename F,
              typename Proj1, typename Proj2>
    static constexpr binary_transform_result<I1, I2, O>
    binary_impl3(I1 first1, S1 last1, I2 first2, O result, F& op, Proj1& proj1,
                 Proj2& proj2)
    {
        while (first1 != last1) {
            *result = nano::invoke(op, nano::invoke(proj1, *first1),
                                   nano::invoke(proj2, *first2));
            ++first1;
            ++first2;
            ++result;
        }

        return {std::move(first1), std::move(first2), std::move(result)};
    }

    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename F, typename Proj1, typename Proj2>
    static constexpr binary_transform_result<I1, I2, O>
    binary_impl4(I1 first1, S1 last1, I2 first2, S2 last2, O result, F& op,
                 Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2) {
            *result = nano::invoke(op, nano::invoke(proj1, *first1),
                                   nano::invoke(proj2, *first2));
            ++first1;
            ++first2;
            ++result;
        }

        return {std::move(first1), std::move(first2), std::move(result)};
    }

public:
    // Unary op, iterators
    template <typename I, typename S, typename O, typename F,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O> &&
            copy_constructible<F> &&
            writable<O, indirect_result_t<F&, projected<I, Proj>>>,
        unary_transform_result<I, O>>
    operator()(I first, S last, O result, F op, Proj proj = Proj{}) const
    {
        return transform_fn::unary_impl(std::move(first), std::move(last),
                                        std::move(result), op, proj);
    }

    // Unary op, range
    template <typename Rng, typename O, typename F, typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && weakly_incrementable<O> && copy_constructible<F> &&
            writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng>, Proj>>>,
        unary_transform_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, F op, Proj proj = Proj{}) const
    {
        return transform_fn::unary_impl(nano::begin(rng), nano::end(rng),
                                        std::move(result), op, proj);
    }

    // Binary op, four-legged
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename F, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2> &&
            sentinel_for<S2, I2> && weakly_incrementable<O> &&
            copy_constructible<F> &&
            writable<O, indirect_result_t<F&, projected<I1, Proj1>,
                                          projected<I2, Proj2>>>,
        binary_transform_result<I1, I2, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result, F op,
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl4(std::move(first1), std::move(last1),
                                          std::move(first2), std::move(last2),
                                          std::move(result), op, proj1, proj2);
    }

    // Binary op, two ranges
    template <typename Rng1, typename Rng2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        input_range<Rng1> && input_range<Rng2> && weakly_incrementable<O> &&
            copy_constructible<F> &&
            writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng1>, Proj1>,
                                       projected<iterator_t<Rng2>, Proj2>>>,
        binary_transform_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, F op, Proj1 proj1 = Proj1{},
               Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl4(nano::begin(rng1), nano::end(rng1),
                                          nano::begin(rng2), nano::end(rng2),
                                          std::move(result), op, proj1, proj2);
    }

    // Binary op, three-legged
    template <typename I1, typename S1, typename I2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        input_iterator<I1> && sentinel_for<S1, I1> &&
            input_iterator<std::decay_t<I2>> &&
            !input_range<I2> &&
            weakly_incrementable<O> && copy_constructible<F> &&
            writable<O, indirect_result_t<F&, projected<I1, Proj1>,
                                          projected<std::decay_t<I2>, Proj2>>>,
        binary_transform_result<I1, std::decay_t<I2>, O>>
    operator()(I1 first1, S1 last1, I2&& first2, O result, F op,
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl3(std::move(first1), std::move(last1),
                                          std::forward<I2>(first2), std::move(result),
                                          op, proj1, proj2);
    }

    // binary op, range-and-a-half
    template <typename Rng1, typename I2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        input_range<Rng1> && input_iterator<std::decay_t<I2>> &&
                !input_range<I2> && weakly_incrementable<O> &&
            copy_constructible<F> &&
            writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng1>, Proj1>,
                                       projected<std::decay_t<I2>, Proj2>>>,
        binary_transform_result<safe_iterator_t<Rng1>, std::decay_t<I2>, O>>
    operator()(Rng1&& rng1, I2&& first2, O result, F op, Proj1 proj1 = Proj1{},
               Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl3(nano::begin(rng1), nano::end(rng1),
                                          std::forward<I2>(first2), std::move(result),
                                          op, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::transform_fn, transform)

NANO_END_NAMESPACE

#endif
