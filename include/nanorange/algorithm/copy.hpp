// nanorange/algorithm/copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_COPY_HPP_INCLUDED

#include <nanorange/iterator/operations.hpp>
#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
struct copy_result {
    I in;
    O out;
};

namespace detail {

struct copy_fn {
private:
    // If we know the distance between first and last, we can use that
    // information to (potentially) allow better codegen
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<SizedSentinel<S, I>, copy_result<I, O>>
    impl(I first, S last, O result, priority_tag<1>)
    {
        const auto dist = last - first;

        for (iter_difference_t<I> i = 0; i < dist; ++i) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr copy_result<I, O> impl(I first, S last, O result,
                                            priority_tag<0>)
    {
        while (first != last) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<InputIterator<I> && Sentinel<S, I> &&
                                   WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<I, O>,
                               copy_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return copy_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<InputRange<Rng> && WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<iterator_t<Rng>, O>,
                               copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return copy_fn::impl(nano::begin(rng), nano::end(rng),
                             std::move(result), priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_fn, copy)

template <typename I, typename O>
using copy_n_result = copy_result<I, O>;

namespace detail {

struct copy_n_fn {
    template <typename I, typename O>
    constexpr std::enable_if_t<InputIterator<I> && WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<I, O>,
                               copy_n_result<I, O>>
    operator()(I first, iter_difference_t<I> n, O result) const
    {
        for (iter_difference_t<I> i{}; i < n; i++) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_n_fn, copy_n)

template <typename I, typename O>
using copy_if_result = copy_result<I, O>;

namespace detail {

struct copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename Proj>
    static constexpr copy_if_result<I, O> impl(I first, S last, O result,
                                               Pred pred, Proj proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                *result = *first;
                ++result;
            }
            ++first;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename Proj = identity,
              typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && WeaklyIncrementable<O> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>> &&
            IndirectlyCopyable<I, O>,
        copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred, Proj proj = Proj{}) const
    {
        return copy_if_fn::impl(std::move(first), std::move(last),
                                std::move(result), std::move(pred),
                                std::move(proj));
    }

    template <typename Rng, typename O, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> && WeaklyIncrementable<O> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, Proj proj = Proj{}) const
    {
        return copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                std::move(result), std::move(pred),
                                std::move(proj));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_if_fn, copy_if)

template <typename I, typename O>
using copy_backward_result = copy_result<I, O>;

namespace detail {

struct copy_backward_fn {
private:
    template <typename I1, typename S1, typename I2>
    static constexpr copy_backward_result<I1, I2>
    impl(I1 first, S1 last, I2 result)
    {
        I1 last_it = nano::next(first, std::move(last));
        I1 it = last_it;

        while (it != first) {
            *--result = *--it;
        }

        return {std::move(last_it), std::move(result)};
    }

public:
    template <typename I1, typename S1, typename I2>
    constexpr std::enable_if_t<BidirectionalIterator<I1> && Sentinel<S1, I1> &&
                                   BidirectionalIterator<I2> &&
                                   IndirectlyCopyable<I1, I2>,
                               copy_backward_result<I1, I2>>
    operator()(I1 first, S1 last, I2 result) const
    {
        return copy_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename I>
    constexpr std::enable_if_t<BidirectionalRange<Rng> &&
                                   BidirectionalIterator<I> &&
                                   IndirectlyCopyable<iterator_t<Rng>, I>,
                               copy_backward_result<safe_iterator_t<Rng>, I>>
    operator()(Rng&& rng, I result) const
    {
        return copy_backward_fn::impl(nano::begin(rng), nano::end(rng),
                                      std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_backward_fn, copy_backward)

NANO_END_NAMESPACE

#endif
