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
    NANO_NO_UNIQUE_ADDRESS I in;
    NANO_NO_UNIQUE_ADDRESS O out;

    template <typename I2, typename O2,
              std::enable_if_t<convertible_to<const I&, I2> &&
                               convertible_to<const O&, O2>, int> = 0>
    constexpr operator copy_result<I2, O2>() const &
    {
        return {in, out};
    }

    template <typename I2, typename O2,
              std::enable_if_t<convertible_to<I, I2> &&
                               convertible_to<O, O2>, int> = 0>
    constexpr operator copy_result<I2, O2>() &&
    {
        return {std::move(in), std::move(out)};
    }
};

namespace detail {

struct copy_fn {
private:
    // If we know the distance between first and last, we can use that
    // information to (potentially) allow better codegen
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<sized_sentinel_for<S, I>, copy_result<I, O>>
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
    constexpr std::enable_if_t<input_iterator<I> && sentinel_for<S, I> &&
                                   weakly_incrementable<O> &&
                                   indirectly_copyable<I, O>,
                               copy_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return copy_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<input_range<Rng> && weakly_incrementable<O> &&
                                   indirectly_copyable<iterator_t<Rng>, O>,
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
    constexpr std::enable_if_t<input_iterator<I> && weakly_incrementable<O> &&
                                   indirectly_copyable<I, O>,
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
        input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O> &&
            indirect_unary_predicate<Pred, projected<I, Proj>> &&
            indirectly_copyable<I, O>,
        copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred, Proj proj = Proj{}) const
    {
        return copy_if_fn::impl(std::move(first), std::move(last),
                                std::move(result), std::move(pred),
                                std::move(proj));
    }

    template <typename Rng, typename O, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        input_range<Rng> && weakly_incrementable<O> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
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
    constexpr std::enable_if_t<
        bidirectional_iterator<I1> && sentinel_for<S1, I1> &&
            bidirectional_iterator<I2> && indirectly_copyable<I1, I2>,
                               copy_backward_result<I1, I2>>
    operator()(I1 first, S1 last, I2 result) const
    {
        return copy_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename I>
    constexpr std::enable_if_t<bidirectional_range<Rng> &&
                                   bidirectional_iterator<I> &&
                                   indirectly_copyable<iterator_t<Rng>, I>,
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
