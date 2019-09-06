// nanorange/algorithm/move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using move_result = copy_result<I, O>;

namespace detail {

struct move_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<sized_sentinel_for<S, I>, move_result<I, O>>
    impl(I first, S last, O result, priority_tag<1>)
    {
        const auto dist = last - first;

        for (iter_difference_t<I> i{0}; i < dist; i++) {
            *result = nano::iter_move(first);
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr move_result<I, O> impl(I first, S last, O result,
                                            priority_tag<0>)
    {
        while (first != last) {
            *result = nano::iter_move(first);
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<input_iterator<I> && sentinel_for<S, I> &&
                                   weakly_incrementable<O> &&
                                   indirectly_movable<I, O>,
                               move_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<input_range<Rng> && weakly_incrementable<O> &&
                                   indirectly_movable<iterator_t<Rng>, O>,
                               move_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return move_fn::impl(nano::begin(rng), nano::end(rng),
                             std::move(result), priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::move_fn, move)

template <typename I1, typename I2>
using move_backward_result = copy_result<I1, I2>;

namespace detail {

struct move_backward_fn {
private:
    template <typename I, typename O>
    static constexpr move_backward_result<I, O> impl(I first, I last, O result)
    {
        auto it = last;

        while (it != first) {
            *--result = nano::iter_move(--it);
        }

        return {std::move(last), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<!same_as<I, S>, move_backward_result<I, O>>
    impl(I first, S sent, O result)
    {
        I last = nano::next(first, sent);
        return impl(std::move(first), std::move(last), std::move(result));
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<
        bidirectional_iterator<I> && sentinel_for<S, I> &&
            bidirectional_iterator<O> && indirectly_movable<I, O>,
                               move_backward_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<bidirectional_range<Rng> &&
                                   bidirectional_iterator<O> &&
                                   indirectly_movable<iterator_t<Rng>, O>,
                               move_backward_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return move_backward_fn::impl(nano::begin(rng), nano::end(rng),
                                      std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::move_backward_fn, move_backward)

NANO_END_NAMESPACE

#endif
