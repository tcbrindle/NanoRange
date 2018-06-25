// nanorange/algorithm/move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED

#include <nanorange/iterator/operations.hpp>
#include <nanorange/range.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct move_fn {
private:
    // FIXME: Use tagged_pair
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<SizedSentinel<S, I>, std::pair<I, O>>
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
    static constexpr std::pair<I, O> impl(I first, S last, O result,
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
    constexpr std::enable_if_t<InputIterator<I> && Sentinel<S, I> &&
                                   WeaklyIncrementable<O> &&
                                   IndirectlyMovable<I, O>,
                               std::pair<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<InputRange<Rng> && WeaklyIncrementable<O> &&
                                   IndirectlyMovable<iterator_t<Rng>, O>,
                               std::pair<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return move_fn::impl(nano::begin(rng), nano::end(rng),
                             std::move(result), priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::move_fn, move)

namespace detail {

struct move_backward_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr std::pair<I, O> impl(I first, S sent, O result)
    {
        auto last = nano::next(first, std::move(sent));
        auto it = last;

        while (it != first) {
            *--result = nano::iter_move(--it);
        }

        return {std::move(last), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<BidirectionalIterator<I> && Sentinel<S, I> &&
                                   BidirectionalIterator<O> &&
                                   IndirectlyMovable<I, O>,
                               std::pair<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<BidirectionalRange<Rng> &&
                                   BidirectionalIterator<O> &&
                                   IndirectlyMovable<iterator_t<Rng>, O>,
                               std::pair<safe_iterator_t<Rng>, O>>
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
