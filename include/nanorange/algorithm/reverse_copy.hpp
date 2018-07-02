// nanorange/algorithm/reverse_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// FIXME: Use tagged_pair
struct reverse_copy_fn {
private:
    template <typename I, typename O>
    static constexpr std::pair<I, O> impl(I first, I last, O result)
    {
        auto ret = last;
        while (last != first) {
            *result = *--last;
            ++result;
        }

        return {std::move(ret), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<
        !Same<I, S>, std::pair<I, O>>
    impl(I first, S bound, O result)
    {
        I last = next(first, bound);
        return reverse_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result));
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<I, O>,
        std::pair<I, O>>
    operator()(I first, S last, O result) const
    {
        return reverse_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<iterator_t<Rng>, O>,
        std::pair<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return reverse_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                     std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::reverse_copy_fn, reverse_copy)

NANO_END_NAMESPACE

#endif
