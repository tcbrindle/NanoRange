// nanorange/algorithm/reverse.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REVERSE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REVERSE_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct reverse_fn {
private:
    template <typename I>
    static constexpr I impl(I first, I last)
    {
        I ret = last;
        while (first != last && first !=  --last) {
            nano::iter_swap(first, last);
            ++first;
        }

        return ret;
    }

    template <typename I, typename S>
    static constexpr std::enable_if_t<
        !same_as<I, S>, I>
    impl(I first, S bound)
    {
        I last = next(first, bound);
        return reverse_fn::impl(std::move(first), std::move(last));
    }

public:
    template <typename I, typename S>
    constexpr std::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I>,
        I>
    operator()(I first, S last) const
    {
        return reverse_fn::impl(std::move(first), std::move(last));
    }

    template <typename Rng>
    constexpr std::enable_if_t<bidirectional_range<Rng>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const
    {
        return reverse_fn::impl(nano::begin(rng), nano::end(rng));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::reverse_fn, reverse)

NANO_END_NAMESPACE

#endif
