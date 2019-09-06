// nanorange/algorithm/reverse_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using reverse_copy_result = copy_result<I, O>;

namespace detail {

struct reverse_copy_fn {
private:
    template <typename I, typename O>
    static constexpr reverse_copy_result<I, O> impl(I first, I last, O result)
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
        !same_as<I, S>, reverse_copy_result<I, O>>
    impl(I first, S bound, O result)
    {
        return reverse_copy_fn::impl(std::move(first), nano::next(first, bound),
                                     std::move(result));
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<
        bidirectional_iterator<I> && sentinel_for<S, I> &&
        weakly_incrementable<O> && indirectly_copyable<I, O>,
        reverse_copy_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return reverse_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<bidirectional_range<Rng> &&
        weakly_incrementable<O> &&
                                   indirectly_copyable<iterator_t<Rng>, O>,
        reverse_copy_result<safe_iterator_t<Rng>, O>>
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
