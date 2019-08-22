// nanorange/algorithm/shuffle.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SHUFFLE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SHUFFLE_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/random.hpp>

#include <random>

NANO_BEGIN_NAMESPACE

namespace detail {

struct shuffle_fn {
private:
    template <typename I, typename S, typename Gen>
    static constexpr I impl(I first, S last, Gen&& g)
    {
        using diff_t = iter_difference_t<I>;
        using distr_t = std::uniform_int_distribution<diff_t>;
        using param_t = typename distr_t::param_type;

        distr_t D;
        const auto n = last - first; // OK, we have SizedSentinel

        for (diff_t i = 0; i < n; i++) {
            nano::iter_swap(first + i, first + D(g, param_t(0, i)));
        }

        return next(first, last);
    }

public:
    template <typename I, typename S, typename Gen>
    constexpr std::enable_if_t<
        random_access_iterator<I> && sentinel_for<S, I> &&
            uniform_random_bit_generator<std::remove_reference_t<Gen>> &&
        convertible_to<invoke_result_t<Gen&>, iter_difference_t<I>>,
        I>
    operator()(I first, S last, Gen&& gen) const
    {
        return shuffle_fn::impl(std::move(first), std::move(last),
                                std::forward<Gen>(gen));
    }

    template <typename Rng, typename Gen>
    constexpr std::enable_if_t<
        random_access_range<Rng> &&
            uniform_random_bit_generator<std::remove_reference_t<Gen>> &&
            convertible_to<invoke_result_t<Gen&>, iter_difference_t<iterator_t<Rng>>>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Gen&& gen) const
    {
        return shuffle_fn::impl(nano::begin(rng), nano::end(rng),
                                std::forward<Gen>(gen));
    }
};

}

NANO_INLINE_VAR(detail::shuffle_fn, shuffle)

NANO_END_NAMESPACE

#endif
