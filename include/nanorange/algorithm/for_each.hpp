// nanorange/algorithm/for_each.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FOR_EACH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FOR_EACH_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

// [range.alg.foreach]

template <typename I, typename F>
struct for_each_result {
    NANO_NO_UNIQUE_ADDRESS I in;
    NANO_NO_UNIQUE_ADDRESS F fun;

    template <typename I2, typename F2,
              std::enable_if_t<convertible_to<const I&, I2> &&
                               convertible_to<const F&, F2>, int> = 0>
    constexpr operator for_each_result<I2, F2>() const &
    {
        return {in, fun};
    }

    template <typename I2, typename F2,
        std::enable_if_t<convertible_to<I, I2> &&
                         convertible_to<F, F2>, int> = 0>
    constexpr operator for_each_result<I2, F2>() &&
    {
        return {std::move(in), std::move(fun)};
    }
};

namespace detail {

struct for_each_fn {
private:
    template <typename I, typename S, typename Proj, typename Fun>
    static constexpr for_each_result<I, Fun>
    impl(I first, S last, Fun& fun, Proj& proj)
    {
        while (first != last) {
            nano::invoke(fun, nano::invoke(proj, *first));
            ++first;
        }
        return {first, std::move(fun)};
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Fun>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            indirect_unary_invocable<Fun, projected<I, Proj>>,
        for_each_result<I, Fun>>
    operator()(I first, S last, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(std::move(first), std::move(last),
                                 fun, proj);
    }

    template <typename Rng, typename Proj = identity, typename Fun>
    constexpr std::enable_if_t<
        input_range<Rng> &&
            indirect_unary_invocable<Fun, projected<iterator_t<Rng>, Proj>>,
        for_each_result<safe_iterator_t<Rng>, Fun>>
    operator()(Rng&& rng, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(nano::begin(rng), nano::end(rng),
                                 fun, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::for_each_fn, for_each)

NANO_END_NAMESPACE

#endif
