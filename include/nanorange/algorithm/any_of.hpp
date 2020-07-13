// nanorange/algorithm/any_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ANY_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ANY_OF_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.alg.any_of]

namespace detail {

struct any_of_fn {
private:
    friend struct is_permutation_fn;
    friend struct none_of_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr bool impl(I first, S last, Pred& pred, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first)) == true) {
                return true;
            }
            ++first;
        }
        return false;
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    [[nodiscard]] constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>,
        bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(std::move(first), std::move(last),
                               pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    [[nodiscard]] constexpr std::enable_if_t<
        input_range<Rng> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(nano::begin(rng), nano::end(rng),
                               pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::any_of_fn, any_of)

NANO_END_NAMESPACE

#endif
