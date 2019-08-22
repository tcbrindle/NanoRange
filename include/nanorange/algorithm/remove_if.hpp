// nanorange/algorithm/remove_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_IF_HPP_INCLUDED

#include <nanorange/ranges.hpp>

#include <nanorange/algorithm/find.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct remove_if_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if(std::move(first), last, pred, proj);

        if (first == last) {
            return first;
        }

        for (auto i = next(first); i != last; ++i) {
            if (!nano::invoke(pred, nano::invoke(proj, *i))) {
                *first = nano::iter_move(i);
                ++first;
            }
        }

        return first;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_iterator<I> && sentinel_for<S, I> && permutable<I> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return remove_if_fn::impl(std::move(first), std::move(last), pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_range<Rng> && permutable<iterator_t<Rng>> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return remove_if_fn::impl(nano::begin(rng), nano::end(rng), pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_if_fn, remove_if)

NANO_END_NAMESPACE

#endif
