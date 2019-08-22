// nanorange/algorithm/replace_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_IF_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct replace_if_fn {
private:
    template <typename I, typename S, typename Pred, typename T, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, const T& new_value,
                            Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                *first = new_value;
            }
            ++first;
        }

        return first;
    }

public:
    template <typename I, typename S, typename T, typename Pred,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> && writable<I, const T&> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_if_fn::impl(std::move(first), std::move(last), pred,
                                   new_value, proj);
    }

    template <typename Rng, typename Pred, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && writable<iterator_t<Rng>, const T2&> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_if_fn::impl(nano::begin(rng), nano::end(rng), pred,
                                   new_value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_if_fn, replace_if)

NANO_END_NAMESPACE

#endif
