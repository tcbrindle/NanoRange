// nanorange/algorithm/is_partitioned.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_PARTITIONED_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PARTITIONED_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_partitioned_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr bool impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if_not(std::move(first), last, pred, proj);
        return nano::find_if(std::move(first), last, pred, proj) == last;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
    operator()(I first, S last, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return is_partitioned_fn::impl(std::move(first), std::move(last),
                                       pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
    operator()(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return is_partitioned_fn::impl(nano::begin(rng), nano::end(rng),
                                       pred, proj);
    }
};

}

NANO_DEFINE_CPO(detail::is_partitioned_fn, is_partitioned)

NANO_END_NAMESPACE

#endif
