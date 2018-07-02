// nanorange/algorithm/upper_bound.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UPPER_BOUND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UPPER_BOUND_HPP_INCLUDED

#include <nanorange/algorithm/partition_point.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct upper_bound_fn {
private:
    friend struct equal_range_fn;

    template <typename Comp, typename T>
    struct compare {
        Comp& comp;
        const T& val;

        template <typename U>
        constexpr bool operator()(U&& u) const
        {
            return !nano::invoke(comp, val, std::forward<U>(u));
        }
    };


    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, const T& value, Comp& comp, Proj& proj)
    {
        const auto comparator = compare<Comp, T>{comp, value};
        return partition_point_fn::impl(std::move(first), std::move(last),
                                        comparator, proj);
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    I>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return upper_bound_fn::impl(std::move(first), std::move(last),
                                    value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return upper_bound_fn::impl(nano::begin(rng), nano::end(rng),
                                    value, comp, proj);
    }
};

}

NANO_DEFINE_CPO(detail::upper_bound_fn, upper_bound)

NANO_END_NAMESPACE

#endif
