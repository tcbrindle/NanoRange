// nanorange/algorithm/minmax.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MINMAX_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MINMAX_HPP_INCLUDED

#include <nanorange/range.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// FIXME: Use tagged_pair
struct minmax_fn {
private:
    template <typename Rng, typename Comp, typename Proj,
              typename T = value_type_t<iterator_t<Rng>>>
    static constexpr std::pair<T, T>
    impl(Rng&& rng, Comp& comp, Proj& proj)
    {
        auto first = nano::begin(rng);
        const auto last = nano::end(rng);

        // Empty ranges not allowed
        auto temp = *first;
        std::pair<T, T> result{temp, std::move(temp)};

        if (++first != last) {
            {
                auto&& val = *first;
                if (nano::invoke(comp, nano::invoke(proj, val),
                                 nano::invoke(proj, result.first))) {
                    result.first = std::forward<decltype(val)>(val);
                } else if (!nano::invoke(comp, nano::invoke(proj, val),
                                         nano::invoke(proj, result.second))){
                    result.second = std::forward<decltype(val)>(val);
                }
            }

            while (++first != last) {
                T val1 = *first;

                // Last iteration
                if (++first == last) {
                    if (nano::invoke(comp, nano::invoke(proj, val1),
                                     nano::invoke(proj, result.first))) {
                        result.first = std::move(val1);
                    }
                    else if (!nano::invoke(comp, nano::invoke(proj, val1),
                                           nano::invoke(proj, result.second))){
                        result.second = std::move(val1);
                    }
                    break;
                }

                auto&& val2 = *first;
                if (nano::invoke(comp, nano::invoke(proj, val2),
                                 nano::invoke(proj, val1))) {
                    if (nano::invoke(comp, nano::invoke(proj, val2),
                                     nano::invoke(proj, result.first))) {
                        result.first = std::forward<decltype(val2)>(val2);
                    }
                    if (!nano::invoke(comp, nano::invoke(proj, val1),
                                      nano::invoke(proj, result.second))) {
                        result.second = std::move(val1);
                    }
                } else {
                    if (nano::invoke(comp, nano::invoke(proj, val1),
                                     nano::invoke(proj, result.first))) {
                        result.first = std::move(val1);
                    }
                    if (!nano::invoke(comp, nano::invoke(proj, val2),
                                      nano::invoke(proj, result.second))) {
                        result.second = std::forward<decltype(val2)>(val2);
                    }
                }
            }
        }

        return result;
    }

public:
    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
    std::pair<const T&, const T&>>
    operator()(const T& a, const T& b, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        if (nano::invoke(comp, nano::invoke(proj, b), nano::invoke(proj, a))) {
            return {b, a};
        } else {
            return {a, b};
        }
    }

    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            Copyable<T> &&
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
            std::pair<T, T>>
    operator()(std::initializer_list<T> rng, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return minmax_fn::impl(rng, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        Copyable<value_type_t<iterator_t<Rng>>> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
    std::pair<value_type_t<iterator_t<Rng>>, value_type_t<iterator_t<Rng>>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_fn::impl(std::forward<Rng>(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::minmax_fn, minmax)

NANO_END_NAMESPACE

#endif
