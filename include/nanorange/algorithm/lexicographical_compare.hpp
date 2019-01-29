// nanorange/algorithm/stl/lexicographical_compare.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_LEXICOGRAPHICAL_COMPARE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_LEXICOGRAPHICAL_COMPARE_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct lexicographical_compare_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr bool impl(I1 first1, S1 last1, I2 first2, S2 last2,
                                Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2) {
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                return true;
            }
            if (nano::invoke(comp, nano::invoke(proj2, *first2),
                             nano::invoke(proj1, *first1))) {
                return false;
            }
            ++first1; ++first2;
        }

        return first1 == last1 && first2 != last2;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr detail::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectStrictWeakOrder<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return lexicographical_compare_fn::impl(
                std::move(first1), std::move(last1),
                std::move(first2), std::move(last2),
                comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr detail::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng1>, Proj1>,
                                      projected<iterator_t<Rng2>, Proj2>>,
            bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return lexicographical_compare_fn::impl(
                nano::begin(rng1), nano::end(rng1),
                nano::begin(rng2), nano::end(rng2),
                comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::lexicographical_compare_fn, lexicographical_compare)

NANO_END_NAMESPACE

#endif
