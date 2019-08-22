// nanorange/algorithm/includes.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_INCLUDES_HPP_INCLUDED
#define NANORANGE_ALGORITHM_INCLUDES_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct includes_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
     static constexpr bool impl(I1 first1, S1 last1, I2 first2, S2 last2,
                                Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first2 != last2) {
            // If range1 is done but we still have elements in range2, then
            // it is not a subset
            if (first1 == last1)  {
                return false;
            }

            // If the current element of r2 is less than the current
            // element of r1, then it is not in r1 => not a subset
            if (nano::invoke(comp, nano::invoke(proj2, *first2),
                             nano::invoke(proj1, *first1))) {
                return false;
            }

            // Now we know that that !(r2 < r1). If we also have !(r1 < r2),
            // then it must be equal, so in range1 -- so move onto the next
            // element
            if (!nano::invoke(comp, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                ++first2;
            }

            ++first1;
        }

        return true;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = ranges::less, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2> &&
            sentinel_for<S2, I2> &&
            indirect_strict_weak_order<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return includes_fn::impl(std::move(first1), std::move(last1),
                                 std::move(first2), std::move(last2),
                                 comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = ranges::less,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        input_range<Rng1> && input_range<Rng2> &&
            indirect_strict_weak_order<Comp,
                                projected<iterator_t<Rng1>, Proj1>,
                                projected<iterator_t<Rng2>, Proj2>>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return includes_fn::impl(nano::begin(rng1), nano::end(rng1),
                                 nano::begin(rng2), nano::end(rng2),
                                 comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::includes_fn, includes)

NANO_END_NAMESPACE

#endif
