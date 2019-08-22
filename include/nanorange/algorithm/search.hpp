// nanorange/algorithm/search.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SEARCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SEARCH_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct search_fn {
private:
    friend struct find_end_fn;

    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2 = identity>
    static constexpr subrange<I1>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
         Proj2& proj2)
    {
        while (true) {
            auto it1 = first1;
            auto it2 = first2;

            while (true) {
                if (it2 == last2) {
                    return {first1, it1};
                }
                if (it1 == last1) {
                    return {it1, it1};
                }
                if (!nano::invoke(pred, nano::invoke(proj1, *it1), nano::invoke(proj2, *it2))) {
                    break;
                }
                ++it1; ++it2;
            }


            ++first1;
        }
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = ranges::equal_to, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2> &&
            sentinel_for<S2, I2> &&
            indirectly_comparable<I1, I2, Pred, Proj1, Proj2>,
         subrange<I1>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
               Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return search_fn::impl(std::move(first1), std::move(last1),
                               std::move(first2), std::move(last2),
                               pred, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Pred = ranges::equal_to,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        forward_range<Rng1> && forward_range<Rng2> &&
            indirectly_comparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred, Proj1, Proj2>,
            safe_subrange_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return search_fn::impl(nano::begin(rng1), nano::end(rng1),
                               nano::begin(rng2), nano::end(rng2),
                               pred, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::search_fn, search)

NANO_END_NAMESPACE

#endif