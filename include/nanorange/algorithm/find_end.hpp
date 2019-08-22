// nanorange/algorithm/find_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED

#include <nanorange/ranges.hpp>

#include <nanorange/algorithm/search.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.alg.find.end]
namespace detail {

// TODO: For BiDir iterators, we can be smarter and search backwards
struct find_end_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2>
    static constexpr subrange<I1>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
         Proj2& proj2)
    {
        if (first2 == last2) {
            auto last_it = nano::next(first1, last1);
            return {last_it, last_it};
        }

        auto result = search_fn::impl(std::move(first1), last1, first2, last2, pred, proj1, proj2);

        if (result.empty()) {
            return result;
        }

        while (true) {
            auto new_result = search_fn::impl(next(result.begin()), last1, first2, last2, pred, proj1, proj2);
            if (new_result.empty()) {
                return result;
            } else {
                result = std::move(new_result);
            }
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
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_end_fn::impl(std::move(first1), std::move(last1),
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
        return find_end_fn::impl(nano::begin(rng1), nano::end(rng1),
                                 nano::begin(rng2), nano::end(rng2),
                                 pred, proj1, proj2);
    }

};

}

NANO_INLINE_VAR(detail::find_end_fn, find_end)

NANO_END_NAMESPACE

#endif
