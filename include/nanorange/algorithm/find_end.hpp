// nanorange/algorithm/find_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <nanorange/algorithm/search.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.alg.find.end]
namespace detail {

// TODO: For BiDir iterators, we can be smarter and search backwards
struct find_end_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj>
    static constexpr I1 impl(I1 first1, S1 last1, I2 first2, S2 last2,
                             Pred& pred, Proj& proj)
    {
        if (first2 == last2) {
            return next(first1, last1);
        }

        first1 = search_fn::impl(std::move(first1), last1, first2, last2, pred, proj);

        if (first1 == last1) {
            return first1;
        }

        while (true) {
            auto new_result = search_fn::impl(next(first1), last1, first2, last2, pred, proj);
            if (new_result == last1) {
                return first1;
            } else {
                first1 = std::move(new_result);
            }
        }
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
            typename Pred = equal_to<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I1> &&
        Sentinel<S1, I1> &&
        ForwardIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectRelation<Pred, I2, projected<I1, Proj>>, I1>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
                             Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return find_end_fn::impl(std::move(first1), std::move(last1),
                                 std::move(first2), std::move(last2),
                                 pred, proj);
    }

    template <typename Rng1, typename Rng2,
            typename Pred = equal_to<>, typename Proj = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng1> &&
            ForwardRange<Rng2> &&
            IndirectRelation<Pred, iterator_t<Rng2>, projected<iterator_t<Rng1>, Proj>>,
            safe_iterator_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2,
               Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return find_end_fn::impl(nano::begin(rng1), nano::end(rng1),
                                 nano::begin(rng2), nano::end(rng2),
                                 pred, proj);
    }

};

}

NANO_INLINE_VAR(detail::find_end_fn, find_end)

#if 0

template <typename I1, typename I2, typename Pred = equal_to<>>
std::enable_if_t<ForwardIterator<I1> && ForwardIterator<I2> &&
                 detail::Cpp98Iterator<I1> &&
                 detail::Cpp98Iterator<I2> &&
                 IndirectRelation<Pred, I2, I1>,
                 I1>
find_end(I1 first1, I1 last1, I2 first2, I2 last2, Pred pred = Pred{})
{
    return std::find_end(std::move(first1), std::move(last1), std::move(first2),
                         std::move(last2), std::move(pred));
}

template <typename Rng1, typename Rng2, typename Pred = equal_to<>>
std::enable_if_t<ForwardRange<Rng1> && ForwardRange<Rng2> &&
                 CommonRange<Rng1> && CommonRange<Rng2> &&
                 detail::Cpp98Iterator<iterator_t<Rng1>> &&
                 detail::Cpp98Iterator<iterator_t<Rng2>> &&
                 IndirectRelation<Pred, iterator_t<Rng2>, iterator_t<Rng1>>,
                 safe_iterator_t<Rng1>>
find_end(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{})
{
    return std::find_end(nano::begin(rng1), nano::end(rng1), nano::begin(rng2),
                         nano::end(rng2), std::move(pred));
}

#endif

NANO_END_NAMESPACE

#endif
