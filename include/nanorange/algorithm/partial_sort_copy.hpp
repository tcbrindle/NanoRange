// nanorange/algorithm/partial_sort_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTIAL_SORT_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTIAL_SORT_COPY_HPP_INCLUDED

#include <nanorange/algorithm/make_heap.hpp>
#include <nanorange/algorithm/sort_heap.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct partial_sort_copy_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr I2 impl(I1 first, S1 last, I2 result_first,
                             S2 result_last, Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        I2 r = result_first;
        if (r == result_last) {
            return r;
        }

        while (r != result_last && first != last) {
            *r = *first;
            ++r;
            ++first;
        }

        nano::make_heap(result_first, r, comp, proj2);
        const auto len = nano::distance(result_first, r);

        while (first != last) {
            iter_reference_t<I1>&& x = *first;
            if (nano::invoke(comp, nano::invoke(proj1, x), nano::invoke(proj2, *result_first))) {
                *result_first = std::forward<iter_reference_t<I1>>(x);
                detail::sift_down_n(result_first, len, result_first, comp, proj2);
            }
            ++first;
        }

        nano::sort_heap(result_first, r, comp, proj2);

        return r;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = less<>, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        RandomAccessIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectlyCopyable<I1, I2> &&
        Sortable<I2, Comp, Proj2> &&
        IndirectStrictWeakOrder<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
    I2>
    operator()(I1 first, S1 last, I2 result_first, S2 result_last, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return partial_sort_copy_fn::impl(std::move(first), std::move(last),
                                   std::move(result_first), std::move(result_last),
                                   comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        RandomAccessRange<Rng2> &&
        IndirectlyCopyable<iterator_t<Rng1>, iterator_t<Rng2>> &&
        Sortable<iterator_t<Rng2>, Comp, Proj2> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng1>, Proj1>, projected<iterator_t<Rng2>, Proj2>>,
    safe_iterator_t<Rng2>>
    operator()(Rng1&& rng, Rng2&& result_rng, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return partial_sort_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                          nano::begin(result_rng), nano::end(result_rng),
                                          comp, proj1, proj2);
    }
};

}

NANO_DEFINE_CPO(detail::partial_sort_copy_fn, partial_sort_copy)

NANO_END_NAMESPACE

#endif
