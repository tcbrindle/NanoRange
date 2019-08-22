// nanorange/algorithm/inplace_merge.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_INPLACE_MERGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_INPLACE_MERGE_HPP_INCLUDED

#include <nanorange/algorithm/lower_bound.hpp>
#include <nanorange/algorithm/merge.hpp>
#include <nanorange/algorithm/min.hpp>
#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/rotate.hpp>
#include <nanorange/algorithm/upper_bound.hpp>
#include <nanorange/detail/memory/temporary_vector.hpp>
#include <nanorange/iterator/back_insert_iterator.hpp>
#include <nanorange/iterator/move_iterator.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct inplace_merge_fn {
private:
    friend struct stable_sort_fn;

    template <typename I, typename Pred, typename Proj>
    static void impl_slow(I first, I middle, I last,
                          iter_difference_t<I> len1, iter_difference_t<I> len2,
                          Pred& pred, Proj& proj)
    {
        using dist_t = iter_difference_t<I>;

        while (true) {
            // if middle == end, we're done
            if (len2 == 0) {
                return;
            }

            // shrink [first, middle) as much as possible (with no moves),
            // returning if it shrinks to 0
            for (; true; ++first, --len1) {
                if (len1 == 0) {
                    return;
                }
                if (nano::invoke(pred, nano::invoke(proj, *middle),
                        nano::invoke(proj, *first))) {
                    break;
                }
            }

            /*if (len1 <= buf.size() || len2 <= buf.size()) {
                impl(std::move(begin), std::move(middle),
                     std::move(end), len1, len2, buf, pred, proj);
                return;
            }*/

            // first < middle < end
            // *first > *middle
            // partition [first, m1) [m1, middle) [middle, m2) [m2, end) such that
            //     all elements in:
            //         [first, m1)  <= [middle, m2)
            //         [middle, m2) <  [m1, middle)
            //         [m1, middle) <= [m2, end)
            //     and m1 or m2 is in the middle of its range
            I m1;  // "median" of [first, middle)
            I m2;  // "median" of [middle, end)
            dist_t len11;      // distance(first, m1)
            dist_t len21;      // distance(middle, m2)
            // binary search smaller range
            if (len1 < len2) {
                // len >= 1, len2 >= 2
                len21 = len2 / 2;
                m2 = nano::next(middle, len21);
                m1 = nano::upper_bound(first, middle, nano::invoke(proj, *m2),
                                       std::ref(pred), std::ref(proj));
                len11 = nano::distance(first, m1);
            } else {
                if (len1 == 1) {
                    // len1 >= len2 && len2 > 0, therefore len2 == 1
                    // It is known *first > *middle
                    nano::iter_swap(first, middle);
                    return;
                }
                // len1 >= 2, len2 >= 1
                len11 = len1 / 2;
                m1 = nano::next(first, len11);
                m2 = nano::lower_bound(middle, last, nano::invoke(proj, *m1),
                                 std::ref(pred), std::ref(proj));
                len21 = nano::distance(middle, m2);
            }
            dist_t len12 = len1 - len11;  // distance(m1, middle)
            dist_t len22 = len2 - len21;  // distance(m2, end)
            // [first, m1) [m1, middle) [middle, m2) [m2, end)
            // swap middle two partitions
            middle = nano::rotate(m1, std::move(middle), m2).begin();
            // len12 and len21 now have swapped meanings
            // merge smaller range with recursive call and larger with tail recursion elimination
            if(len11 + len21 < len12 + len22) {
                impl_slow(std::move(first), std::move(m1), middle, len11, len21,
                     pred, proj);
                first = std::move(middle);
                middle = std::move(m2);
                len1 = len12;
                len2 = len22;
            } else {
                impl_slow(middle, std::move(m2), std::move(last), len12, len22,
                     pred, proj);
                last = std::move(middle);
                middle = std::move(m1);
                len1 = len11;
                len2 = len21;
            }
        }
    }

    template <typename I, typename Buf, typename Comp, typename Proj>
    static void impl_buffered(I first, I middle, I last,
                              iter_difference_t<I> len1, iter_difference_t<I> len2,
                              Buf& buf, Comp& comp, Proj& proj)
    {
        if (len1 <= len2) {
            nano::move(first, middle, nano::back_inserter(buf));
            nano::merge(
                nano::make_move_iterator(buf.begin()),
                nano::make_move_sentinel(buf.end()),
                nano::make_move_iterator(std::move(middle)),
                nano::make_move_sentinel(std::move(last)),
                std::move(first), std::ref(comp), std::ref(proj), std::ref(proj));
        } else {
            nano::move(middle, last, nano::back_inserter(buf));
            using ri_t = nano::reverse_iterator<I>;
            // TODO: C++17's not_fn would be useful
            auto not_comp = [&comp] (auto&& a, auto&& b) {
                return !nano::invoke(comp, std::forward<decltype(a)>(a),
                        std::forward<decltype(b)>(b));
            };
            nano::merge(
                nano::make_move_iterator(ri_t{std::move(middle)}),
                nano::make_move_sentinel(ri_t{std::move(first)}),
                nano::make_move_iterator(nano::rbegin(buf)),
                nano::make_move_sentinel(nano::rend(buf)),
                nano::make_reverse_iterator(std::move(last)),
                not_comp, std::ref(proj), std::ref(proj));
        }
    }

    template <typename I, typename S, typename Comp, typename Proj>
    static I impl(I first, I middle, S last, Comp& comp, Proj& proj)
    {
        auto dist1 = nano::distance(first, middle);
        I ilast = middle;
        iter_difference_t<I> dist2 = 0;
        while (ilast != last) {
            ++ilast;
            ++dist2;
        }

        const auto sz = nano::min(dist1, dist2);
        auto buf = detail::temporary_vector<iter_value_t<I>>(sz);

        if (buf.capacity() >= static_cast<std::size_t>(sz)) {
            impl_buffered(std::move(first), std::move(middle), std::move(ilast),
                          dist1, dist2, buf, comp, proj);
        } else {
            impl_slow(std::move(first), std::move(middle), std::move(ilast),
                      dist1, dist2, comp, proj);
        }

        return ilast;
    }

public:
    template <typename I, typename S, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I> &&
                         sortable<I, Comp, Proj>, I>
    operator()(I first, I middle, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return inplace_merge_fn::impl(std::move(first), std::move(middle),
                                      std::move(last), comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<bidirectional_range<Rng> &&
                         sortable<iterator_t<Rng>, Comp, Proj>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, iterator_t<Rng> middle, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return inplace_merge_fn::impl(nano::begin(rng), std::move(middle),
                                      nano::end(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::inplace_merge_fn, inplace_merge)

NANO_END_NAMESPACE

#endif
