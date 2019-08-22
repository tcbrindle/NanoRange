// nanorange/algorithm/nth_element.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//

//
//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_NTH_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_NTH_ELEMENT_HPP_INCLUDED

#include <nanorange/algorithm/min_element.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct nth_element_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static constexpr void impl(I first, I nth, I last, Comp& comp, Proj& proj)
    {
        constexpr iter_difference_t<I> limit = 7;

        const auto pred = [&comp, &proj] (auto&& t, auto&& u) {
            return nano::invoke(comp, nano::invoke(proj, std::forward<decltype(t)>(t)),
                                nano::invoke(proj, std::forward<decltype(u)>(u)));
        };

        I end = last;

        while (true) {
            if (nth == end) return;

            iter_difference_t<I> len = end - first;

            switch (len) {
            case 0:
            case 1:
                return;
            case 2:
                if (pred(*--end, *first)) iter_swap(first, end);
                return;
            case 3:
            {
                I m = first;
                sort3(first, ++m, --end, comp, proj);
                return;
            }
            default: break;
            }
            if (len <= limit) {
                selection_sort(first, end, comp, proj);
                return;
            }
            // Post: len > limit

            I m = first + len / 2;
            I lm1 = end;
            unsigned n_swaps = sort3(first, m, --lm1, comp, proj);
            // Post: *m is median

            // partition [first, m) < *m and *m <= [m, end)
            //(this inhibits tossing elements equivalent to m around unnecessarily)
            I i = first;
            I j = lm1;
            // j points beyond range to be tested, *lm1 is known to be <= *m
            // The search going up is known to be guarded but the search coming down isn't.
            // Prime the downward search with a guard.
            if (!pred(*i, *m)) {  // if *first == *m
                // *first == *m, *first doesn't go in first part
                // manually guard downward moving j against i
                bool restart = false;
                while (true) {
                    if (i == --j) {
                        // *first == *m, *m <= all other elements
                        // Parition instead into [first, i) == *first and *first < [i, end)
                        ++i;  // first + 1
                        j = end;
                        if (!pred(*first, *--j)) {  // we need a guard if *first == *(end-1)
                            while (true) {
                                if (i == j) {
                                    return;  // [first, end) all equivalent elements
                                }
                                if (pred(*first, *i)) {
                                    iter_swap(i, j);
                                    ++n_swaps;
                                    ++i;
                                    break;
                                }
                                ++i;
                            }
                        }
                        // [first, i) == *first and *first < [j, end) and j == end - 1
                        if (i == j) return;

                        while (true) {
                            while (!pred(*first, *i)) { ++i; }
                            while (pred(*first, *--j)) {}
                            if (i >= j) break;
                            iter_swap(i, j);
                            ++n_swaps;
                            ++i;
                        }
                        // [first, i) == *first and *first < [i, end)
                        // The first part is sorted,
                        if (nth < i) return;

                        // nth_element the second part
                        // nth_element<C>(i, nth, end, comp);
                        restart = true;
                        break;
                    }
                    if (pred(*j, *m)) {
                        iter_swap(i, j);
                        ++n_swaps;
                        break;  // found guard for downward moving j, now use unguarded partition
                    }
                }
                if (restart) {
                    first = i;
                    continue;
                }
            }
            ++i;
            // j points beyond range to be tested, *lm1 is known to be <= *m
            // if not yet partitioned...
            if (i < j) {
                // known that *(i - 1) < *m
                while (true) {
                    // m still guards upward moving i
                    while (pred(*i, *m)) { ++i; }
                    // It is now known that a guard exists for downward moving j
                    while (!pred(*--j, *m)) {}
                    if (i >= j) break;
                    iter_swap(i, j);
                    ++n_swaps;
                    // It is known that m != j
                    // If m just moved, follow it
                    if (m == i) m = j;
                    ++i;
                }
            }
            // [first, i) < *m and *m <= [i, end)
            if (i != m && pred(*m, *i)) {
                iter_swap(i, m);
                ++n_swaps;
            }
            // [first, i) < *i and *i <= [i+1, end)
            if (nth == i) return;

            if (n_swaps == 0) {
                // We were given a perfectly partitioned sequence.  Coincidence?
                if (nth < i) {
                    // Check for [first, i) already sorted
                    j = m = first;
                    while (true) {
                        if (++j == i) {
                            // [first, i) sorted
                            return;
                        }
                        if (pred(*j, *m)) {
                            // not yet sorted, so sort
                            break;
                        }
                        m = j;
                    }
                } else {
                    // Check for [i, end) already sorted
                    j = m = i;
                    while (true) {
                        if (++j == end) {
                            // [i, end) sorted
                            return;
                        }
                        if (pred(*j, *m)) {
                            // not yet sorted, so sort
                            break;
                        }
                        m = j;
                    }
                }
            }

            // nth_element on range containing nth
            if (nth < i) {
                // nth_element<C>(first, nth, i, comp);
                end = i;
            } else {
                // nth_element<C>(i+1, nth, end, comp);
                first = ++i;
            }
        }
    }

    template <typename I, typename Comp, typename Proj>
    static constexpr unsigned sort3(I x, I y, I z, Comp& comp, Proj& proj)
    {
        const auto pred = [&comp, &proj] (auto&& t, auto&& u) {
            return nano::invoke(comp, nano::invoke(proj, std::forward<decltype(t)>(t)),
                                nano::invoke(proj, std::forward<decltype(u)>(u)));
        };

        if (!pred(*y, *x)) {      // if x <= y
            if (!pred(*z, *y)) {  // if y <= z
                return 0;         // x <= y && y <= z
            }
            // x <= y && y > z
            iter_swap(y, z);      // x <= z && y < z
            if (pred(*y, *x)) {   // if x > y
                iter_swap(x, y);  // x < y && y <= z
                return 2;
            }
            return 1;             // x <= y && y < z
        }
        if (pred(*z, *y)) {       // x > y, if y > z
            iter_swap(x, z);      // x < y && y < z
            return 1;
        }
        iter_swap(x, y);          // x > y && y <= z
        // x < y && x <= z
        if (pred(*z, *y)) {       // if y > z
            iter_swap(y, z);      // x <= y && y < z
            return 2;
        }
        return 1;
    }

    template <typename I, typename Comp, typename Proj>
    static constexpr void selection_sort(I first, I last, Comp& comp, Proj& proj)
    {
        for (I lm1 = prev(last); first != lm1; ++first) {
            I i = min_element_fn::impl(first, last, comp, proj);
            if (i != first) {
                iter_swap(first, i);
            }
        }
    }

public:
    template <typename I, typename S, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<random_access_iterator<I> && sentinel_for<S, I> &&
                         sortable<I, Comp, Proj>, I>
    constexpr operator()(I first, I nth, S last,
                         Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const I ilast = nano::next(nth, last);
        impl(std::move(first), nth, std::move(ilast), comp, proj);
        return ilast;
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<random_access_range<Rng> &&
                         sortable<iterator_t<Rng>, Comp, Proj>,
    safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, iterator_t<Rng> nth,
                         Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto last = nano::next(nth, nano::end(rng));
        impl(nano::begin(rng), std::move(nth), last, comp, proj);
        return last;
    }
};

}

NANO_INLINE_VAR(detail::nth_element_fn, nth_element)

NANO_END_NAMESPACE

#endif
