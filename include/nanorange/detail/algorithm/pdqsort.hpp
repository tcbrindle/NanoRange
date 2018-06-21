// nanorange/detail/algorithm/pqdsort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Modified from pdqsort.h by Orson Peters
// https://github.com/orlp/pdqsort

/*
   pdqsort.h - Pattern-defeating quicksort.

   Copyright (c) 2015 Orson Peters

   This software is provided 'as-is', without any express or implied warranty.
   In no event will the authors be held liable for any damages arising from the
   use of this software.

   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:

   1. The origin of this software must not be misrepresented; you must not claim
   that you wrote the original software. If you use this software in a product,
   an acknowledgment in the product documentation would be appreciated but is
   not required.

   2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

   3. This notice may not be removed or altered from any source distribution.
*/

#ifndef NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED
#define NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED

#include <nanorange/range.hpp>
#include <nanorange/algorithm/make_heap.hpp>
#include <nanorange/algorithm/sort_heap.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// Partitions below this size are sorted using insertion sort.
constexpr int pdqsort_insertion_sort_threshold = 24;

// Partitions above this size use Tukey's ninther to select the pivot.
constexpr int pdqsort_ninther_threshold = 128;

// When we detect an already sorted partition, attempt an insertion sort that
// allows this amount of element moves before giving up.
constexpr int pqdsort_partial_insertion_sort_limit = 8;

// Returns floor(log2(n)), assumes n > 0.
template <class T>
constexpr int log2(T n)
{
    int log = 0;
    while (n >>= 1)
        ++log;
    return log;
}

// Sorts [begin, end) using insertion sort with the given comparison function.
template <typename I, typename Comp, typename Proj>
constexpr void insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = value_type_t<I>;

    if (begin == end) {
        return;
    }

    for (I cur = begin + 1; cur != end; ++cur) {
        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (sift != begin &&
                     nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
        }
    }
}

// Sorts [begin, end) using insertion sort with the given comparison function.
// Assumes
// *(begin - 1) is an element smaller than or equal to any element in [begin,
// end).
template <typename I, typename Comp, typename Proj>
constexpr void unguarded_insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = value_type_t<I>;

    if (begin == end) {
        return;
    }

    for (I cur = begin + 1; cur != end; ++cur) {
        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
        }
    }
}

// Attempts to use insertion sort on [begin, end). Will return false if more
// than partial_insertion_sort_limit elements were moved, and abort sorting.
// Otherwise it will successfully sort and return true.
template <typename I, typename Comp, typename Proj>
constexpr bool partial_insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = value_type_t<I>;

    if (begin == end) {
        return true;
    }

    int limit = 0;
    for (I cur = begin + 1; cur != end; ++cur) {
        if (limit > pqdsort_partial_insertion_sort_limit) {
            return false;
        }

        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (sift != begin &&
                     nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
            limit += cur - sift;
        }
    }

    return true;
}

template <typename I, typename Comp, typename Proj>
constexpr void sort2(I a, I b, Comp& comp, Proj& proj)
{
    if (nano::invoke(comp, nano::invoke(proj, *b), nano::invoke(proj, *a))) {
        nano::iter_swap(a, b);
    }
}

// Sorts the elements *a, *b and *c using comparison function comp.
template <typename I, typename Comp, typename Proj>
constexpr void sort3(I a, I b, I c, Comp& comp, Proj& proj)
{
    sort2(a, b, comp, proj);
    sort2(b, c, comp, proj);
    sort2(a, b, comp, proj);
}

// Partitions [begin, end) around pivot *begin using comparison function comp.
// Elements equal to the pivot are put in the right-hand partition. Returns the
// position of the pivot after partitioning and whether the passed sequence
// already was correctly partitioned. Assumes the pivot is a median of at least
// 3 elements and that [begin, end) is at least insertion_sort_threshold long.
template <typename I, typename Comp, typename Proj>
constexpr std::pair<I, bool> partition_right(I begin, I end, Comp& comp,
                                             Proj& proj)
{
    using T = value_type_t<I>;

    // Move pivot into local for speed.
    T pivot(nano::iter_move(begin));

    I first = begin;
    I last = end;

    // Find the first element greater than or equal than the pivot (the median
    // of 3 guarantees this exists).
    while (nano::invoke(comp, nano::invoke(proj, *++first),
                        nano::invoke(proj, pivot))) {
    }

    // Find the first element strictly smaller than the pivot. We have to guard
    // this search if there was no element before *first.
    if (first - 1 == begin) {
        while (first < last && !nano::invoke(comp, nano::invoke(proj, *--last),
                                             nano::invoke(proj, pivot))) {
        }
    } else {
        while (!nano::invoke(comp, nano::invoke(proj, *--last),
                             nano::invoke(proj, pivot))) {
        }
    }

    // If the first pair of elements that should be swapped to partition are the
    // same element, the passed in sequence already was correctly partitioned.
    bool already_partitioned = first >= last;

    // Keep swapping pairs of elements that are on the wrong side of the pivot.
    // Previously swapped pairs guard the searches, which is why the first
    // iteration is special-cased above.
    while (first < last) {
        nano::iter_swap(first, last);
        while (nano::invoke(comp, nano::invoke(proj, *++first),
                            nano::invoke(proj, pivot)))
            ;
        while (!nano::invoke(comp, nano::invoke(proj, *--last),
                             nano::invoke(proj, pivot)))
            ;
    }

    // Put the pivot in the right place.
    I pivot_pos = first - 1;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return std::make_pair(std::move(pivot_pos), already_partitioned);
}

// Similar function to the one above, except elements equal to the pivot are put
// to the left of the pivot and it doesn't check or return if the passed
// sequence already was partitioned. Since this is rarely used (the many equal
// case), and in that case pdqsort already has O(n) performance, no block
// quicksort is applied here for simplicity.
template <typename I, typename Comp, typename Proj>
constexpr I partition_left(I begin, I end, Comp& comp, Proj& proj)
{
    using T = value_type_t<I>;

    T pivot(nano::iter_move(begin));
    I first = begin;
    I last = end;

    while (nano::invoke(comp, nano::invoke(proj, pivot),
                        nano::invoke(proj, *--last)))
        ;

    if (last + 1 == end) {
        while (first < last && !nano::invoke(comp, nano::invoke(proj, pivot),
                                             nano::invoke(proj, *++first)))
            ;
    } else {
        while (!nano::invoke(comp, nano::invoke(proj, pivot),
                             nano::invoke(proj, *++first)))
            ;
    }

    while (first < last) {
        nano::iter_swap(first, last);
        while (nano::invoke(comp, nano::invoke(proj, pivot),
                            nano::invoke(proj, *--last)))
            ;
        while (!nano::invoke(comp, nano::invoke(proj, pivot),
                             nano::invoke(proj, *++first)))
            ;
    }

    I pivot_pos = last;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return pivot_pos;
}

template <typename I, typename Comp, typename Proj>
constexpr void pdqsort_loop(I begin, I end, Comp& comp, Proj& proj,
                            int bad_allowed, bool leftmost = true)
{
    using diff_t = difference_type_t<I>;

    // Use a while loop for tail recursion elimination.
    while (true) {
        diff_t size = nano::distance(begin, end);

        // Insertion sort is faster for small arrays.
        if (size < pdqsort_insertion_sort_threshold) {
            if (leftmost) {
                insertion_sort(begin, end, comp, proj);
            } else {
                unguarded_insertion_sort(begin, end, comp, proj);
            }
            return;
        }

        // Choose pivot as median of 3 or pseudomedian of 9.
        diff_t s2 = size / 2;
        if (size > pdqsort_ninther_threshold) {
            sort3(begin, begin + s2, end - 1, comp, proj);
            sort3(begin + 1, begin + (s2 - 1), end - 2, comp, proj);
            sort3(begin + 2, begin + (s2 + 1), end - 3, comp, proj);
            sort3(begin + (s2 - 1), begin + s2, begin + (s2 + 1), comp, proj);
            nano::iter_swap(begin, begin + s2);
        } else {
            sort3(begin + s2, begin, end - 1, comp, proj);
        }

        // If *(begin - 1) is the end of the right partition of a previous
        // partition operation there is no element in [begin, end) that is
        // smaller than *(begin - 1). Then if our pivot compares equal to
        // *(begin - 1) we change strategy, putting equal elements in the left
        // partition, greater elements in the right partition. We do not have to
        // recurse on the left partition, since it's sorted (all equal).
        if (!leftmost && !nano::invoke(comp, nano::invoke(proj, *(begin - 1)),
                                       nano::invoke(proj, *begin))) {
            begin = partition_left(begin, end, comp, proj) + 1;
            continue;
        }

        // Partition and get results.
        std::pair<I, bool> part_result =
            partition_right(begin, end, comp, proj);
        I pivot_pos = part_result.first;
        bool already_partitioned = part_result.second;

        // Check for a highly unbalanced partition.
        diff_t l_size = pivot_pos - begin;
        diff_t r_size = end - (pivot_pos + 1);
        bool highly_unbalanced = l_size < size / 8 || r_size < size / 8;

        // If we got a highly unbalanced partition we shuffle elements to break
        // many patterns.
        if (highly_unbalanced) {
            // If we had too many bad partitions, switch to heapsort to
            // guarantee O(n log n).
            if (--bad_allowed == 0) {
                nano::make_heap(begin, end, comp, proj);
                nano::sort_heap(begin, end, comp, proj);
                return;
            }

            if (l_size >= pdqsort_insertion_sort_threshold) {
                nano::iter_swap(begin, begin + l_size / 4);
                nano::iter_swap(pivot_pos - 1, pivot_pos - l_size / 4);

                if (l_size > pdqsort_ninther_threshold) {
                    nano::iter_swap(begin + 1, begin + (l_size / 4 + 1));
                    nano::iter_swap(begin + 2, begin + (l_size / 4 + 2));
                    nano::iter_swap(pivot_pos - 2,
                                    pivot_pos - (l_size / 4 + 1));
                    nano::iter_swap(pivot_pos - 3,
                                    pivot_pos - (l_size / 4 + 2));
                }
            }

            if (r_size >= pdqsort_insertion_sort_threshold) {
                nano::iter_swap(pivot_pos + 1, pivot_pos + (1 + r_size / 4));
                nano::iter_swap(end - 1, end - r_size / 4);

                if (r_size > pdqsort_ninther_threshold) {
                    nano::iter_swap(pivot_pos + 2,
                                    pivot_pos + (2 + r_size / 4));
                    nano::iter_swap(pivot_pos + 3,
                                    pivot_pos + (3 + r_size / 4));
                    nano::iter_swap(end - 2, end - (1 + r_size / 4));
                    nano::iter_swap(end - 3, end - (2 + r_size / 4));
                }
            }
        } else {
            // If we were decently balanced and we tried to sort an already
            // partitioned sequence try to use insertion sort.
            if (already_partitioned &&
                partial_insertion_sort(begin, pivot_pos, comp, proj) &&
                partial_insertion_sort(pivot_pos + 1, end, comp, proj))
                return;
        }

        // Sort the left partition first using recursion and do tail recursion
        // elimination for the right-hand partition.
        detail::pdqsort_loop(begin, pivot_pos, comp, proj, bad_allowed,
                             leftmost);
        begin = pivot_pos + 1;
        leftmost = false;
    }
}

template <typename I, typename Comp, typename Proj>
constexpr void pdqsort(I begin, I end, Comp& comp, Proj& proj)
{
    if (begin == end) {
        return;
    }

    detail::pdqsort_loop(std::move(begin), std::move(end), comp, proj,
                         detail::log2(nano::distance(begin, end)));
}

} // namespace detail

NANO_END_NAMESPACE

#endif