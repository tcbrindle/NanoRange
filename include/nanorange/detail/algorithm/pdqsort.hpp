// nanorange/detail/algorithm/pqdsort.hpp
//
// Copyright Orson Peters 2017.
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Modified from Boost.Sort by Orson Peters
// https://github.com/boostorg/sort/blob/develop/include/boost/sort/pdqsort/pdqsort.hpp

#ifndef NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED
#define NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED

#include <nanorange/algorithm/make_heap.hpp>
#include <nanorange/algorithm/min.hpp>
#include <nanorange/algorithm/sort_heap.hpp>
#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// Partitions below this size are sorted using insertion sort.
constexpr int pdqsort_insertion_sort_threshold = 24;

// Partitions above this size use Tukey's ninther to select the pivot.
constexpr int pdqsort_ninther_threshold = 128;

// When we detect an already sorted partition, attempt an insertion sort that
// allows this amount of element moves before giving up.
constexpr int pqdsort_partial_insertion_sort_limit = 8;

// Must be multiple of 8 due to loop unrolling, and < 256 to fit in unsigned
// char.
constexpr int pdqsort_block_size = 64;

// Cacheline size, assumes power of two.
constexpr int pdqsort_cacheline_size = 64;

template <typename>
struct is_default_compare : std::false_type {};
template <>
struct is_default_compare<nano::less> : std::true_type {};
template <>
struct is_default_compare<nano::greater> : std::true_type {};
template <typename T>
struct is_default_compare<std::less<T>> : std::true_type {};
template <typename T>
struct is_default_compare<std::greater<T>> : std::true_type {};

template <typename T>
constexpr bool is_default_compare_v = is_default_compare<T>::value;

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
    using T = iter_value_t<I>;

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
    using T = iter_value_t<I>;

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
    using T = iter_value_t<I>;

    if (begin == end) {
        return true;
    }

    iter_difference_t<I> limit = 0;
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

template <typename I>
constexpr void swap_offsets(I first, I last, unsigned char* offsets_l,
                            unsigned char* offsets_r, int num, bool use_swaps)
{
    using T = iter_value_t<I>;
    if (use_swaps) {
        // This case is needed for the descending distribution, where we need
        // to have proper swapping for pdqsort to remain O(n).
        for (int i = 0; i < num; ++i) {
            nano::iter_swap(first + offsets_l[i], last - offsets_r[i]);
        }
    } else if (num > 0) {
        I l = first + offsets_l[0];
        I r = last - offsets_r[0];
        T tmp(nano::iter_move(l));
        *l = nano::iter_move(r);

        for (int i = 1; i < num; ++i) {
            l = first + offsets_l[i];
            *r = nano::iter_move(l);
            r = last - offsets_r[i];
            *l = nano::iter_move(r);
        }
        *r = std::move(tmp);
    }
}

// Partitions [begin, end) around pivot *begin using comparison function comp.
// Elements equal to the pivot are put in the right-hand partition. Returns the
// position of the pivot after partitioning and whether the passed sequence
// already was correctly partitioned. Assumes the pivot is a median of at least
// 3 elements and that [begin, end) is at least insertion_sort_threshold long.
// Uses branchless partitioning.
template <typename I, typename Comp, typename Pred>
constexpr std::pair<I, bool> partition_right_branchless(I begin, I end,
                                                        Comp& comp, Pred& pred)
{
    using T = iter_value_t<I>;

    // Move pivot into local for speed.
    T pivot(nano::iter_move(begin));
    I first = begin;
    I last = end;

    // Find the first element greater than or equal than the pivot (the median
    // of 3 guarantees this exists).
    while (nano::invoke(comp, nano::invoke(pred, *++first),
                        nano::invoke(pred, pivot)))
        ;

    // Find the first element strictly smaller than the pivot. We have to guard
    // this search if there was no element before *first.
    if (first - 1 == begin) {
        while (first < last && !nano::invoke(comp, nano::invoke(pred, *--last),
                                             nano::invoke(pred, pivot)))
            ;
    } else {
        while (!nano::invoke(comp, nano::invoke(pred, *--last),
                             nano::invoke(pred, pivot)))
            ;
    }

    // If the first pair of elements that should be swapped to partition are the
    // same element, the passed in sequence already was correctly partitioned.
    bool already_partitioned = first >= last;
    if (!already_partitioned) {
        nano::iter_swap(first, last);
        ++first;
    }

    // The following branchless partitioning is derived from "BlockQuicksort:
    // How Branch Mispredictions don't affect Quicksort" by Stefan Edelkamp and
    // Armin Weiss.
    alignas(pdqsort_cacheline_size) unsigned char
        offsets_l_storage[pdqsort_block_size] = {};
    alignas(pdqsort_cacheline_size) unsigned char
        offsets_r_storage[pdqsort_block_size] = {};
    unsigned char* offsets_l = offsets_l_storage;
    unsigned char* offsets_r = offsets_r_storage;
    int num_l = 0, num_r = 0, start_l = 0, start_r = 0;

    while (last - first > 2 * pdqsort_block_size) {
        // Fill up offset blocks with elements that are on the wrong side.
        if (num_l == 0) {
            start_l = 0;
            I it = first;
            for (unsigned char i = 0; i < pdqsort_block_size;) {
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
            }
        }
        if (num_r == 0) {
            start_r = 0;
            I it = last;
            for (unsigned char i = 0; i < pdqsort_block_size;) {
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
            }
        }

        // Swap elements and update block sizes and first/last boundaries.
        int num = (nano::min)(num_l, num_r);
        swap_offsets(first, last, offsets_l + start_l, offsets_r + start_r, num,
                     num_l == num_r);
        num_l -= num;
        num_r -= num;
        start_l += num;
        start_r += num;
        if (num_l == 0)
            first += pdqsort_block_size;
        if (num_r == 0)
            last -= pdqsort_block_size;
    }

    iter_difference_t<I> l_size = 0, r_size = 0;
    iter_difference_t<I> unknown_left =
        (last - first) - ((num_r || num_l) ? pdqsort_block_size : 0);
    if (num_r) {
        // Handle leftover block by assigning the unknown elements to the other
        // block.
        l_size = unknown_left;
        r_size = pdqsort_block_size;
    } else if (num_l) {
        l_size = pdqsort_block_size;
        r_size = unknown_left;
    } else {
        // No leftover block, split the unknown elements in two blocks.
        l_size = unknown_left / 2;
        r_size = unknown_left - l_size;
    }

    // Fill offset buffers if needed.
    if (unknown_left && !num_l) {
        start_l = 0;
        I it = first;
        for (unsigned char i = 0; i < l_size;) {
            offsets_l[num_l] = i++;
            num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                   nano::invoke(pred, pivot));
            ++it;
        }
    }
    if (unknown_left && !num_r) {
        start_r = 0;
        I it = last;
        for (unsigned char i = 0; i < r_size;) {
            offsets_r[num_r] = ++i;
            num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                  nano::invoke(pred, pivot));
        }
    }

    int num = (nano::min)(num_l, num_r);
    swap_offsets(first, last, offsets_l + start_l, offsets_r + start_r, num,
                 num_l == num_r);
    num_l -= num;
    num_r -= num;
    start_l += num;
    start_r += num;
    if (num_l == 0)
        first += l_size;
    if (num_r == 0)
        last -= r_size;

    // We have now fully identified [first, last)'s proper position. Swap the
    // last elements.
    if (num_l) {
        offsets_l += start_l;
        while (num_l--)
            nano::iter_swap(first + offsets_l[num_l], --last);
        first = last;
    }
    if (num_r) {
        offsets_r += start_r;
        while (num_r--)
            nano::iter_swap(last - offsets_r[num_r], first), ++first;
        last = first;
    }

    // Put the pivot in the right place.
    I pivot_pos = first - 1;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return std::make_pair(std::move(pivot_pos), already_partitioned);
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
    using T = iter_value_t<I>;

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
    using T = iter_value_t<I>;

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

template <bool Branchless, typename I, typename Comp, typename Proj>
constexpr void pdqsort_loop(I begin, I end, Comp& comp, Proj& proj,
                            int bad_allowed, bool leftmost = true)
{
    using diff_t = iter_difference_t<I>;

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
            Branchless ? partition_right_branchless(begin, end, comp, proj)
                       : partition_right(begin, end, comp, proj);
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
        detail::pdqsort_loop<Branchless>(begin, pivot_pos, comp, proj,
                                         bad_allowed, leftmost);
        begin = pivot_pos + 1;
        leftmost = false;
    }
}

template <typename I, typename Comp, typename Proj,
          bool Branchless = is_default_compare_v<std::remove_const_t<Comp>>&&
              same_as<Proj, identity>&& std::is_arithmetic<iter_value_t<I>>::value>
constexpr void pdqsort(I begin, I end, Comp& comp, Proj& proj)
{
    if (begin == end) {
        return;
    }

    detail::pdqsort_loop<Branchless>(std::move(begin), std::move(end), comp,
                                     proj,
                                     detail::log2(nano::distance(begin, end)));
}

} // namespace detail

NANO_END_NAMESPACE

#endif