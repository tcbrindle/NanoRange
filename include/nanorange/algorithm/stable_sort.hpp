// nanorange/algorithm/stable_sort.hpp
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
//  Copyright (c) 1994
//  Hewlett-Packard Company
//
//  Permission to use, copy, modify, distribute and sell this software
//  and its documentation for any purpose is hereby granted without fee,
//  provided that the above copyright notice appear in all copies and
//  that both that copyright notice and this permission notice appear
//  in supporting documentation.  Hewlett-Packard Company makes no
//  representations about the suitability of this software for any
//  purpose.  It is provided "as is" without express or implied warranty.
//
//  Copyright (c) 1996
//  Silicon Graphics Computer Systems, Inc.
//
//  Permission to use, copy, modify, distribute and sell this software
//  and its documentation for any purpose is hereby granted without fee,
//  provided that the above copyright notice appear in all copies and
//  that both that copyright notice and this permission notice appear
//  in supporting documentation.  Silicon Graphics makes no
//  representations about the suitability of this software for any
//  purpose.  It is provided "as is" without express or implied warranty.


#ifndef NANORANGE_ALGORITHM_STABLE_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STABLE_SORT_HPP_INCLUDED

#include <nanorange/algorithm/inplace_merge.hpp>
#include <nanorange/detail/algorithm/pdqsort.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_sort_fn {
private:
    static constexpr int merge_sort_chunk_size = 7;

    template <typename I, typename Comp, typename Proj>
    static void inplace_stable_sort(I first, I last, Comp& comp, Proj& proj)
    {
        if (last - first < 15) {
            detail::insertion_sort(std::move(first), std::move(last), comp, proj);
        } else {
            I middle = first + iter_difference_t<I>(last - first) / 2;
            inplace_stable_sort(first, middle, comp, proj);
            inplace_stable_sort(middle, last, comp, proj);
            inplace_merge_fn::impl_slow(first, middle, last,
                                        middle - first, last - middle,
                                        comp, proj);
        }
    }

    template <typename I, typename Buf, typename Comp, typename Proj>
    static void stable_sort_adaptive(I first, I last, Buf& buf, Comp& comp, Proj& proj)
    {
        auto len = iter_difference_t<I>((last - first + 1) / 2);
        auto middle = first + len;
        if (static_cast<std::size_t>(len) > buf.capacity()) {
            stable_sort_adaptive(first, middle, buf, comp, proj);
            stable_sort_adaptive(middle, last, buf, comp, proj);
        } else {
            merge_sort_with_buffer(first, middle, buf, comp, proj);
            merge_sort_with_buffer(middle, last, buf, comp, proj);
        }
        inplace_merge_fn::impl_buffered(first, middle, last,
                                        middle - first, last - middle, buf,
                                        comp, proj);
    }

    template <typename I, typename Buf, typename Comp, typename Proj>
    static void merge_sort_with_buffer(I first, I last, Buf& buf, Comp& comp, Proj& proj)
    {
        auto len = iter_difference_t<I>(last - first);
        auto step_size = iter_difference_t<I>(merge_sort_chunk_size);
        chunk_insertion_sort(first, last, step_size, comp, proj);
        if (step_size >= len) {
            return;
        }
        assert(buf.empty());
        merge_sort_loop(first, last, nano::back_inserter(buf), step_size, comp, proj);
        step_size *= 2;
        while (true) {
            merge_sort_loop(buf.begin(), buf.end(), first, step_size, comp, proj);
            step_size *= 2;
            if (step_size >= len) {
                buf.clear();
                return;
            }
            merge_sort_loop(first, last, buf.begin(), step_size, comp, proj);
            step_size *= 2;
        }
        buf.clear();
    }

    template <typename I, typename Comp, typename Proj>
    static void chunk_insertion_sort(I first, I last, iter_difference_t<I> chunk_size,
                                     Comp& comp, Proj& proj)
    {
            while (last - first >= chunk_size) {
                detail::insertion_sort(first, first + chunk_size, comp, proj);
                first += chunk_size;
            }
            detail::insertion_sort(first, last, comp, proj);
    }

    template <typename I, typename O, typename Comp, typename Proj>
    static void merge_sort_loop(I first, I last, O result, iter_difference_t<I> step_size,
                                Comp& comp, Proj& proj)
    {
        auto two_step = iter_difference_t<I>(2 * step_size);
        while (last - first >= two_step) {
            result = nano::merge(
                         nano::make_move_iterator(first),
                         nano::make_move_iterator(first + step_size),
                         nano::make_move_iterator(first + step_size),
                         nano::make_move_iterator(first + two_step),
                         result, std::ref(comp), std::ref(proj), std::ref(proj)).out;
            first += two_step;
        }
        step_size = nano::min(iter_difference_t<I>(last - first), step_size);
        nano::merge(
                nano::make_move_iterator(first),
                nano::make_move_iterator(first + step_size),
                nano::make_move_iterator(first + step_size),
                nano::make_move_iterator(last),
                result, std::ref(comp), std::ref(proj), std::ref(proj));
    }

    template <typename I, typename Comp, typename Proj>
    static void impl(I first, I last, Comp& comp, Proj& proj)
    {
        auto len = last - first;
        if (len == 0) {
            return;
        }

        temporary_vector<iter_value_t<I>> buf(len > 256 ? len : 0);

        if (buf.capacity() != 0) {
            stable_sort_adaptive(std::move(first), std::move(last), buf, comp, proj);
        } else {
            inplace_stable_sort(std::move(first), std::move(last), comp, proj);
        }
    }

public:
    template <typename I, typename S, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<random_access_iterator<I> && sentinel_for<S, I> &&
                         sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto ilast = nano::next(first, last);
        impl(std::move(first), ilast, comp, proj);
        return ilast;
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    std::enable_if_t<random_access_range<Rng> &&
                         sortable<iterator_t<Rng>, Comp, Proj>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        auto first = nano::begin(rng);
        const auto last = nano::next(first, nano::end(rng));
        impl(std::move(first), last, comp, proj);
        return last;
    }
};

}

NANO_INLINE_VAR(detail::stable_sort_fn, stable_sort)

NANO_END_NAMESPACE

#endif
