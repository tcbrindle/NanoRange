// nanorange/detail/algorithm/heap_sift.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
#ifndef NANORANGE_DETAIL_ALGORITHM_HEAP_SIFT_HPP
#define NANORANGE_DETAIL_ALGORITHM_HEAP_SIFT_HPP

#include <nanorange/detail/iterator/associated_types.hpp>
#include <nanorange/detail/iterator/iter_move.hpp>
#include <nanorange/functional.hpp>

///////////////////////////////////////////////////////////////////////////
// detail::sift_up_n and detail::sift_down_n
// (heap implementation details)
//

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename I, typename Comp, typename Proj>
constexpr void sift_up_n(I first, iter_difference_t<I> n, Comp& comp,
                         Proj& proj)
{
    if (n > 1) {
        I last = first + n;
        n = (n - 2) / 2;
        I i = first + n;
        if (nano::invoke(comp, nano::invoke(proj, *i),
                         nano::invoke(proj, *--last))) {
            iter_value_t<I> v = nano::iter_move(last);
            do {
                *last = nano::iter_move(i);
                last = i;
                if (n == 0) {
                    break;
                }
                n = (n - 1) / 2;
                i = first + n;
            } while (nano::invoke(comp, nano::invoke(proj, *i),
                                  nano::invoke(proj, v)));
            *last = std::move(v);
        }
    }
}

template <typename I, typename Comp, typename Proj>
constexpr void sift_down_n(I first, iter_difference_t<I> n, I start, Comp& comp,
                           Proj& proj)
{
    // left-child of start is at 2 * start + 1
    // right-child of start is at 2 * start + 2
    auto child = start - first;

    if (n < 2 || (n - 2) / 2 < child) {
        return;
    }

    child = 2 * child + 1;
    I child_i = first + child;

    if ((child + 1) < n && nano::invoke(comp, nano::invoke(proj, *child_i),
                                        nano::invoke(proj, *(child_i + 1)))) {
        // right-child exists and is greater than left-child
        ++child_i;
        ++child;
    }

    // check if we are in heap-order
    if (nano::invoke(comp, nano::invoke(proj, *child_i),
                     nano::invoke(proj, *start))) {
        // we are, start is larger than its largest child
        return;
    }

    iter_value_t<I> top = nano::iter_move(start);
    do {
        // we are not in heap-order, swap the parent with it's largest child
        *start = nano::iter_move(child_i);
        start = child_i;

        if ((n - 2) / 2 < child) {
            break;
        }

        // recompute the child based off of the updated parent
        child = 2 * child + 1;
        child_i = first + child;

        if ((child + 1) < n &&
            nano::invoke(comp, nano::invoke(proj, *child_i),
                         nano::invoke(proj, *(child_i + 1)))) {
            // right-child exists and is greater than left-child
            ++child_i;
            ++child;
        }

        // check if we are in heap-order
    } while (!nano::invoke(comp, nano::invoke(proj, *child_i),
                           nano::invoke(proj, top)));
    *start = std::move(top);
}

} // namespace detail

NANO_END_NAMESPACE

#endif
