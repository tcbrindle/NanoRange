// nanorange/algorithm/stl/stable_partition.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_STABLE_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STABLE_PARTITION_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>
#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/partition_copy.hpp>
#include <nanorange/algorithm/rotate.hpp>
#include <nanorange/iterator/back_insert_iterator.hpp>
#include <nanorange/iterator/move_iterator.hpp>
#include <nanorange/iterator/reverse_iterator.hpp>
#include <nanorange/views/subrange.hpp>

#include <nanorange/detail/memory/temporary_vector.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_partition_fn {
private:
    template <typename I, typename Buf, typename Pred, typename Proj>
    static subrange<I> impl_buffered(I first, I last, Buf& buf, Pred& pred, Proj& proj)
    {
        // first is known to be false, so pop it straight into the buffer
        buf.push_back(nano::iter_move(first));

        const auto res = nano::partition_copy(
                nano::make_move_iterator(nano::next(first)),
                nano::make_move_sentinel(--last),
                first, nano::back_inserter(buf),
                std::ref(pred), std::ref(proj));

        // last is known to be true, move that to the correct pos
        first = std::move(res.out1);
        *first = nano::iter_move(last);
        ++first;

        // Now move all the other elements from the buffer back into the sequence
        nano::move(buf, first);
        return {std::move(first), std::move(last)};
    }

    // Note to self: this is a closed range, last is NOT past-the-end!
    template <typename I, typename Pred, typename Proj>
    static subrange<I> impl_unbuffered(I first, I last, iter_difference_t<I> dist,
                             Pred& pred, Proj& proj)
    {
        using dist_t = iter_difference_t<I>;

        if (dist == 2) {
            // We know first is false and last is true, so swap them
            nano::iter_swap(first, last);
            return {nano::next(first), nano::next(last)};
        }

        if (dist == 3) {
            // We know first is false and last is true, so look at middle
            I middle = nano::next(first);

            if (nano::invoke(pred, nano::invoke(proj, *middle))) {
                nano::iter_swap(first, middle);
                nano::iter_swap(middle, last);
                return {nano::next(first, 2), nano::next(last)};
            }

            // middle is false
            nano::iter_swap(middle, last);
            nano::iter_swap(first, middle);
            return {std::move(middle), std::next(last)};
        }

        const dist_t half = dist/2;
        const I middle = nano::next(first, half);

        I m1 = nano::prev(middle);
        dist_t len_half = half;

        while (m1 != first && !nano::invoke(pred, nano::invoke(proj, *m1))) {
            --len_half;
            --m1;
        }

        const I first_false = (m1 == first) ? first :
                impl_unbuffered(first, m1, len_half, pred, proj).begin();

        m1 = middle;
        len_half = dist - half;

        while (nano::invoke(pred, nano::invoke(proj, *m1))) {
            if (++m1 == last) {
                auto rot = nano::rotate(first_false, middle, ++last);
                return {std::move(rot.begin()), nano::next(last)};
            }
        }

        const I last_false = impl_unbuffered(m1, last, len_half, pred, proj).begin();

        auto rot = nano::rotate(first_false, middle, last_false);
        return {rot.begin(), nano::next(last)};
    }

    template <typename I, typename Pred, typename Proj>
    static subrange<I> impl(I first, I last, Pred& pred, Proj& proj)
    {
        // Find the first non-true value
        first = nano::find_if_not(std::move(first), last, std::ref(pred), std::ref(proj));
        if (first == last) {
            return {std::move(first), std::move(last)};
        }

        // Find the last true value
        I it = nano::find_if(nano::make_reverse_iterator(last),
                             nano::make_reverse_iterator(first),
                             std::ref(pred), std::ref(proj)).base();
        if (it == first) {
            return {std::move(first), std::move(it)};
        }

        const auto dist = nano::distance(first, it);

        auto buf = detail::temporary_vector<iter_value_t<I>>(dist);
        if (buf.capacity() < static_cast<std::size_t>(dist)) {
            return {impl_unbuffered(first, --it, dist, pred, proj).begin(), last};
        }
        return {impl_buffered(first, it, buf, pred, proj).begin(), last};
    }

    template <typename I, typename S, typename Pred, typename Proj>
    static std::enable_if_t<!same_as<I, S>, subrange<I>>
    impl(I first, S last, Pred& pred, Proj& proj)
    {
        return impl(first, nano::next(first, last), pred, proj);
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    [[nodiscard]] std::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I> &&
                         indirect_unary_predicate<Pred, projected<I, Proj>> &&
                         permutable<I>, subrange<I>>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return stable_partition_fn::impl(std::move(first), std::move(last),
                                         pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    [[nodiscard]] std::enable_if_t<
        bidirectional_range<Rng> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>> &&
            permutable<iterator_t<Rng>>,
        borrowed_subrange_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return stable_partition_fn::impl(nano::begin(rng), nano::end(rng),
                                         pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::stable_partition_fn, stable_partition)

NANO_END_NAMESPACE

#endif
