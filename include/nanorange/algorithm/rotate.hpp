// nanorange/algorithm/rotate.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED

#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/swap_ranges.hpp>
#include <nanorange/iterator/unreachable.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct rotate_fn {
private:
    template <typename I, typename S>
    static constexpr subrange<I> do_rotate_one_left(I first, S last)
    {
        // Stash the first element and move everything one place
        iter_value_t<I> val = nano::iter_move(first);
        auto ret = nano::move(nano::next(first), std::move(last), first);
        *ret.out = std::move(val);
        return {std::move(ret.out), std::move(ret.in)};
    }

    template <typename I>
    static constexpr subrange<I> do_rotate_one_right(I first, I middle)
    {
        I last = nano::next(middle);
        iter_value_t<I> val = nano::iter_move(middle);
        nano::move_backward(first, middle, last);
        *first = std::move(val);
        return {std::move(++first), std::move(last)};
    }

    template <typename I, typename S>
    static constexpr std::enable_if_t<
        random_access_iterator<I> && sized_sentinel_for<S, I>,
            subrange<I>>
    do_rotate(I first, I middle, S last, priority_tag<2>)
    {
        constexpr bool is_tma = std::is_trivially_move_assignable<iter_value_t<I>>::value;

        auto i = nano::distance(first, middle);
        auto j = nano::distance(first, last) - i;
        I out = first + (last - middle);

        while (i != j) {
            if (i > j) {
                if (is_tma && j == 1) {
                    do_rotate_one_right(middle - i, middle);
                    return {std::move(out), nano::next(first, last)};
                }
                nano::swap_ranges(middle - i, unreachable_sentinel, middle, middle + j);
                i -= j;
            } else {
                if (is_tma && i == 1) {
                    do_rotate_one_left(middle - i, middle + j);
                    return {std::move(out), nano::next(first, last)};
                }
                nano::swap_ranges(middle - i, middle, middle + j - i, unreachable_sentinel);
                j -= i;
            }
        }
        nano::swap_ranges(middle  - i, middle, middle, unreachable_sentinel);

        return {std::move(out), nano::next(first, last)};
    }

    template <typename I, typename S>
    static constexpr std::enable_if_t<bidirectional_iterator<I>, subrange<I>>
    do_rotate(I first, I middle, S last, priority_tag<1>)
    {
        if (std::is_trivially_move_assignable<iter_value_t<I>>::value &&
                nano::next(middle) == last) {
            return do_rotate_one_right(std::move(first), std::move(middle));
        }

        return do_rotate(std::move(first), std::move(middle), std::move(last),
                         priority_tag<0>{});
    }

    template <typename I, typename S>
    static constexpr subrange<I>
    do_rotate(I first, I middle, S last, priority_tag<0>)
    {
        if (std::is_trivially_move_assignable<iter_value_t<I>>::value &&
                nano::next(first) == middle) {
            return do_rotate_one_left(std::move(first), std::move(last));
        }

        if (sized_sentinel_for<I, I> && sized_sentinel_for<S, I> &&
            nano::distance(first, middle) == nano::distance(middle, last))
        {
            auto ret = nano::swap_ranges(first, middle, middle, unreachable_sentinel);
            return {std::move(ret.in1), std::move(ret.in2)};
        }

        I next = middle;

        do {
            nano::iter_swap(first++, next++);
            if (first == middle) {
                middle = next;
            }
        } while (next != last);

        I ret = first;
        next = middle;

        while (next != last) {
            nano::iter_swap(first++, next++);
            if (first == middle) {
                middle = next;
            } else if (next == last) {
                next = middle;
            }
        }

        return {std::move(ret), std::move(next)};
    }

    template <typename I, typename S>
    static constexpr subrange<I> impl(I first, I middle, S last)
    {
        if (first == middle) {
            auto ret = nano::next(first, last);
            return {ret, ret};
        }
        if (middle == last) {
            return {first, middle};
        }

        return do_rotate(std::move(first), std::move(middle), std::move(last),
                         priority_tag<2>{});
    }

public:
    template <typename I, typename S>
    constexpr std::enable_if_t<
        forward_iterator<I> && sentinel_for<S, I> && permutable<I>,
        subrange<I>>
    operator()(I first, I middle, S last) const
    {
        return rotate_fn::impl(std::move(first), std::move(middle), std::move(last));
    }

    template <typename Rng>
    constexpr std::enable_if_t<
        forward_range<Rng> && permutable<iterator_t<Rng>>,
        safe_subrange_t<Rng>>
    operator()(Rng&& rng, iterator_t<Rng> middle) const
    {
        return rotate_fn::impl(nano::begin(rng), std::move(middle), nano::end(rng));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::rotate_fn, rotate)

NANO_END_NAMESPACE

#endif
