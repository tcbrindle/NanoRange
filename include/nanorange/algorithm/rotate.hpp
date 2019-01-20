// nanorange/algorithm/rotate.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED

#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/swap_ranges.hpp>
#include <nanorange/view/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct rotate_fn {
private:
    template <typename I, typename S>
    static constexpr subrange<I> impl_one_left(I first, S last)
    {
        // Stash the first element and move everything one place
        iter_value_t<I> val = nano::iter_move(first);
        auto ret = nano::move(nano::next(first), std::move(last), first);
        *ret.out = std::move(val);
        return {std::move(ret.out), std::move(ret.in)};
    }

    template <typename I>
    static constexpr subrange<I> impl_one_right(I first, I middle)
    {
        I last = nano::next(middle);
        iter_value_t<I> val = nano::iter_move(middle);
        nano::move_backward(first, middle, last);
        *first = std::move(val);
        return {std::move(++first), std::move(last)};
    }

    template <typename I, typename S>
    static constexpr subrange<I> impl_general(I first, I middle, S last, std::false_type)
    {
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
    static constexpr subrange<I> impl_general(I first, I middle, S last, std::true_type /* isBiDir*/)
    {
        if (std::is_trivially_move_assignable<iter_value_t<I>>::value &&
            nano::next(middle) == last) {
            return impl_one_right(std::move(first), std::move(middle));
        }
        return impl_general(std::move(first), std::move(middle), std::move(last),
                            std::false_type{});
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

        if (std::is_trivially_move_assignable<iter_value_t<I>>::value &&
            nano::next(first) == middle) {
            return impl_one_left(std::move(first), std::move(last));
        }

        if (SizedSentinel<I, I> && SizedSentinel<S, I> &&
            nano::distance(first, middle) == nano::distance(middle, last))
        {
            auto ret = swap_ranges_fn::impl3(first, middle, middle);
            return {std::move(ret.in1), std::move(ret.in2)};
        }

        using is_bidir_t = std::conditional_t<BidirectionalIterator<I>,
                std::true_type, std::false_type>;

        return impl_general(std::move(first), std::move(middle), std::move(last),
                            is_bidir_t{});
    }

public:
    template <typename I, typename S>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        Permutable<I>,
        subrange<I>>
    operator()(I first, I middle, S last) const
    {
        return rotate_fn::impl(std::move(first), std::move(middle), std::move(last));
    }

    template <typename Rng>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        Permutable<iterator_t<Rng>>,
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
