// nanorange/algorithm/rotate.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/view/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct rotate_fn {
private:
    template <typename I, typename S>
    static constexpr subrange<I> impl(I first, I middle, S last)
    {
        if (first == middle) {
            auto ret = next(first, last);
            return {ret, ret};
        }
        if (middle == last) {
            return {first, middle};
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

public:
    template <typename I, typename S>
    constexpr detail::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        Permutable<I>,
        subrange<I>>
    operator()(I first, I middle, S last) const
    {
        return rotate_fn::impl(std::move(first), std::move(middle), std::move(last));
    }

    template <typename Rng>
    constexpr detail::enable_if_t<
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
