// nanorange/algorithm/unique.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED

#include <nanorange/ranges.hpp>

#include <nanorange/algorithm/adjacent_find.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct unique_fn {
private:
    template <typename I, typename S, typename R, typename Proj>
    static constexpr subrange<I> impl(I first, S last, R& comp, Proj& proj)
    {
        I it = adjacent_find_fn::impl(first, last, comp, proj);

        if (it == last) {
            return {it, std::move(it)};
        }

        I n = nano::next(it, 2, last);
        for (; n != last; ++n) {
            if (!nano::invoke(comp, nano::invoke(proj, *it),
                              nano::invoke(proj, *n))) {
                *++it = iter_move(n);
            }
        }

        return {nano::next(it), std::move(n)};
    }

public:
    template <typename I, typename S, typename R = ranges::equal_to,
              typename Proj = identity>
    [[nodiscard]] constexpr std::enable_if_t<forward_iterator<I> && sentinel_for<S, I> &&
                                   indirect_relation<R, projected<I, Proj>> &&
                                   permutable<I>, subrange<I>>
    operator()(I first, S last, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(std::move(first), std::move(last),
                               comp, proj);
    }

    template <typename Rng, typename R = ranges::equal_to, typename Proj = identity>
    [[nodiscard]] constexpr std::enable_if_t<
        forward_range<Rng> &&
            indirect_relation<R, projected<iterator_t<Rng>, Proj>> &&
            permutable<iterator_t<Rng>>,
        borrowed_subrange_t<Rng>>
    operator()(Rng&& rng, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(nano::begin(rng), nano::end(rng),
                               comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::unique_fn, unique)

NANO_END_NAMESPACE

#endif
