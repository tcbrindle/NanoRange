// nanorange/algorithm/is_heap_until.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
//
// Copyright Eric Niebler 2014
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_IS_HEAP_UNTIL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_HEAP_UNTIL_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_heap_until_fn {
private:
    friend struct is_heap_fn;

    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, const iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        iter_difference_t<I> p = 0, c = 1;

        I pp = first;

        while (c < n) {
            I cp = first + c;

            if (nano::invoke(comp, nano::invoke(proj, *pp),
                             nano::invoke(proj, *cp))) {
                return cp;
            }

            ++c;
            ++cp;

            if (c == n || nano::invoke(comp, nano::invoke(proj, *pp),
                                       nano::invoke(proj, *cp))) {
                return cp;
            }

            ++p;
            ++pp;

            c = 2 * p + 1;
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = ranges::less,
              typename Proj = identity>
    constexpr std::enable_if_t<
        random_access_iterator<I> && sentinel_for<S, I> &&
            indirect_strict_weak_order<Comp, projected<I, Proj>>,
        I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        auto n = nano::distance(first, last);
        return is_heap_until_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<
        random_access_range<Rng> &&
        indirect_strict_weak_order<Comp, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_heap_until_fn::impl(nano::begin(rng), nano::distance(rng),
                                      comp, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_heap_until_fn, is_heap_until)

NANO_END_NAMESPACE

#endif
