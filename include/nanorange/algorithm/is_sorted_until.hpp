// nanorange/algorithm/is_sorted_until.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_SORTED_UNTIL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_SORTED_UNTIL_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct is_sorted_until_fn {
private:
    friend struct is_sorted_fn;

    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I n = next(first);

        while (n != last) {
            if (nano::invoke(comp, nano::invoke(proj, *n),
                              nano::invoke(proj, *first))) {
                return n;
            }
            ++first;
            ++n;
        }

        return n;
    }

public:
    template <typename I, typename S, typename Comp = ranges::less,
              typename Proj = identity>
    constexpr std::enable_if_t<
        forward_iterator<I> && sentinel_for<S, I> &&
            indirect_strict_weak_order<Comp, projected<I, Proj>>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(std::move(first), std::move(last),
                                        comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_range<Rng> &&
            indirect_strict_weak_order<Comp, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(nano::begin(rng), nano::end(rng),
                                        comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::is_sorted_until_fn, is_sorted_until)

NANO_END_NAMESPACE

#endif
