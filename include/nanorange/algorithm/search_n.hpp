// nanorange/algorithm/search_n.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SEARCH_N_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SEARCH_N_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE


namespace detail {

struct search_n_fn {
private:
    template <typename I, typename S, typename T, typename Pred, typename Proj>
    static constexpr subrange<I>
    impl(I first, S last, iter_difference_t<I> count, const T& value, Pred pred,
         Proj& proj)
    {
        if (count == iter_difference_t<I>{0}) {
            return {first, first};
        }

        for (; first != last; ++first) {
            if (!nano::invoke(pred, nano::invoke(proj, *first), value)) {
                continue;
            }

            I save = first;
            iter_difference_t<I> running_count{1};

            while (true) {
                if (running_count++ == count) {
                    // Success
                    return {save, nano::next(first)};
                }

                if (++first == last) {
                    // We have run out of elements
                    return {first, first};
                }

                if (!nano::invoke(pred, nano::invoke(proj, *first), value)) {
                    break;
                }
            }
        }

        return {first, first};
    }

public:
    template <typename I, typename S, typename T, typename Pred = ranges::equal_to,
        typename Proj = identity>
    constexpr auto operator()(I first, S last, iter_difference_t<I> count,
                              const T& value, Pred pred = Pred{},
                              Proj proj = Proj{}) const
    -> std::enable_if_t<forward_iterator<I> && sentinel_for<S, I> &&
                                indirectly_comparable<I, const T*, Pred, Proj>,
        subrange<I>>
    {
        return search_n_fn::impl(std::move(first), std::move(last), count,
                                 value, pred, proj);
    }

    template <typename Rng, typename T, typename Pred = ranges::equal_to,
        typename Proj = identity>
    constexpr auto
    operator()(Rng&& rng, iter_difference_t<iterator_t<Rng>> count,
               const T& value, Pred pred = Pred{}, Proj proj = Proj{}) const
    -> std::enable_if_t<
            forward_range<Rng> &&
                indirectly_comparable<iterator_t<Rng>, const T*, Pred, Proj>,
        safe_subrange_t<Rng>>
    {
        return search_n_fn::impl(nano::begin(rng), nano::end(rng), count, value, pred,
                                 proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::search_n_fn, search_n)

NANO_END_NAMESPACE

#endif
