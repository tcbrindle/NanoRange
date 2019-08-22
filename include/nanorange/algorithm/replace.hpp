// nanorange/algorithm/replace.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct replace_fn {
private:
    template <typename I, typename S, typename T1, typename T2, typename Proj>
    static constexpr I impl(I first, S last, const T1& old_value,
                            const T2& new_value, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(proj, *first) == old_value) {
                *first = new_value;
            }
            ++first;
        }

        return first;
    }

public:
    template <typename I, typename S, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> && writable<I, const T2&> &&
            indirect_relation<ranges::equal_to, projected<I, Proj>, const T1*>,
        I>
    operator()(I first, S last, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_fn::impl(std::move(first), std::move(last), old_value,
                                new_value, proj);
    }

    template <typename Rng, typename T1, typename T2, typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && writable<iterator_t<Rng>, const T2&> &&
            indirect_relation<ranges::equal_to, projected<iterator_t<Rng>, Proj>,
                             const T1*>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_fn::impl(nano::begin(rng), nano::end(rng), old_value,
                                new_value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_fn, replace)

NANO_END_NAMESPACE

#endif
