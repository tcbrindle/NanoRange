// nanorange/algorithm/replace_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_COPY_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using replace_copy_result = copy_result<I, O>;

namespace detail {

struct replace_copy_fn {
private:
    template <typename I, typename S, typename O, typename T1, typename T2,
              typename Proj>
    static constexpr replace_copy_result<I, O>
    impl(I first, S last, O result, const T1& old_value, const T2& new_value,
         Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(proj, *first) == old_value) {
                *result = new_value;
            } else {
                *result = *first;
            }
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            output_iterator<O, const T2&> && indirectly_copyable<I, O> &&
            indirect_relation<ranges::equal_to, projected<I, Proj>, const T1*>,
        replace_copy_result<I, O>>
    operator()(I first, S last, O result, const T1& old_value,
               const T2& new_value, Proj proj = Proj{}) const
    {
        return replace_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result), old_value, new_value,
                                     proj);
    }

    template <typename Rng, typename O, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && output_iterator<O, const T2&> &&
            indirectly_copyable<iterator_t<Rng>, O> &&
            indirect_relation<ranges::equal_to, projected<iterator_t<Rng>, Proj>,
                             const T1*>,
        replace_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                     std::move(result), old_value, new_value,
                                     proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_copy_fn, replace_copy)

NANO_END_NAMESPACE

#endif
