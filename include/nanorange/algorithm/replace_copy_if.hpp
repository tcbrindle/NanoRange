// nanorange/algorithm/replace_copy_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_COPY_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_COPY_IF_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using replace_copy_if_result = copy_result<I, O>;

namespace detail {

struct replace_copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename T,
              typename Proj>
    static constexpr replace_copy_if_result<I, O>
    impl(I first, S last, O result, Pred& pred, const T& new_value, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
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
    template <typename I, typename S, typename O, typename Pred, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            output_iterator<O, const T&> && indirectly_copyable<I, O> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>,
        replace_copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_if_fn::impl(std::move(first), std::move(last),
                                        std::move(result), pred, new_value,
                                        proj);
    }

    template <typename Rng, typename O, typename Pred, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && output_iterator<O, const T&> &&
            indirectly_copyable<iterator_t<Rng>, O> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        replace_copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                        std::move(result), pred, new_value,
                                        proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_copy_if_fn, replace_copy_if)

NANO_END_NAMESPACE

#endif
