// nanorange/algorithm/unique_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UNIQUE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UNIQUE_COPY_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using unique_copy_result = copy_result<I, O>;

namespace detail {

struct unique_copy_fn {
private:
    template <typename I, typename S, typename O,
              typename Comp, typename Proj>
    static constexpr unique_copy_result<I, O>
    impl(I first, S last, O result, Comp& comp, Proj& proj)
    {
        if (first != last) {
            iter_value_t<I> saved = *first;
            *result = saved;
            ++result;

            while (++first != last) {
                auto&& v = *first;
                if (!nano::invoke(comp, nano::invoke(proj, v), nano::invoke(proj, saved))) {
                    saved = std::forward<decltype(v)>(v);
                    *result = saved;
                    ++result;
                }
            }
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<2>) -> std::enable_if_t<forward_iterator<I>, std::true_type>;

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<1>) -> std::enable_if_t<input_iterator<O> && same_as<iter_value_t<I>, iter_value_t<O>>,
        std::true_type>;

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<0>) -> std::enable_if_t<indirectly_copyable_storable<I, O>, std::true_type>;

public:
    template <typename I, typename S, typename O, typename Comp = ranges::equal_to,
              typename Proj = identity>
    constexpr auto operator()(I first, S last, O result,
                              Comp comp = Comp{}, Proj proj = Proj{}) const
        -> std::enable_if_t<
            input_iterator<I> && sentinel_for<S, I> &&
               weakly_incrementable<O> &&
                indirect_relation<Comp, projected<I, Proj>> &&
                indirectly_copyable<I, O> &&
               decltype(constraint_helper<I, O>(priority_tag<2>{}))::value,
        unique_copy_result<I, O>>
    {
        return unique_copy_fn::impl(std::move(first), std::move(last),
                                    std::move(result), comp, proj);
    }

    template <typename Rng, typename O, typename Comp = ranges::equal_to,
              typename Proj = identity>
    constexpr auto
    operator()(Rng&& rng, O result, Comp comp = Comp{}, Proj proj = Proj{}) const
    -> std::enable_if_t<
            input_range<Rng> &&
            weakly_incrementable<O> &&
                indirect_relation<Comp, projected<iterator_t<Rng>, Proj>> &&
                indirectly_copyable<iterator_t<Rng>, O> &&
            decltype(constraint_helper<iterator_t<Rng>, O>(priority_tag<2>{}))::value,
       unique_copy_result<safe_iterator_t<Rng>, O>>
    {
        return unique_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                    std::move(result), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::unique_copy_fn, unique_copy)

NANO_END_NAMESPACE

#endif
