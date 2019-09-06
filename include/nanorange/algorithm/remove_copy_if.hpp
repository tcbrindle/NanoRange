// nanorange/algorithm/remove_copy_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_COPY_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_COPY_IF_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using remove_copy_if_result = copy_result<I, O>;

namespace detail {

struct remove_copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename Proj>
    static constexpr remove_copy_if_result<I, O>
    impl(I first, S last, O result, Pred& pred, Proj& proj)
    {
        while (first != last) {
            auto&& ref = *first;
            if (!nano::invoke(pred, nano::invoke(proj, ref))) {
                *result = std::forward<decltype(ref)>(ref);
                ++result;
            }
            ++first;
        }
        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename Pred,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O> &&
            indirectly_copyable<I, O> &&
            indirect_unary_predicate<Pred, projected<I, Proj>>,
        remove_copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred,
               Proj proj = Proj{}) const
    {
        return remove_copy_if_fn::impl(std::move(first), std::move(last),
                                    std::move(result), pred, proj);
    }

    template <typename Rng, typename O, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && weakly_incrementable<O> &&
            indirectly_copyable<iterator_t<Rng>, O> &&
            indirect_unary_predicate<Pred, projected<iterator_t<Rng>, Proj>>,
        remove_copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, Proj proj = Proj{}) const
    {
        return remove_copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                       std::move(result), pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_copy_if_fn, remove_copy_if)

NANO_END_NAMESPACE

#endif
