// nanorange/algorithm/remove_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_COPY_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>
#include <nanorange/algorithm/find.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using remove_copy_result = copy_result<I, O>;

namespace detail {

struct remove_copy_fn {
private:
    template <typename I, typename S, typename O, typename T, typename Proj>
    static constexpr remove_copy_result<I, O>
    impl(I first, S last, O result, const T& value, Proj& proj)
    {
        while (first != last) {
            auto&& ref = *first;
            if (!(nano::invoke(proj, ref) == value)) {
                *result = std::forward<decltype(ref)>(ref);
                ++result;
            }
            ++first;
        }
        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O> &&
            indirectly_copyable<I, O> &&
            indirect_relation<ranges::equal_to, projected<I, Proj>, const T*>,
        remove_copy_result<I, O>>
    operator()(I first, S last, O result, const T& value,
               Proj proj = Proj{}) const
    {
        return remove_copy_fn::impl(std::move(first), std::move(last),
                                    std::move(result), value, proj);
    }

    template <typename Rng, typename O, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        input_range<Rng> && weakly_incrementable<O> &&
            indirectly_copyable<iterator_t<Rng>, O> &&
            indirect_relation<ranges::equal_to, projected<iterator_t<Rng>, Proj>,
                             const T*>,
        remove_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, const T& value, Proj proj = Proj{}) const
    {
        return remove_copy_fn::impl(nano::begin(rng), nano::end(rng),
                               std::move(result), value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_copy_fn, remove_copy)

NANO_END_NAMESPACE

#endif
