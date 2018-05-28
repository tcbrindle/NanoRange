// nanorange/algorithm/stl/unique_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_UNIQUE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_UNIQUE_COPY_HPP_INCLUDED

#include <nanorange/range.hpp>

#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct unique_copy_fn {
private:
    template <typename I, typename O, typename R>
    static std::enable_if_t<ForwardIterator<I>, O>
    dispatch_helper(I first, I last, O result, R& comp, priority_tag<2>)
    {
        return std::unique_copy(std::move(first), std::move(last),
                                std::move(result), std::ref(comp));
    }

    template <typename I, typename O, typename R>
    static std::enable_if_t<
            InputIterator<O> && Same<value_type_t<I>, value_type_t<O>>,
    O>
    dispatch_helper(I first, I last, O result, R& comp, priority_tag<1>)
    {
        return std::unique_copy(std::move(first), std::move(last),
                                std::move(result), std::ref(comp));
    }

    template <typename I, typename O, typename R>
    static std::enable_if_t<IndirectlyCopyableStorable<I, O>, O>
    dispatch_helper(I first, I last, O result, R& comp, priority_tag<0>)
    {
        return std::unique_copy(std::move(first), std::move(last),
                                std::move(result), std::ref(comp));
    }

public:
    template <typename I, typename O, typename R = equal_to<>>
    auto operator()(I first, I last, O result, R comp = R{}) const
        -> std::enable_if_t<
               InputIterator<I> &&
               detail::Cpp98Iterator<I> &&
               WeaklyIncrementable<O> &&
               detail::Cpp98Iterator<O> &&
               IndirectRelation<R, I> &&
               IndirectlyCopyable<I, O>,
    decltype(
    unique_copy_fn::dispatch_helper(first, last, result,
                                    comp, priority_tag<2>{}))>
    {
        return std::unique_copy(std::move(first), std::move(last),
                                std::move(result), std::ref(comp));
    }

    // Case 1: Input is ForwardIterator
    template <typename Rng, typename O, typename R = equal_to<>>
    auto operator()(Rng&& rng, O result, R comp = R{}) const
    -> std::enable_if_t<
            InputRange<Rng> &&
            CommonRange<Rng> &&
            Cpp98Iterator<iterator_t<Rng>> &&
            WeaklyIncrementable<O> &&
            Cpp98Iterator<O> &&
            IndirectRelation<R, iterator_t<Rng>> &&
            IndirectlyCopyable<iterator_t<Rng>, O>,
    decltype(
    unique_copy_fn::dispatch_helper(nano::begin(rng), nano::end(rng), result,
                                    comp, priority_tag<2>{}))>
    {
        return std::unique_copy(nano::begin(rng), nano::end(rng),
                                std::move(result), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::unique_copy_fn, unique_copy)

NANO_END_NAMESPACE

#endif
