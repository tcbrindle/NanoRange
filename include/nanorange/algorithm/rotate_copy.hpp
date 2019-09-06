// nanorange/algorithm/rotate_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_COPY_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/algorithm/copy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using rotate_copy_result = copy_result<I, O>;

namespace detail {

struct rotate_copy_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr rotate_copy_result<I, O>
    impl(I first, I middle, S last, O result)
    {
        auto ret = nano::copy(middle, std::move(last), std::move(result));
        ret.out = nano::copy(std::move(first), std::move(middle), ret.out).out;
        return ret;
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<forward_iterator<I> && sentinel_for<S, I> &&
        weakly_incrementable<O> &&
                                   indirectly_copyable<I, O>,
        rotate_copy_result<I, O>>
    operator()(I first, I middle, S last, O result) const
    {
        return rotate_copy_fn::impl(std::move(first), std::move(middle),
                                    std::move(last), std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<forward_range<Rng> &&
        weakly_incrementable<O> &&
                                   indirectly_copyable<iterator_t<Rng>, O>,
        rotate_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, iterator_t<Rng> middle, O result) const
    {
        return rotate_copy_fn::impl(nano::begin(rng), std::move(middle),
                                    nano::end(rng), std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::rotate_copy_fn, rotate_copy)

NANO_END_NAMESPACE

#endif
