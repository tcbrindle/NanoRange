// nanorange/memory/destroy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_DESTROY_HPP_INCLUDED
#define NANORANGE_MEMORY_DESTROY_HPP_INCLUDED

#include <nanorange/detail/memory/concepts.hpp>
#include <nanorange/iterator/counted_iterator.hpp>

NANO_BEGIN_NAMESPACE

template <typename T>
std::enable_if_t<destructible<T>>
destroy_at(T* location) noexcept
{
    location->~T();
}

namespace detail {

struct destroy_fn {
private:
    template <typename I, typename S>
    static I impl(I first, S last) noexcept
    {
        for (; first != last; ++first) {
            nano::destroy_at(std::addressof(*first));
        }
        return first;
    }

public:
    template <typename I, typename S>
    std::enable_if_t<no_throw_input_iterator<I> && no_throw_sentinel<S, I> &&
        destructible<iter_value_t<I>>, I>
    operator()(I first, S last) const noexcept
    {
        return destroy_fn::impl(std::move(first), std::move(last));
    }

    template <typename Rng>
    std::enable_if_t<no_throw_input_range<Rng> &&
        destructible<iter_value_t<iterator_t<Rng>>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const noexcept
    {
        return destroy_fn::impl(nano::begin(rng), nano::end(rng));
    }
};

}

NANO_INLINE_VAR(detail::destroy_fn, destroy)

namespace detail {

struct destroy_n_fn {
    template <typename I>
    std::enable_if_t<
        no_throw_input_iterator<I> &&
        destructible<iter_value_t<I>>, I>
    operator()(I first, iter_difference_t<I> n) const noexcept
    {
        return nano::destroy(make_counted_iterator(std::move(first), n),
                             default_sentinel).base();
    }


};

}

NANO_INLINE_VAR(detail::destroy_n_fn, destroy_n)

NANO_END_NAMESPACE

#endif
