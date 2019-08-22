// nanorange/memory/uninitialized_fill.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_FILL_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_FILL_HPP_INCLUDED

#include <nanorange/memory/destroy.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct uninitialized_fill_fn {
private:
    friend struct uninitialized_fill_n_fn;

    template <typename I, typename S, typename T>
    static I impl(I first, S last, const T& x)
    {
        I it = first;
        try {
            for (; it != last; ++it) {
                ::new(detail::voidify(*it))
                        std::remove_reference_t<iter_reference_t<I>>(x);
            }
            return it;
        } catch (...) {
            nano::destroy(first, ++it);
            throw;
        }
    }

public:
    template <typename I, typename S, typename T>
    std::enable_if_t<no_throw_forward_iterator<I> && no_throw_sentinel<S, I> &&
        constructible_from<iter_value_t<I>, const T&>,
        I>
    operator()(I first, S last, const T& x) const
    {
        return uninitialized_fill_fn::impl(std::move(first), std::move(last), x);
    }

    template <typename Rng, typename T>
    std::enable_if_t<
        no_throw_forward_range<Rng> &&
        constructible_from<iter_value_t<iterator_t<Rng>>, const T&>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& x) const
    {
        return uninitialized_fill_fn::impl(nano::begin(rng), nano::end(rng), x);
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_fill_fn, uninitialized_fill)

namespace detail {

struct uninitialized_fill_n_fn {
    template <typename I, typename T>
    std::enable_if_t<no_throw_forward_iterator<I> &&
        constructible_from<iter_value_t<I>, const T&>,
        I>
    operator()(I first, iter_difference_t<I> n, const T& x) const
    {
        return uninitialized_fill_fn::impl(
                    make_counted_iterator(std::move(first), n),
                    default_sentinel, x).base();
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_fill_n_fn, uninitialized_fill_n)

NANO_END_NAMESPACE

#endif
