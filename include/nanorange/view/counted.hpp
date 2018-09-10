// nanorange/view/counted.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_COUNTED_HPP_INCLUDED
#define NANORANGE_VIEW_COUNTED_HPP_INCLUDED

#include <nanorange/iterator/counted_iterator.hpp>
#include <nanorange/view/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace view {

namespace detail {

struct counted_fn {

    template <typename T>
    constexpr auto operator()(T* t, std::ptrdiff_t n) const
        -> std::enable_if_t<std::is_object<T>::value, subrange<T*>>
    {
        return nano::make_subrange(t, t + n);
    }

    template <typename I>
    constexpr auto operator()(I iter, iter_difference_t<I> n) const
        -> std::enable_if_t<Iterator<I>, decltype(
                nano::make_subrange(
                  nano::make_counted_iterator(std::move(iter), n),
                  default_sentinel{}))>
    {
        return nano::make_subrange(
                  nano::make_counted_iterator(std::move(iter), n),
                  default_sentinel{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::counted_fn, counted)

} // namespace view

NANO_END_NAMESPACE

#endif
