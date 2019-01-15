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
private:
    template <typename I>
    static constexpr auto impl(I i, iter_difference_t<I> n, nano::detail::priority_tag<1>)
        noexcept(noexcept(nano::make_subrange(i, i + n)))
        -> std::enable_if_t<RandomAccessIterator<I>, decltype(nano::make_subrange(i, i + n))>
    {
        return nano::make_subrange(i, i + n);
    }

    template <typename I>
    static constexpr auto impl(I i, iter_difference_t<I> n, nano::detail::priority_tag<0>)
        noexcept(noexcept(nano::make_subrange(
                nano::make_counted_iterator(std::move(i), n),
                default_sentinel)))
        -> decltype(nano::make_subrange(
            nano::make_counted_iterator(std::move(i), n), default_sentinel))
    {
        return nano::make_subrange(nano::make_counted_iterator(std::move(i), n),
                                   default_sentinel);
    }

public:
    template <typename E, typename F, typename T = std::decay_t<E>>
    constexpr auto operator()(E&& e, F&& f) const
        noexcept(noexcept(impl(std::forward<E>(e),
                               static_cast<iter_difference_t<T>>(std::forward<F>(f)),
                               nano::detail::priority_tag<1>{})))
        -> std::enable_if_t<
            Iterator<T> &&
            ConvertibleTo<F, iter_difference_t<T>>,
            decltype(impl(std::forward<E>(e),
                          static_cast<iter_difference_t<T>>(std::forward<F>(f)),
                          nano::detail::priority_tag<1>{}))>
    {
        return impl(std::forward<E>(e),
                    static_cast<iter_difference_t<T>>(std::forward<F>(f)),
                    nano::detail::priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::counted_fn, counted)

} // namespace view

NANO_END_NAMESPACE

#endif
