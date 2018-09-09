// nanorange/view/all.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_ALL_HPP_INCLUDED
#define NANORANGE_VIEW_ALL_HPP_INCLUDED

#include <nanorange/detail/view/ref.hpp>
#include <nanorange/view/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// TODO: Handle piping views
struct all_fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<2>)
        noexcept(noexcept(detail::decay_copy(std::forward<T>(t))))
        -> std::enable_if_t<View<std::decay_t<T>>,
                            decltype(detail::decay_copy(std::forward<T>(t)))>
    {
        return std::forward<T>(t);
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept
        -> decltype(detail::make_ref_view(std::forward<T>(t)))
    {
        return detail::make_ref_view(std::forward<T>(t));
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(nano::make_subrange(std::forward<T>(t))))
        -> decltype(nano::make_subrange(std::forward<T>(t)))
    {
        return nano::make_subrange(std::forward<T>(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(all_fn::impl(std::forward<T>(t), priority_tag<2>{})))
        -> decltype(all_fn::impl(std::forward<T>(t), priority_tag<2>{}))
    {
        return all_fn::impl(std::forward<T>(t), priority_tag<2>{});
    }
};

} // namespace detail

namespace view {

NANO_INLINE_VAR(detail::all_fn, all)

}

template <typename R>
using all_view = std::enable_if_t<ViewableRange<R>,
                                  decltype(view::all(std::declval<R>()))>;

NANO_END_NAMESPACE

#endif
