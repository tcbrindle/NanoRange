// nanorange/views/all.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_ALL_HPP_INCLUDED
#define NANORANGE_VIEWS_ALL_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/ref.hpp>
#include <nanorange/views/subrange.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct all_view_fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<2>)
        noexcept(noexcept(detail::decay_copy(std::forward<T>(t))))
        -> std::enable_if_t<view<std::decay_t<T>>,
                            decltype(detail::decay_copy(std::forward<T>(t)))>
    {
        return std::forward<T>(t);
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept
        -> decltype(ref_view(std::forward<T>(t)))
    {
        return ref_view(std::forward<T>(t));
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(nano::subrange{std::forward<T>(t)}))
        -> decltype(nano::subrange{std::forward<T>(t)})
    {
        return nano::subrange{std::forward<T>(t)};
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(all_view_fn::impl(std::forward<T>(t), priority_tag<2>{})))
        -> decltype(all_view_fn::impl(std::forward<T>(t), priority_tag<2>{}))
    {
        return all_view_fn::impl(std::forward<T>(t), priority_tag<2>{});
    }
};

template <>
inline constexpr bool is_raco<all_view_fn> = true;

} // namespace detail

namespace views {

NANO_INLINE_VAR(nano::detail::all_view_fn, all)

}

template <typename R>
using all_view = std::enable_if_t<viewable_range<R>,
                                  decltype(views::all(std::declval<R>()))>;

NANO_END_NAMESPACE

#endif
