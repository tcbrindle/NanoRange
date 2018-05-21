// nanorange/detail/iterator/iter_move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED

#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {
namespace iter_move_ {

struct fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<2>) noexcept(
        noexcept(static_cast<decltype(iter_move(t))>(iter_move(t))))
        -> decltype(static_cast<decltype(iter_move(t))>(iter_move(t)))
    {
        return static_cast<decltype(iter_move(t))>(iter_move(t));
    }

    template <typename T>
    static constexpr auto impl(T& t, priority_tag<1>) noexcept(
        noexcept(std::move(*std::declval<T&>()))) -> decltype(std::move(*t))
    {
        return std::move(*t);
    }

    template <typename T>
    static constexpr auto
    impl(T&& t,
         priority_tag<0>) noexcept(noexcept(static_cast<decltype(*t)>(*t)))
        -> decltype(static_cast<decltype(*t)>(*t))
    {
        return static_cast<decltype(*t)>(*t);
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<2>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<2>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<2>{});
    }
};

} // namespace iter_move_
} // namespace detail

NANO_INLINE_VAR(detail::iter_move_::fn, iter_move)

NANO_END_NAMESPACE

#endif
