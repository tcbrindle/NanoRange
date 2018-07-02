// nanorange/detail/iterator/iter_move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED

#include <nanorange/detail/type_traits.hpp>

#include <utility>

NANO_BEGIN_NAMESPACE

namespace detail {
namespace iter_move_ {

void iter_move();

struct fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<2>) /*noexcept(
        noexcept(static_cast<decltype(iter_move(t))>(iter_move(t))))
        -> decltype(static_cast<decltype(iter_move(t))>(iter_move(t)))*/
        noexcept(noexcept(iter_move(t)))
        -> decltype(iter_move(t))
    {
        return iter_move(t);
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(std::move(*std::declval<T&&>())))
       -> std::enable_if_t<std::is_lvalue_reference<decltype(*std::forward<T>(t))>::value,
                           decltype(std::move(*std::forward<T>(t)))>
    {
        return std::move(*std::forward<T>(t));
    }

    template <typename T>
    static constexpr auto
    impl(T&& t, priority_tag<0>)
        noexcept(noexcept(*std::forward<T>(t)))
        -> decltype(*std::forward<T>(t))
    {
        return *std::forward<T>(t);
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

NANO_DEFINE_CPO(detail::iter_move_::fn, iter_move)

NANO_END_NAMESPACE

#endif
