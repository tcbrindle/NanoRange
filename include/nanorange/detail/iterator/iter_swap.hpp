// nanorange/detail/iterator/iter_swap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ITER_SWAP_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ITER_SWAP_HPP_INCLUDED

#include <nanorange/detail/iterator/indirect_callable_concepts.hpp>
#include <nanorange/detail/iterator/iter_move.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {
namespace iter_swap_ {

// ADL "poison pill"
template <typename I1, typename I2>
void iter_swap(I1, I2) = delete;

// FIXME MSVC: add a second (redundant) poison pill
template <typename I>
void iter_swap(I, I) = delete;

struct fn {
private:
    template <typename T1, typename T2>
    static constexpr bool iter_exchange_move_noexcept =
        std::is_nothrow_constructible<iter_value_t<T1>,
                                      iter_rvalue_reference_t<T1>>::value&&
            std::is_nothrow_assignable<iter_value_t<T1>&,
                                       iter_rvalue_reference_t<T1>>::value&&
                std::is_nothrow_assignable<iter_reference_t<T1>,
                                           iter_rvalue_reference_t<T2>>::value&&
                    std::is_nothrow_assignable<iter_reference_t<T1>,
                                               iter_value_t<T2>>::value&&
                        std::is_nothrow_move_constructible<iter_value_t<T1>>::
                            value&& noexcept(
                                ranges::iter_move(std::declval<T1&>()));

    template <typename X, typename Y>
    static constexpr iter_value_t<std::remove_reference_t<X>>
    iter_exchange_move(X&& x, Y&& y) noexcept(
        iter_exchange_move_noexcept<std::remove_reference_t<X>,
                                    std::remove_reference_t<Y>>&&
            iter_exchange_move_noexcept<std::remove_reference_t<Y>,
                                        std::remove_reference_t<X>>)
    {
        iter_value_t<std::remove_reference_t<X>> old_value(
            ranges::iter_move(x));
        *x = ranges::iter_move(y);
        return old_value;
    }

    template <typename T, typename U>
    static constexpr auto impl(T&& t, U&& u, priority_tag<2>) noexcept(
        noexcept((void) (iter_swap(std::forward<T>(t), std::forward<U>(u)))))
        -> decltype((void) (iter_swap(std::forward<T>(t), std::forward<U>(u))))
    {
        (void) iter_swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U>
    static constexpr auto impl(T&& t, U&& u, priority_tag<1>) noexcept(
        noexcept(ranges::swap(*std::forward<T>(t), *std::forward<U>(u))))
        -> std::enable_if_t<Readable<std::remove_reference_t<T>> &&
                            Readable<std::remove_reference_t<U>> &&
                            SwappableWith<iter_reference_t<T>, iter_reference_t<U>>>
    {
        ranges::swap(*std::forward<T>(t), *std::forward<U>(u));
    }

    template <typename T, typename U>
    static constexpr auto impl(T&& t, U&& u, priority_tag<0>) noexcept(noexcept(
        *t = fn::iter_exchange_move(std::forward<U>(u), std::forward<T>(t))))
        -> std::enable_if_t<IndirectlyMovableStorable<T, U> &&
                            IndirectlyMovableStorable<U, T>>
    {
        return *t = fn::iter_exchange_move(std::forward<U>(u),
                                           std::forward<T>(t));
    }

public:
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), std::forward<U>(u),
                                   priority_tag<2>{})))
            -> decltype(fn::impl(std::forward<T>(t), std::forward<U>(u),
                                 priority_tag<2>{}))
    {
        return fn::impl(std::forward<T>(t), std::forward<U>(u),
                        priority_tag<2>{});
    }
};
}
} // namespace detail

NANO_DEFINE_CPO(detail::iter_swap_::fn, iter_swap)

NANO_END_NAMESPACE

#endif
