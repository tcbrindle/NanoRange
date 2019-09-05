// nanorange/detail/type_traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_TYPE_TRAITS_HPP_INCLUDED
#define NANORANGE_DETAIL_TYPE_TRAITS_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <type_traits>

NANO_BEGIN_NAMESPACE

template <typename T>
struct remove_cvref {
    using type = std::remove_cv_t<std::remove_reference_t<T>>;
};

template <typename T>
using remove_cvref_t = typename remove_cvref<T>::type;

template <typename T>
struct type_identity {
    using type = T;
};

template <typename T>
using type_identity_t = typename type_identity<T>::type;

namespace detail {

template <bool>
struct conditional {
    template <typename T, typename>
    using type = T;
};

template <>
struct conditional<false> {
    template <typename, typename U>
    using type = U;
};

template <bool B, typename T, typename U>
using conditional_t = typename conditional<B>::template type<T, U>;

template <template <class...> class AliasT, typename... Args>
auto exists_helper(long) -> std::false_type;

template <template <class...> class AliasT, typename... Args,
          typename = AliasT<Args...>>
auto exists_helper(int) -> std::true_type;

template <template <class...> class AliasT, typename... Args>
inline constexpr bool exists_v = decltype(exists_helper<AliasT, Args...>(0))::value;

template <typename, typename...>
auto test_requires_fn(long) -> std::false_type;

template <typename R, typename... Args,
          typename = decltype(&R::template requires_<Args...>)>
auto test_requires_fn(int) -> std::true_type;

template <typename R, typename... Args>
inline constexpr bool requires_ = decltype(test_requires_fn<R, Args...>(0))::value;

template <bool Expr>
using requires_expr = std::enable_if_t<Expr, int>;

template <std::size_t I>
struct priority_tag : priority_tag<I - 1> {
};

template <>
struct priority_tag<0> {
};

} // namespace detail

NANO_END_NAMESPACE

#endif
