// nanorange/detail/common_reference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_COMMON_TYPE_HPP_INCLUDED
#define NANORANGE_DETAIL_COMMON_TYPE_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>
#include <nanorange/detail/common_reference.hpp>
#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

template <typename...>
struct common_type;

template <typename... Ts>
using common_type_t = typename common_type<Ts...>::type;

namespace detail {

template <typename T, typename U>
constexpr bool same_decayed_v =
        std::is_same<T, std::decay_t<T>>::value &&
        std::is_same<U, std::decay_t<U>>::value;

template <typename T, typename U>
using ternary_return_t =
    std::decay_t<decltype(false ? std::declval<T>() : std::declval<U>())>;

template <typename, typename, typename = void>
struct binary_common_type {};

template <typename T, typename U>
struct binary_common_type<T, U,
        std::enable_if_t<!same_decayed_v<T, U>>>
    : nano::common_type<std::decay_t<T>, std::decay_t<U>> {};

template <typename T, typename U>
struct binary_common_type<T, U,
        std::enable_if_t<same_decayed_v<T, U> &&
                         exists_v<ternary_return_t, T, U>>> {
    using type = ternary_return_t<T, U>;
};

template <typename T, typename U>
struct binary_common_type<T, U,
        std::enable_if_t<same_decayed_v<T, U> &&
                         !exists_v<ternary_return_t, T, U> &&
                          exists_v<cond_res_t, cref_t<T>, cref_t<U>>>> {
    using type = std::decay_t<cond_res_t<cref_t<T>, cref_t<U>>>;
};

}

template <>
struct common_type<> {};

template <typename T>
struct common_type<T> : common_type<T, T> {};

template <typename T, typename U>
struct common_type<T, U>
    : detail::binary_common_type<T, U> {};

namespace detail {

template <typename Void, typename...>
struct multiple_common_type {};

template <typename T1, typename T2, typename... R>
struct multiple_common_type<std::void_t<common_type_t<T1, T2>>, T1, T2, R...>
    : common_type<common_type_t<T1, T2>, R...> {};

}


template <typename T1, typename T2, typename... R>
struct common_type<T1, T2, R...>
    : detail::multiple_common_type<void, T1, T2, R...> {};

NANO_END_NAMESPACE

#endif
