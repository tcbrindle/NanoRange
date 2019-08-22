// nanorange/detail/functional/invoke.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>
#include <nanorange/detail/type_traits.hpp>

#include <functional>

NANO_BEGIN_NAMESPACE

namespace detail {

// This is a reimplementation of std::invoke, which for some stupid
// reason is not constexpr in C++17
namespace invoke_ {

template <typename>
inline constexpr bool is_reference_wrapper_v = false;

template <typename T>
inline constexpr bool is_reference_wrapper_v<std::reference_wrapper<T>> = true;

struct fn {
private:
    template <typename T, typename Type, typename T1, typename... Args>
    static constexpr decltype(auto)
    impl_member_ptr(Type T::* f, T1&& t1, Args&&... args)
    {
        if constexpr (std::is_member_function_pointer_v<decltype(f)>) {
            if constexpr (std::is_base_of_v<T, std::decay_t<T1>>) {
                return (std::forward<T1>(t1).*f)(std::forward<Args>(args)...);
            } else if constexpr (is_reference_wrapper_v<std::decay_t<T1>>) {
                return (t1.get().*f)(std::forward<Args>(args)...);
            } else {
                return ((*std::forward<T1>(t1)).*f)(std::forward<Args>(args)...);
            }
        } else {
            static_assert(std::is_member_object_pointer_v<decltype(f)>);
            static_assert(sizeof...(args) == 0);
            if constexpr (std::is_base_of_v<T, std::decay_t<T1>>) {
                return std::forward<T1>(t1).*f;
            } else if constexpr (is_reference_wrapper_v<std::decay_t<T1>>) {
                return t1.get().*f;
            } else {
                return (*std::forward<T1>(t1)).*f;
            }
        }
    }

public:
    template <typename F, typename... Args>
    constexpr auto operator()(F&& f, Args&&... args) const
        noexcept(std::is_nothrow_invocable_v<F, Args...>)
        -> std::invoke_result_t<F, Args...>
    {
        if constexpr (std::is_member_pointer_v<std::decay_t<F>>) {
            return impl_member_ptr(std::forward<F>(f), std::forward<Args>(args)...);
        } else {
            return std::forward<F>(f)(std::forward<Args>(args)...);
        }
    }
};

} // namespace invoke_
} // namespace detail

NANO_INLINE_VAR(nano::detail::invoke_::fn, invoke)

template <typename F, typename... Args>
using invoke_result = std::invoke_result<F, Args...>;

template <typename F, typename... Args>
using invoke_result_t = std::invoke_result_t<F, Args...>;

NANO_END_NAMESPACE

#endif
