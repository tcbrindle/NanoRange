// nanorange/detail/functional/invoke.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED

#include <functional>

#include <nanorange/detail/macros.hpp>
#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// This is a reimplementation of std::invoke, which for some stupid
// reason is not constexpr in C++17
inline namespace invoke_ {

template <typename>
constexpr bool is_reference_wrapper_v = false;

template <typename T>
constexpr bool is_reference_wrapper_v<std::reference_wrapper<T>> = true;

struct fn {
private:
    template <class Base, class T, class Derived, class... Args>
    static constexpr auto
    impl(T Base::*pmf, Derived&& ref,
         Args&&... args) noexcept(noexcept((std::forward<Derived>(ref).*
                                            pmf)(std::forward<Args>(args)...)))
        -> std::enable_if_t<
            std::is_function<T>::value &&
                std::is_base_of<Base, std::decay_t<Derived>>::value,
            decltype((std::forward<Derived>(ref).*
                      pmf)(std::forward<Args>(args)...))>
    {
        return (std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...);
    }

    template <class Base, class T, class RefWrap, class... Args>
    static constexpr auto
    impl(T Base::*pmf, RefWrap&& ref, Args&&... args) noexcept(
        noexcept((ref.get().*pmf)(std::forward<Args>(args)...)))
        -> std::enable_if_t<std::is_function<T>::value &&
                                is_reference_wrapper_v<std::decay_t<RefWrap>>,
                            decltype((ref.get().*
                                      pmf)(std::forward<Args>(args)...))>
    {
        return (ref.get().*pmf)(std::forward<Args>(args)...);
    }

    template <class Base, class T, class Pointer, class... Args>
    static constexpr auto
    impl(T Base::*pmf, Pointer&& ptr,
         Args&&... args) noexcept(noexcept(((*std::forward<Pointer>(ptr)).*
                                            pmf)(std::forward<Args>(args)...)))
        -> std::enable_if_t<
            std::is_function<T>::value &&
                !is_reference_wrapper_v<std::decay_t<Pointer>> &&
                !std::is_base_of<Base, std::decay_t<Pointer>>::value,
            decltype(((*std::forward<Pointer>(ptr)).*
                      pmf)(std::forward<Args>(args)...))>
    {
        return ((*std::forward<Pointer>(ptr)).*
                pmf)(std::forward<Args>(args)...);
    }

    template <class Base, class T, class Derived>
    static constexpr auto
    impl(T Base::*pmd,
         Derived&& ref) noexcept(noexcept(std::forward<Derived>(ref).*pmd))
        -> std::enable_if_t<
            !std::is_function<T>::value &&
                std::is_base_of<Base, std::decay_t<Derived>>::value,
            decltype(std::forward<Derived>(ref).*pmd)>
    {
        return std::forward<Derived>(ref).*pmd;
    }

    template <class Base, class T, class RefWrap>
    static constexpr auto impl(T Base::*pmd,
                               RefWrap&& ref) noexcept(noexcept(ref.get().*pmd))
        -> std::enable_if_t<!std::is_function<T>::value &&
                                is_reference_wrapper_v<std::decay_t<RefWrap>>,
                            decltype(ref.get().*pmd)>
    {
        return ref.get().*pmd;
    }

    template <class Base, class T, class Pointer>
    static constexpr auto
    impl(T Base::*pmd,
         Pointer&& ptr) noexcept(noexcept((*std::forward<Pointer>(ptr)).*pmd))
        -> std::enable_if_t<
            !std::is_function<T>::value &&
                !is_reference_wrapper_v<std::decay_t<Pointer>> &&
                !std::is_base_of<Base, std::decay_t<Pointer>>::value,
            decltype((*std::forward<Pointer>(ptr)).*pmd)>
    {
        return (*std::forward<Pointer>(ptr)).*pmd;
    }

    template <class F, class... Args>
    static constexpr auto impl(F&& f, Args&&... args) noexcept(
        noexcept(std::forward<F>(f)(std::forward<Args>(args)...)))
        -> std::enable_if_t<
            !std::is_member_pointer<std::decay_t<F>>::value,
            decltype(std::forward<F>(f)(std::forward<Args>(args)...))>
    {
        return std::forward<F>(f)(std::forward<Args>(args)...);
    }

public:
    template <typename F, typename... Args>
    constexpr auto operator()(F&& f, Args&&... args) const noexcept(
        noexcept(fn::impl(std::forward<F>(f), std::forward<Args>(args)...)))
        -> decltype(fn::impl(std::forward<F>(f), std::forward<Args>(args)...))
    {
        return fn::impl(std::forward<F>(f), std::forward<Args>(args)...);
    }
};

} // namespace invoke_
} // namespace detail

NANO_DEFINE_CPO(nano::detail::invoke_::fn, invoke)

namespace detail {

template <typename Void, typename, typename...>
struct invoke_result_helper {
};

template <typename F, typename... Args>
struct invoke_result_helper<
    void_t<decltype(nano::invoke(std::declval<F>(), std::declval<Args>()...))>,
    F, Args...> {
    using type =
        decltype(nano::invoke(std::declval<F>(), std::declval<Args>()...));
};

} // namespace detail

template <typename F, typename... Args>
struct invoke_result : detail::invoke_result_helper<void, F, Args...> {
};

template <typename F, typename... Args>
using invoke_result_t = typename invoke_result<F, Args...>::type;

NANO_END_NAMESPACE

#endif
