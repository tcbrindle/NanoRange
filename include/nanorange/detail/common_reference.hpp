// nanorange/detail/common_reference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_COMMON_REFERENCE_HPP_INCLUDED
#define NANORANGE_DETAIL_COMMON_REFERENCE_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>
#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

template <typename...>
struct common_type;

namespace detail {

template <typename T, typename U>
struct copy_cv {
    using type = U;
};

template <typename T, typename U>
struct copy_cv<const T, U> {
    using type = std::add_const_t<U>;
};

template <typename T, typename U>
struct copy_cv<volatile T, U> {
    using type = std::add_volatile_t<U>;
};

template <typename T, typename U>
struct copy_cv<const volatile T, U> {
    using type = std::add_cv_t<U>;
};

template <typename T, typename U>
using copy_cv_t = typename copy_cv<T, U>::type;

template <typename T>
using cref_t = std::add_lvalue_reference_t<const std::remove_reference_t<T>>;

// Workaround for "term does not evaluate to a function taking 0 arguments"
// error in MSVC 19.22 (issue #75)
#if defined(_MSC_VER) && _MSC_VER >= 1922
template <typename, typename, typename = void>
struct cond_res {};

template <typename T, typename U>
struct cond_res<T, U, std::void_t<decltype(false ? std::declval<T (&)()>()()
                                                 : std::declval<U (&)()>()())>>
{
    using type = decltype(false ? std::declval<T (&)()>()()
                                : std::declval<U (&)()>()());
};

template <typename T, typename U>
using cond_res_t = typename cond_res<T, U>::type;
#else
template <typename T, typename U>
using cond_res_t = decltype(false ? std::declval<T (&)()>()()
                                  : std::declval<U (&)()>()());
#endif


// For some value of "simple"
template <typename A, typename B,
          typename X = std::remove_reference_t<A>,
          typename Y = std::remove_reference_t<B>,
          typename = void>
struct common_ref {};

template <typename A, typename B>
using common_ref_t = typename common_ref<A, B>::type;

template <typename A, typename B,
          typename X = std::remove_reference_t<A>,
          typename Y = std::remove_reference_t<B>,
          typename = void>
struct lval_common_ref {};

template <typename A, typename B, typename X, typename Y>
struct lval_common_ref<A, B, X, Y, std::enable_if_t<
    std::is_reference_v<cond_res_t<copy_cv_t<X, Y>&, copy_cv_t<Y, X>&>>>>
{
    using type = cond_res_t<copy_cv_t<X, Y>&, copy_cv_t<Y, X>&>;
};

template <typename A, typename B>
using lval_common_ref_t = typename lval_common_ref<A, B>::type;

template <typename A, typename B, typename X, typename Y>
struct common_ref<A&, B&, X, Y> : lval_common_ref<A&, B&> {};

template <typename X, typename Y>
using rref_cr_helper_t = std::remove_reference_t<lval_common_ref_t<X&, Y&>>&&;

template <typename A, typename B, typename X, typename Y>
struct common_ref<A&&, B&&, X, Y, std::enable_if_t<
    std::is_convertible_v<A&&, rref_cr_helper_t<X, Y>> &&
    std::is_convertible_v<B&&, rref_cr_helper_t<X, Y>>>>
{
    using type = rref_cr_helper_t<X, Y>;
};

template <typename A, typename B, typename X, typename Y>
struct common_ref<A&&, B&, X, Y, std::enable_if_t<
    std::is_convertible_v<A&&, lval_common_ref_t<const X&, Y&>>>>
{
    using type = lval_common_ref_t<const X&, Y&>;
};

template <typename A, typename B, typename X, typename Y>
struct common_ref<A&, B&&, X, Y>
    : common_ref<B&&, A&>
{};

template <typename>
struct xref { template <typename U> using type = U; };

template <typename A>
struct xref<A&> {
    template <typename U>
    using type = std::add_lvalue_reference_t<typename xref<A>::template type<U>>;
};

template <typename A>
struct xref<A&&> {
    template <typename U>
    using type = std::add_rvalue_reference_t<typename xref<A>::template type<U>>;
};

template <typename A>
struct xref<const A> {
    template <typename U>
    using type = std::add_const_t<typename xref<A>::template type<U>>;
};

template <typename A>
struct xref<volatile A> {
    template <typename U>
    using type = std::add_volatile_t<typename xref<A>::template type<U>>;
};

template <typename A>
struct xref<const volatile A> {
    template <typename U>
    using type = std::add_cv_t<typename xref<A>::template type<U>>;
};

} // namespace detail

template <class T, class U, template <class> class TQual,
          template <class> class UQual>
struct basic_common_reference {
};

template <typename...>
struct common_reference;

template <typename... Ts>
using common_reference_t = typename common_reference<Ts...>::type;

template <>
struct common_reference<> {
};

template <typename T0>
struct common_reference<T0> {
    using type = T0;
};

namespace detail {

template <typename T, typename U>
inline constexpr bool has_common_ref_v =
    exists_v<common_ref_t, T, U>;

template <typename T, typename U>
using basic_common_ref_t =
    typename basic_common_reference<remove_cvref_t<T>, remove_cvref_t<U>,
                                    detail::xref<T>::template type, detail::xref<U>::template type>::type;

template <typename T, typename U>
inline constexpr bool has_basic_common_ref_v =
    exists_v<basic_common_ref_t, T, U>;

template <typename T, typename U>
inline constexpr bool has_cond_res_v = exists_v<cond_res_t, T, U>;

template <typename T, typename U, typename = void>
struct binary_common_ref : common_type<T, U> {
};

template <typename T, typename U>
struct binary_common_ref<T, U, std::enable_if_t<has_common_ref_v<T, U>>>
    : common_ref<T, U> {};

template <typename T, typename U>
struct binary_common_ref<T, U,
                         std::enable_if_t<has_basic_common_ref_v<T, U> &&
                                          !has_common_ref_v<T, U>>>
{
    using type = basic_common_ref_t<T, U>;
};

template <typename T, typename U>
struct binary_common_ref<T, U,
                         std::enable_if_t<has_cond_res_v<T, U> &&
                                          !has_basic_common_ref_v<T, U> &&
                                          !has_common_ref_v<T, U>>>
{
    using type = cond_res_t<T, U>;
};

} // namespace detail

template <typename T1, typename T2>
struct common_reference<T1, T2> : detail::binary_common_ref<T1, T2> {
};

namespace detail {

template <typename Void, typename T1, typename T2, typename... Rest>
struct multiple_common_reference {
};

template <typename T1, typename T2, typename... Rest>
struct multiple_common_reference<std::void_t<common_reference_t<T1, T2>>, T1, T2,
                                 Rest...>
    : common_reference<common_reference_t<T1, T2>, Rest...> {
};

} // namespace detail

template <typename T1, typename T2, typename... Rest>
struct common_reference<T1, T2, Rest...>
    : detail::multiple_common_reference<void, T1, T2, Rest...> {
};

NANO_END_NAMESPACE

#endif
