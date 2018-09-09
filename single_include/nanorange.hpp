// nanorange.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_HPP_INCLUDED
#define NANORANGE_HPP_INCLUDED

// nanorange/algorithm.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_HPP_INCLUDED
#define NANORANGE_ALGORITHM_HPP_INCLUDED

// Algorithms reimplemented in Nanorange
// nanorange/algorithm/adjacent_find.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ADJACENT_FIND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ADJACENT_FIND_HPP_INCLUDED

// nanorange/range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANGES_HPP_INCLUDED
#define NANORANGE_RANGES_HPP_INCLUDED

// nanorange/detail/iterator/algorithm_requirements.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ALGORITHM_REQUIREMENTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ALGORITHM_REQUIREMENTS_HPP_INCLUDED

// nanorange/detail/functional/comparisons.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_COMPARISONS_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_COMPARISONS_HPP_INCLUDED

// nanorange/detail/concepts/comparison.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_COMPARISON_HPP_INCLUDED

// nanorange/detail/concepts/core.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_CORE_HPP_INCLUDED

// nanorange/detail/macros.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MACROS_HPP_INCLUDED
#define NANORANGE_DETAIL_MACROS_HPP_INCLUDED

#if (__cplusplus >= 201703) || (defined(_MSVC_LANG) && _MSVC_LANG >= 201703L)
#define NANO_HAVE_CPP17
#endif

#if defined(NANO_HAVE_CPP17) || defined(__cpp_inline_variables)
#define NANO_HAVE_INLINE_VARS
#endif

#ifdef NANORANGE_NO_DEPRECATION_WARNINGS
#define NANO_DEPRECATED
#define NANO_DEPRECATED_FOR(x)
#else
#define NANO_DEPRECATED [[deprecated]]
#define NANO_DEPRECATED_FOR(x) [[deprecated(x)]]
#endif

#ifdef NANO_HAVE_CPP17
#define NANO_NODISCARD [[nodiscard]]
#else
#define NANO_NODISCARD
#endif

#if defined(NANO_HAVE_CPP17) || defined(__cpp_deduction_guides)
#define NANO_HAVE_DEDUCTION_GUIDES
#endif

#define NANO_CONCEPT constexpr bool

#define NANO_BEGIN_NAMESPACE                                                   \
    \
namespace nano                                                                 \
    {                                                                          \
        \
inline namespace ranges                                                        \
        {

#define NANO_END_NAMESPACE                                                     \
    }                                                                          \
    }

#ifdef NANO_HAVE_INLINE_VARS
#define NANO_INLINE_VAR(type, name)                                            \
    inline namespace function_objects {                                        \
    inline constexpr type name{};                                              \
    }

#else
#define NANO_INLINE_VAR(type, name)                                            \
    inline namespace function_objects {                                        \
    inline namespace {                                                         \
    constexpr const auto& name =                                               \
        ::nano::ranges::detail::static_const_<type>::value;                    \
    }                                                                          \
    }
#endif

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
struct static_const_ {
    static constexpr T value{};
};

template <typename T>
constexpr T static_const_<T>::value;

} // namespace detail

NANO_END_NAMESPACE

#endif

// nanorange/type_traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_TYPE_TRAITS_HPP_INCLUDED
#define NANORANGE_TYPE_TRAITS_HPP_INCLUDED

// nanorange/detail/common_reference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_COMMON_REFERENCE_HPP_INCLUDED
#define NANORANGE_DETAIL_COMMON_REFERENCE_HPP_INCLUDED


// nanorange/detail/type_traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_TYPE_TRAITS_HPP_INCLUDED
#define NANORANGE_DETAIL_TYPE_TRAITS_HPP_INCLUDED



#include <type_traits>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template <typename...>
using void_t = void;

template <typename... T>
void valid_expr(T&&...);

struct error_t {
    error_t() = delete;
    error_t(error_t const&) = delete;
    error_t& operator=(const error_t&) = delete;
    ~error_t() = delete;
};

template <typename Void, template <class...> class Trait, typename... Args>
struct test_ {
    using type = error_t;
};

template <template <class...> class Trait, typename... Args>
struct test_<void_t<Trait<Args...>>, Trait, Args...> {
    using type = Trait<Args...>;
};

template <template <class...> class Trait, typename... Args>
using test_t = typename test_<void, Trait, Args...>::type;

// Work around GCC5 bug that won't let us specialise variable templates
template <typename Void, template <class...> class AliasT, typename... Args>
struct exists_helper : std::false_type{};

template <template <class...> class AliasT, typename... Args>
struct exists_helper<void_t<AliasT<Args...>>, AliasT, Args...>
    : std::true_type{};

template <template <class...> class AliasT, typename... Args>
constexpr bool exists_v = exists_helper<void, AliasT, Args...>::value;

template <typename R, typename... Args,
          typename = decltype(&R::template requires_<Args...>)>
auto test_requires(R&) -> void;

template <typename R, typename... Args>
using test_requires_t = decltype(test_requires<R, Args...>(std::declval<R&>()));

template <typename R, typename... Args>
constexpr bool requires_ = exists_v<test_requires_t, R, Args...>;

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

template <typename T>
using uncvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template <typename T>
struct rref_res {
    using type = T;
};

template <typename T>
struct rref_res<T&> {
    using type = std::remove_reference_t<T>&&;
};

template <typename T>
using rref_res_t = typename rref_res<T>::type;

template <typename T, typename U>
using cond_res_t = decltype(std::declval<bool>() ? std::declval<T (&)()>()()
                                                 : std::declval<U (&)()>()());

// For some value of "simple"
template <typename T, typename U>
struct simple_common_reference {
};

template <typename T, typename U,
          typename C = test_t<cond_res_t, copy_cv_t<T, U>&, copy_cv_t<U, T>&>>
struct lvalue_simple_common_reference
    : std::enable_if<std::is_reference<C>::value, C> {
};

template <typename T, typename U>
using lvalue_scr_t = typename lvalue_simple_common_reference<T, U>::type;

template <typename T, typename U>
struct simple_common_reference<T&, U&> : lvalue_simple_common_reference<T, U> {
};

template <typename T, typename U, typename LCR = test_t<lvalue_scr_t, T, U>,
          typename C = rref_res_t<LCR>>
struct rvalue_simple_common_reference
    : std::enable_if<std::is_convertible<T&&, C>::value &&
                         std::is_convertible<U&&, C>::value,
                     C> {
};

template <typename T, typename U>
struct simple_common_reference<T&&, U&&>
    : rvalue_simple_common_reference<T, U> {
};

template <typename A, typename B, typename C = test_t<lvalue_scr_t, A, const B>>
struct mixed_simple_common_reference
    : std::enable_if<std::is_convertible<B&&, C>::value, C> {
};

template <typename A, typename B>
struct simple_common_reference<A&, B&&> : mixed_simple_common_reference<A, B> {
};

template <typename A, typename B>
struct simple_common_reference<A&&, B&> : simple_common_reference<B&, A&&> {
};

template <typename T, typename U>
using simple_common_reference_t = typename simple_common_reference<T, U>::type;


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
constexpr bool has_simple_common_ref_v =
    exists_v<simple_common_reference_t, T, U>;

template <typename T, typename U>
using basic_common_ref_t =
    typename basic_common_reference<remove_cvref_t<T>, remove_cvref_t<U>,
                                    detail::xref<T>::template type, detail::xref<U>::template type>::type;

template <typename T, typename U>
constexpr bool has_basic_common_ref_v =
    exists_v<basic_common_ref_t, T, U>;

template <typename T, typename U>
constexpr bool has_cond_res_v = exists_v<cond_res_t, T, U>;

template <typename T, typename U, typename = void>
struct binary_common_ref : common_type<T, U> {
};

template <typename T, typename U>
struct binary_common_ref<T, U, std::enable_if_t<has_simple_common_ref_v<T, U>>>
    : simple_common_reference<T, U> {
};

template <typename T, typename U>
struct binary_common_ref<T, U,
                         std::enable_if_t<has_basic_common_ref_v<T, U> &&
                                          !has_simple_common_ref_v<T, U>>>
{
    using type = basic_common_ref_t<T, U>;
};

template <typename T, typename U>
struct binary_common_ref<T, U,
                         std::enable_if_t<has_cond_res_v<T, U> &&
                                          !has_basic_common_ref_v<T, U> &&
                                          !has_simple_common_ref_v<T, U>>>
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
struct multiple_common_reference<void_t<common_reference_t<T1, T2>>, T1, T2,
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
// nanorange/detail/common_reference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_COMMON_TYPE_HPP_INCLUDED
#define NANORANGE_DETAIL_COMMON_TYPE_HPP_INCLUDED





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
struct multiple_common_type<void_t<common_type_t<T1, T2>>, T1, T2, R...>
    : common_type<common_type_t<T1, T2>, R...> {};

}


template <typename T1, typename T2, typename... R>
struct common_type<T1, T2, R...>
    : detail::multiple_common_type<void, T1, T2, R...> {};

NANO_END_NAMESPACE

#endif


#endif


#include <functional>

NANO_BEGIN_NAMESPACE

// [concepts.lib.corelang.same]
template <typename T, typename U>
NANO_CONCEPT Same = std::is_same<T, U>::value;

// [concepts.lib.corelang.derived]

// FIXME: Spec doesn't use remove_reference_t here, not sure if it should
template <typename Derived, typename Base>
NANO_CONCEPT DerivedFrom = std::is_base_of<Base, Derived>::value&&
    std::is_convertible<const volatile std::remove_reference_t<Derived>*,
                        const volatile std::remove_reference_t<Base>*>::value;

// [concepts.lib.corelang.convertibleto]
namespace detail {

struct ConvertibleTo_req {
    template <typename From, typename To>
    auto requires_(From (&f)()) -> decltype(static_cast<To>(f()));
};

} // namespace detail

// [concepts.lib.corelang.convertibleto]
template <typename From, typename To>
NANO_CONCEPT ConvertibleTo = std::is_convertible<From, To>::value&&
    detail::requires_<detail::ConvertibleTo_req, From, To>;

// [concepts.lib.corelang.commonref]
namespace detail {

template <typename T, typename U>
auto CommonReference_fn(long) -> std::false_type;

template <typename T, typename U>
auto CommonReference_fn(int) -> std::enable_if_t<
        Same<common_reference_t<T, U>, common_reference_t<U, T>> &&
        ConvertibleTo<T, common_reference_t<T, U>> &&
        ConvertibleTo<U, common_reference_t<T, U>>,
                std::true_type>;
}


template <typename T, typename U>
NANO_CONCEPT CommonReference = decltype(detail::CommonReference_fn<T, U>(0))::value;

// [concepts.lib.corelang.common]
namespace detail {

template <typename T, typename U>
auto Common_fn(long) -> std::false_type;

template <typename T, typename U>
auto Common_fn(int) -> std::enable_if_t<
    Same<common_type_t<T, U>, common_type_t<U, T>> &&
    ConvertibleTo<T, common_type_t<T, U>> &&
    ConvertibleTo<U, common_type_t<T, U>> &&
    CommonReference<
        std::add_lvalue_reference_t<const T>,
        std::add_lvalue_reference_t<const U>> &&
    CommonReference<
        std::add_lvalue_reference_t<common_type_t<T, U>>,
        common_reference_t<
            std::add_lvalue_reference_t<const T>,
            std::add_lvalue_reference_t<const U>>>,
    std::true_type>;

}


template <typename T, typename U>
NANO_CONCEPT Common = decltype(detail::Common_fn<T, U>(0))::value;

// [concepts.lib.corelang.integral]
template <typename T>
NANO_CONCEPT Integral = std::is_integral<T>::value;

// [concepts.lib.corelang.signedintegral]
template <typename T>
NANO_CONCEPT SignedIntegral = Integral<T>&& std::is_signed<T>::value;

// [concepts.lib.corelang.unsignedintegral]
template <typename T>
NANO_CONCEPT UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

// [concepts.lib.corelang.assignable]

namespace detail {

struct Assignable_req {
    template <typename LHS, typename RHS>
    auto requires_(LHS lhs, RHS&& rhs) -> decltype(valid_expr(
        lhs = std::forward<RHS>(rhs),
        requires_expr<Same<decltype(lhs = std::forward<RHS>(rhs)), LHS>>{}));
};

template <typename LHS, typename RHS>
auto Assignable_fn(long) -> std::false_type;

template <typename LHS, typename RHS>
auto Assignable_fn(int) -> std::enable_if_t<
        std::is_lvalue_reference<LHS>::value &&
        CommonReference<const std::remove_reference_t<LHS>&,
                        const std::remove_reference_t<RHS>&> &&
        requires_<Assignable_req, LHS, RHS>,
                std::true_type>;

} // namespace detail

template <typename LHS, typename RHS>
NANO_CONCEPT Assignable = decltype(detail::Assignable_fn<LHS, RHS>(0))::value;

// [concepts.lib.corelang.destructible]
template <typename T>
NANO_CONCEPT Destructible = std::is_nothrow_destructible<T>::value;

// [concepts.lib.corelang.constructible]
template <typename T, typename... Args>
NANO_CONCEPT Constructible =
    Destructible<T>&& std::is_constructible<T, Args...>::value;

// [concepts.lib.corelang.defaultconstructible]
template <typename T>
NANO_CONCEPT DefaultConstructible = Constructible<T>;

// [concepts.lib.corelang.moveconstructible]
template <typename T>
NANO_CONCEPT MoveConstructible = Constructible<T, T>&& ConvertibleTo<T, T>;

// [concepts.lib.corelang.copyconstructible]
namespace detail {

template <typename T>
auto CopyConstructible_fn(long) -> std::false_type;

template <typename T>
auto CopyConstructible_fn(int) -> std::enable_if_t<
        MoveConstructible<T> &&
        Constructible<T, T&> && ConvertibleTo<T&, T> &&
        Constructible<T, const T&> && ConvertibleTo<const T&, T> &&
        Constructible<T, const T> && ConvertibleTo<const T, T>,
                std::true_type>;


}

template <typename T>
NANO_CONCEPT CopyConstructible = decltype(detail::CopyConstructible_fn<T>(0))::value;

NANO_END_NAMESPACE

#endif

// nanorange/detail/concepts/movable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_MOVABLE_HPP
#define NANORANGE_DETAIL_CONCEPTS_MOVABLE_HPP


// nanorange/detail/concepts/swappable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_SWAPPABLE_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_SWAPPABLE_HPP_INCLUDED


// nanorange/detail/swap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_SWAP_HPP_INCLUDED
#define NANORANGE_DETAIL_SWAP_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.swap]

namespace detail {
namespace swap_ {

template <typename T>
void swap(T&, T&) = delete;

template <typename T, std::size_t N>
void swap(T (&)[N], T (&)[N]) = delete;

struct fn {
private:
    template <typename T, typename U>
    static constexpr auto impl(T&& t, U&& u, priority_tag<2>) noexcept(
        noexcept(swap(std::forward<T>(t), std::forward<U>(u))))
        -> decltype(static_cast<void>(swap(std::forward<T>(t),
                                           std::forward<U>(u))))
    {
        (void) swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U, std::size_t N, typename F = fn>
    static constexpr auto
        impl(T (&t)[N], U (&u)[N],
             priority_tag<1>) noexcept(noexcept(std::declval<F&>()(*t, *u)))
            -> decltype(std::declval<F&>()(*t, *u))
    {
        for (std::size_t i = 0; i < N; ++i) {
            fn::impl(t[i], u[i], priority_tag<2>{});
        }
    }

    template <typename T>
    static constexpr auto impl(T& a, T& b, priority_tag<0>) noexcept(
        std::is_nothrow_move_constructible<T>::value&&
            std::is_nothrow_assignable<T&, T>::value)
        -> std::enable_if_t<MoveConstructible<T> && Assignable<T&, T>>
    {
        T temp = std::move(a);
        a = std::move(b);
        b = std::move(temp);
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

} // end namespace swap_
} // end namespace detail

NANO_INLINE_VAR(detail::swap_::fn, swap)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

// Implement the Swappable concepts now we have swap()

struct Swappable_req {
    template <typename T>
    auto requires_(T& a, T& b) -> decltype(ranges::swap(a, b));
};

struct SwappableWith_req {
    template <typename T, typename U>
    auto requires_(T&& t, U&& u)
        -> decltype(ranges::swap(std::forward<T>(t), std::forward<T>(t)),
                    ranges::swap(std::forward<U>(u), std::forward<U>(u)),
                    ranges::swap(std::forward<T>(t), std::forward<U>(u)),
                    ranges::swap(std::forward<U>(u), std::forward<T>(t)));
};

} // namespace detail

template <typename T>
NANO_CONCEPT Swappable = detail::requires_<detail::Swappable_req, T>;

template <typename T, typename U>
NANO_CONCEPT SwappableWith = detail::requires_<detail::SwappableWith_req, T, U>;

NANO_END_NAMESPACE

#endif


// Movable is listed as an object concept, but is required for the definition
// of Boolean, so we treat it specially

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
auto Movable_fn(long) -> std::false_type;

template <typename T>
auto Movable_fn(int) -> std::enable_if_t<
        std::is_object<T>::value &&
        MoveConstructible<T> &&
        Assignable<T&, T> &&
        Swappable<T>,
                std::true_type>;

}

template <typename T>
NANO_CONCEPT Movable = decltype(detail::Movable_fn<T>(0))::value;

NANO_END_NAMESPACE

#endif



NANO_BEGIN_NAMESPACE

// [concepts.lib.compare.boolean]

namespace detail {

struct Boolean_req {
    template <typename B>
    auto requires_(const std::remove_reference_t<B>& b1,
                   const std::remove_reference_t<B>& b2, const bool a)
        -> decltype(valid_expr(
            requires_expr<
                ConvertibleTo<const std::remove_reference_t<B&>, bool>>{},
            !b1, requires_expr<ConvertibleTo<decltype(!b1), bool>>{}, b1&& a,
            requires_expr<Same<decltype(b1 && a), bool>>{}, b1 || a,
            requires_expr<Same<decltype(b1 || a), bool>>{}, b1&& b2,
            requires_expr<Same<decltype(b1 && b2), bool>>{}, a&& b2,
            requires_expr<Same<decltype(a && b2), bool>>{}, b1 || b2,
            requires_expr<Same<decltype(b1 || b2), bool>>{}, a || b2,
            requires_expr<Same<decltype(a || b2), bool>>{}, b1 == b2,
            requires_expr<ConvertibleTo<decltype(b1 == b2), bool>>{}, b1 == a,
            requires_expr<ConvertibleTo<decltype(b1 == a), bool>>{}, a == b2,
            requires_expr<ConvertibleTo<decltype(a == b2), bool>>{}, b1 != b2,
            requires_expr<ConvertibleTo<decltype(b1 != b2), bool>>{}, b1 != a,
            requires_expr<ConvertibleTo<decltype(b1 != a), bool>>{}, a != b2,
            requires_expr<ConvertibleTo<decltype(a != b2), bool>>{}));
};

} // namespace detail

template <typename B>
NANO_CONCEPT Boolean = Movable<detail::remove_cvref_t<B>>&&
    detail::requires_<detail::Boolean_req, B>;

// [concepts.lib.compare.equalitycomparable]

namespace detail {

struct WeaklyEqualityComparableWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            valid_expr(t == u, requires_expr<Boolean<decltype(t == u)>>{},
                       t != u, requires_expr<Boolean<decltype(t != u)>>{},
                       u == t, requires_expr<Boolean<decltype(u == t)>>{},
                       u != t, requires_expr<Boolean<decltype(u != t)>>{}));
};

template <typename T, typename U>
NANO_CONCEPT WeaklyEqualityComparableWith =
    requires_<WeaklyEqualityComparableWith_req, T, U>;

} // namespace detail

template <typename T>
NANO_CONCEPT EqualityComparable = detail::WeaklyEqualityComparableWith<T, T>;

namespace detail {

template <typename, typename>
auto EqualityComparableWith_fn(long) -> std::false_type;

template <typename T, typename U>
auto EqualityComparableWith_fn(int) -> std::enable_if_t<
    EqualityComparable<T> && EqualityComparable<U> &&
    CommonReference<const std::remove_reference_t<T>&,
                    const std::remove_reference_t<U>&> &&
    EqualityComparable<
       common_reference_t<
               const std::remove_reference_t<T>&,
               const std::remove_reference_t<U>&>> &&
    WeaklyEqualityComparableWith<T, U>,
            std::true_type>;

}

template <typename T, typename U>
NANO_CONCEPT EqualityComparableWith =
    decltype(detail::EqualityComparableWith_fn<T, U>(0))::value;

// [concepts.lib.compare.stricttotallyordered]

namespace detail {

struct StrictTotallyOrdered_req {
    template <typename T>
    auto requires_(const std::remove_reference_t<T>& a,
                   const std::remove_reference_t<T>& b)
        -> decltype(
            valid_expr(a<b, requires_expr<Boolean<decltype(a < b)>>{}, a> b,
                       requires_expr<Boolean<decltype(a > b)>>{}, a <= b,
                       requires_expr<Boolean<decltype(a <= b)>>{}, a >= b,
                       requires_expr<Boolean<decltype(a >= b)>>{}));
};

} // namespace detail

template <typename T>
NANO_CONCEPT StrictTotallyOrdered = EqualityComparable<T>&&
    detail::requires_<detail::StrictTotallyOrdered_req, T>;

namespace detail {

struct StrictTotallyOrderedWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u)
        -> decltype(
            valid_expr(t<u, requires_expr<Boolean<decltype(t < u)>>{}, t> u,
                       requires_expr<Boolean<decltype(t > u)>>{}, t <= u,
                       requires_expr<Boolean<decltype(t <= u)>>{}, t >= u,
                       requires_expr<Boolean<decltype(t >= u)>>{},
                       u<t, requires_expr<Boolean<decltype(u < t)>>{}, u> t,
                       requires_expr<Boolean<decltype(u > t)>>{}, u <= t,
                       requires_expr<Boolean<decltype(u <= t)>>{}, u >= t,
                       requires_expr<Boolean<decltype(u >= t)>>{}));
};

template <typename, typename>
auto StrictTotallyOrderedWith_fn(long) -> std::false_type;

template <typename T, typename U>
auto StrictTotallyOrderedWith_fn(int) -> std::enable_if_t<
        StrictTotallyOrdered<T> && StrictTotallyOrdered<U> &&
        CommonReference<const std::remove_reference_t<T>&,
                        const std::remove_reference_t<U>&> &&
        StrictTotallyOrdered<
                common_reference_t<
                        const std::remove_reference_t<T>&,
                        const std::remove_reference_t<U>&>> &&
        EqualityComparableWith<T, U> &&
        requires_<StrictTotallyOrderedWith_req, T, U>,
                std::true_type>;

} // namespace detail

template <typename T, typename U>
NANO_CONCEPT StrictTotallyOrderedWith =
    decltype(detail::StrictTotallyOrderedWith_fn<T, U>(0))::value;

NANO_END_NAMESPACE

#endif



#include <functional>

NANO_BEGIN_NAMESPACE

// [range.comparisons]

namespace detail {

template <typename = void, typename = void>
struct equal_to_helper;

template <>
struct equal_to_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(std::equal_to<>{}(std::forward<T>(t),
                                            std::forward<U>(u))))
            -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct equal_to_helper<T, std::enable_if_t<EqualityComparable<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(equal_to_helper<>{}(t, u)))
    {
        return equal_to_helper<>{}(t, u);
    }
};

template <typename, typename = void>
struct not_equal_to_helper;

template <>
struct not_equal_to_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(!equal_to_helper<>{}(std::forward<T>(t),
                                               std::forward<U>(u))))
            -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return !equal_to_helper<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct not_equal_to_helper<T, std::enable_if_t<EqualityComparable<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!equal_to_helper<>{}(t, u)))
    {
        return !equal_to_helper<>{}(t, u);
    }
};

template <typename = void, typename = void>
struct less_helper;

template <>
struct less_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(std::less<>{}(std::forward<T>(t), std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return std::less<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct less_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(less_helper<>{}(t, u)))
    {
        return less_helper<>{}(t, u);
    }
};

template <typename, typename = void>
struct greater_helper;

template <>
struct greater_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(less_helper<>{}(std::forward<T>(t),
                                          std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return less_helper<>{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct greater_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(less_helper<>{}(u, t)))
    {
        return less_helper<>{}(u, t);
    }
};

template <typename, typename = void>
struct less_equal_helper;

template <>
struct less_equal_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(!less_helper<>{}(std::forward<U>(u),
                                           std::forward<T>(t))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return !less_helper<>{}(std::forward<U>(u), std::forward<T>(t));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct less_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(u, t)))
    {
        return !less_helper<>{}(u, t);
    }
};

template <typename, typename = void>
struct greater_equal_helper;

template <>
struct greater_equal_helper<void> {
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(less_helper<>{}(std::forward<T>(t),
                                          std::forward<U>(u))))
            -> std::enable_if_t<StrictTotallyOrderedWith<T, U>, bool>
    {
        return !less_helper<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

template <typename T>
struct greater_equal_helper<T, std::enable_if_t<StrictTotallyOrdered<T>>> {
    constexpr bool operator()(const T& t, const T& u) const
        noexcept(noexcept(!less_helper<>{}(t, u)))
    {
        return !less_helper<>{}(t, u);
    }
};

} // namespace detail

template <typename T = void>
struct equal_to : detail::equal_to_helper<T> {
};

template <typename T = void>
struct not_equal_to : detail::not_equal_to_helper<T> {
};

template <typename T = void>
struct less : detail::less_helper<T> {
};

template <typename T = void>
struct greater : detail::greater_helper<T> {
};

template <typename T = void>
struct greater_equal : detail::greater_equal_helper<T> {
};

template <typename T = void>
struct less_equal : detail::less_equal_helper<T> {
};

NANO_END_NAMESPACE

#endif
// nanorange/detail/functional/identity.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_IDENTITY_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_IDENTITY_HPP_INCLUDED



#include <functional>
#include <type_traits>

NANO_BEGIN_NAMESPACE

struct identity {
    template <typename T>
    constexpr T&& operator()(T&& t) const noexcept
    {
        return std::forward<T>(t);
    }

    using is_transparent = std::true_type;
};

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/indirect_callable_concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_INDIRECT_CALLABLE_CONCEPTS_HPP_INCLUDED

// nanorange/detail/iterator/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED

// nanorange/detail/concepts/object.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_CONCEPTS_OBJECT_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_OBJECT_HPP_INCLUDED




// nanorange/detail/functional/invoke.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_INVOKE_HPP_INCLUDED

#include <functional>




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

NANO_INLINE_VAR(nano::detail::invoke_::fn, invoke)

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


NANO_BEGIN_NAMESPACE

// [concepts.lib.object.copyable]
namespace detail {

template <typename>
auto Copyable_fn(long) -> std::false_type;

template <typename T>
auto Copyable_fn(int) -> std::enable_if_t<
        CopyConstructible<T> &&
        Movable<T> &&
        Assignable<T&, const T&>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT Copyable = decltype(detail::Copyable_fn<T>(0))::value;

// [concepts.lib.object.semiregular]
template <typename T>
NANO_CONCEPT Semiregular = Copyable<T>&& DefaultConstructible<T>;

// [concepts.lib.object.regular]
template <typename T>
NANO_CONCEPT Regular = Semiregular<T>&& EqualityComparable<T>;

namespace detail {

struct Invocable_req {
    /*template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) -> decltype(
        nano::invoke(std::forward<F>(f), std::forward<Args>(args)...)
    );*/
    // FIXME: Clang really doesn't like the above, work out why
    template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) -> invoke_result_t<F, Args...>;
};

} // namespace detail

template <typename F, typename... Args>
NANO_CONCEPT Invocable = detail::requires_<detail::Invocable_req, F, Args...>;

template <typename F, typename... Args>
NANO_CONCEPT RegularInvocable = Invocable<F, Args...>;

namespace detail {

template <typename, typename...>
auto Predicate_fn(long) -> std::false_type;

template <typename F, typename... Args>
auto Predicate_fn(int) -> std::enable_if_t<
        RegularInvocable<F, Args...> &&
        Boolean<invoke_result_t<F, Args...>>,
        std::true_type>;

}

template <typename F, typename... Args>
NANO_CONCEPT Predicate = decltype(detail::Predicate_fn<F, Args...>(0))::value;

namespace detail {

template <typename, typename, typename>
auto Relation_fn(long) -> std::false_type;

template <typename R, typename T, typename U>
auto Relation_fn(int) -> std::enable_if_t<
        Predicate<R, T, T> && Predicate<R, U, U> &&
        CommonReference<const std::remove_reference_t<T>&,
                        const std::remove_reference_t<U>&> &&
        Predicate<R,
                  common_reference_t<const std::remove_reference_t<T>&,
                                     const std::remove_reference_t<U>&>,
                  common_reference_t<const std::remove_reference_t<T>&,
                                     const std::remove_reference_t<U>&>> &&
        Predicate<R, T, U> && Predicate<R, U, T>,
                std::true_type>;

}

template <typename R, typename T, typename U>
NANO_CONCEPT Relation = decltype(detail::Relation_fn<R, T, U>(0))::value;

template <typename R, typename T, typename U>
NANO_CONCEPT StrictWeakOrder = Relation<R, T, U>;

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/associated_types.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ASSOCIATED_TYPES_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ASSOCIATED_TYPES_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename>
struct incrementable_traits;

namespace detail {

struct empty {};

template <typename T>
struct with_difference_type {
    using difference_type = T;
};

template <typename, typename = void>
struct incrementable_traits_helper {
};

// Workaround for GCC silliness: void* has no difference_type
// FIXME: This is required to stop WeaklyIncrementable<void*> being a hard error
// Can we formulate the concept differently to avoid the need for this hack?
template <>
struct incrementable_traits_helper<void*> {};

template <typename T>
struct incrementable_traits_helper<T*>
    : std::conditional_t<std::is_object<T>::value,
            with_difference_type<std::ptrdiff_t>, empty> {
};

template <class I>
struct incrementable_traits_helper<const I> : incrementable_traits<std::decay_t<I>> {
};

template <typename, typename = void>
struct has_member_difference_type : std::false_type {};

template <typename T>
struct has_member_difference_type<T, void_t<typename T::difference_type>>
    : std::true_type{};

template <typename T>
constexpr bool has_member_difference_type_v =
        has_member_difference_type<T>::value;

template <typename T>
struct incrementable_traits_helper<T, std::enable_if_t<has_member_difference_type_v<T>>> {
    using difference_type = typename T::difference_type;
};

template <typename T>
struct incrementable_traits_helper<
    T, std::enable_if_t<!std::is_pointer<T>::value &&
                        !has_member_difference_type_v<T> &&
                        Integral<decltype(std::declval<const T&>() -
                                          std::declval<const T&>())>>>
    : with_difference_type<std::make_signed_t<decltype(std::declval<T>() - std::declval<T>())>> {
};

} // namespace detail

template <typename T>
struct incrementable_traits : detail::incrementable_traits_helper<T> {
};

template <typename T>
using iter_difference_t = typename incrementable_traits<T>::difference_type;

// [range.iterator.assoc.types.value_type]

template <typename>
struct readable_traits;

namespace detail {

template <typename T>
struct with_value_type {
    using value_type = T;
};

template <typename, typename = void>
struct readable_traits_helper {};

template <typename T>
struct readable_traits_helper<T*>
    : std::conditional_t<std::is_object<T>::value,
            with_value_type<std::remove_cv_t<T>>,
            empty> {};

template <typename I>
struct readable_traits_helper<I, std::enable_if_t<std::is_array<I>::value>>
    : readable_traits<std::decay_t<I>> {};

template <typename I>
struct readable_traits_helper<const I, std::enable_if_t<!std::is_array<I>::value>>
    : readable_traits<std::decay_t<I>> {};

template <typename T, typename V = typename T::value_type>
struct member_value_type
    : std::conditional_t<std::is_object<V>::value,
            with_value_type<V>, empty> {};

template <typename T, typename E = typename T::element_type>
struct member_element_type
    : std::conditional_t<std::is_object<E>::value,
            with_value_type<std::remove_cv_t<E>>, empty> {};

template <typename T>
using member_value_type_t = typename T::value_type;

template <typename T>
constexpr bool has_member_value_type_v = exists_v<member_value_type_t, T>;

template <typename T>
using member_element_type_t = typename T::element_type;

template <typename T>
constexpr bool has_member_element_type_v = exists_v<member_element_type_t, T>;

template <typename T>
struct readable_traits_helper<T, std::enable_if_t<
    has_member_value_type_v<T> &&
    !has_member_element_type_v<T>>>
    : member_value_type<T> {};

template <typename T>
struct readable_traits_helper<T, std::enable_if_t<
    has_member_element_type_v<T> &&
    !has_member_value_type_v<T>>>
    : member_element_type<T> {};

// A type which has both value_type and element_type members must specialise
// readable_traits to tell us which one to prefer -- see
// https://github.com/ericniebler/stl2/issues/562
template <typename T>
struct readable_traits_helper<T, std::enable_if_t<
    has_member_element_type_v<T> &&
    has_member_value_type_v<T>>>
{};

} // namespace detail

template <typename T>
struct readable_traits : detail::readable_traits_helper<T> {};

template <typename T>
using iter_value_t = typename readable_traits<T>::value_type;

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_TRAITS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_TRAITS_HPP_INCLUDED

// nanorange/detail/iterator/dereferenceable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_DEREFERENCABLE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_DEREFERENCABLE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
auto not_void(T &&) -> int;

struct Dereferenceable_req {
    template <typename T>
    auto requires_(T& t) -> decltype(valid_expr(not_void(*t)));
};

template <typename T>
NANO_CONCEPT Dereferenceable = requires_<Dereferenceable_req, T>;

// GCC and Clang allow dereferencing void* as an extension.
// Let's kill that off now.

template <>
NANO_CONCEPT Dereferenceable<void*> = false;

} // namespace detail

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/iter_move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ITER_MOVE_HPP_INCLUDED



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

NANO_INLINE_VAR(detail::iter_move_::fn, iter_move)

NANO_END_NAMESPACE

#endif


#include <iterator>

NANO_BEGIN_NAMESPACE

// [range.iterator.assoc.types.iterator_category]
using std::bidirectional_iterator_tag;
using std::forward_iterator_tag;
using std::input_iterator_tag;
using std::output_iterator_tag;
using std::random_access_iterator_tag;

struct contiguous_iterator_tag : random_access_iterator_tag {};

template <typename T>
struct iterator_category;

namespace detail {

template <typename T, typename = void>
struct iterator_category_ {
};

template <typename T>
struct iterator_category_<T*>
    : std::enable_if<std::is_object<T>::value, contiguous_iterator_tag> {
};

template <typename T>
struct iterator_category_<const T> : iterator_category<T> {
};

template <typename T>
struct iterator_category_<T, void_t<typename T::iterator_category>> {
    using type = typename T::iterator_category;
};

} // namespace detail

template <typename T>
struct iterator_category : detail::iterator_category_<T> {
};

template <typename T>
using iterator_category_t = typename iterator_category<T>::type;

namespace detail {

template <typename T, typename = void>
struct legacy_iterator_category
    : iterator_category<T> {};

template <typename T>
struct legacy_iterator_category<T,
        std::enable_if_t<std::is_same<iterator_category_t<T>, contiguous_iterator_tag>::value>>
{
    using type = random_access_iterator_tag;
};

template <typename T>
using legacy_iterator_category_t = typename legacy_iterator_category<T>::type;

}


namespace detail {

template <typename, typename = void>
struct reference_helper {
};

template <typename T>
struct reference_helper<T, std::enable_if_t<Dereferenceable<T>>> {
    using type = decltype(*std::declval<T&>());
};

} // namespace detail

template <typename T>
using iter_reference_t = typename detail::reference_helper<T>::type;

template <typename T>
using iter_rvalue_reference_t = decltype(ranges::iter_move(std::declval<T&>()));

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

// [range.iterators.readable]
namespace detail {

struct Readable_req {
    template <typename In>
    auto requires_()
        -> decltype(std::declval<iter_value_t<In>>(),
                               std::declval<iter_reference_t<In>>(),
                               std::declval<iter_rvalue_reference_t<In>>());
};

template <typename>
auto Readable_fn(long) -> std::false_type;

template <typename In>
auto Readable_fn(int) -> std::enable_if_t<
     requires_<Readable_req, In> &&
     CommonReference<iter_reference_t<In>&&, iter_value_t<In>&> &&
     CommonReference<iter_reference_t<In>&&, iter_rvalue_reference_t<In>&&> &&
     CommonReference<iter_rvalue_reference_t<In>&&, const iter_value_t<In>&>,
             std::true_type>;

} // namespace detail

template <typename In>
NANO_CONCEPT Readable = decltype(detail::Readable_fn<In>(0))::value;

// [range.iterators.writable]
namespace detail {

struct Writable_req {
    template <typename Out, typename T>
    auto requires_(Out&& o, T&& t) -> decltype(valid_expr(
        *o = std::forward<T>(t), *std::forward<Out>(o) = std::forward<T>(t),
        const_cast<const iter_reference_t<Out>&&>(*o) = std::forward<T>(t),
        const_cast<const iter_reference_t<Out>&&>(*std::forward<Out>(o)) =
            std::forward<T>(t)));
};

} // namespace detail

template <typename Out, typename T>
NANO_CONCEPT Writable = detail::requires_<detail::Writable_req, Out, T>;

// [range.iterators.weaklyincrementable]

namespace detail {

template <typename T, typename Deduced>
auto same_lv(Deduced&) -> std::enable_if_t<Same<T, Deduced>, int>;

struct WeaklyIncrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(
        std::declval<iter_difference_t<I>>(),
        requires_expr<SignedIntegral<iter_difference_t<I>>>{},
        same_lv<I>(++i), i++);
};

} // namespace detail

template <typename I>
NANO_CONCEPT WeaklyIncrementable =
    Semiregular<I>&& detail::requires_<detail::WeaklyIncrementable_req, I>;

// [range.iterators.incrementable]

namespace detail {

struct Incrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(requires_expr<Same<decltype(i++), I>>{});
};

} // namespace detail

template <typename I>
NANO_CONCEPT Incrementable = Regular<I>&& WeaklyIncrementable<I>&&
    detail::requires_<detail::Incrementable_req, I>;

// [range.iterators.iterator]

namespace detail {

struct Iterator_req {
    template <typename I>
    auto requires_(I i) -> decltype(not_void(*i));
};

} // namespace detail

template <typename I>
NANO_CONCEPT Iterator =
    detail::requires_<detail::Iterator_req, I> && WeaklyIncrementable<I>;

// [range.iterators.sentinel]

template <typename S, typename I>
NANO_CONCEPT Sentinel =
    Semiregular<S>&& Iterator<I>&& detail::WeaklyEqualityComparableWith<S, I>;

// [range.iterators.sizedsentinel]

template <typename S, typename I>
constexpr bool disable_sized_sentinel = false;

namespace detail {

struct SizedSentinel_req {
    template <typename S, typename I>
    auto requires_(const S& s, const I& i)
        -> decltype(requires_expr<Same<decltype(s - i), iter_difference_t<I>>>{},
                    requires_expr<Same<decltype(i - s), iter_difference_t<I>>>{});
};

} // namespace detail

template <typename S, typename I>
NANO_CONCEPT SizedSentinel =
    Sentinel<S, I> &&
    !disable_sized_sentinel<std::remove_cv_t<S>, std::remove_cv_t<I>> &&
    detail::requires_<detail::SizedSentinel_req, S, I>;

// This is a hack, but I'm fed up with my tests breaking because GCC
// has a silly extension
template <typename S>
NANO_CONCEPT SizedSentinel<S, void*> = false;

template <typename I>
NANO_CONCEPT SizedSentinel<void*, I> = false;

template <>
NANO_CONCEPT SizedSentinel<void*, void*> = false;

// [range.iterators.input]

namespace detail {

template <typename>
auto InputIterator_fn(long) -> std::false_type;

template <typename I>
auto InputIterator_fn(int) -> std::enable_if_t<
    Iterator<I> && Readable<I> &&
    exists_v<iterator_category_t, I> &&
    DerivedFrom<iterator_category_t<I>, input_iterator_tag>,
            std::true_type>;


}

template <typename I>
NANO_CONCEPT InputIterator = decltype(detail::InputIterator_fn<I>(0))::value;

// [ranges.iterator.output]

namespace detail {

struct OutputIterator_req {
    template <typename I, typename T>
    auto requires_(I i, T&& t)
        -> decltype(valid_expr(*i++ = std::forward<T>(t)));
};

} // namespace detail

template <typename I, typename T>
NANO_CONCEPT OutputIterator = Iterator<I>&& Writable<I, T>&&
    detail::requires_<detail::OutputIterator_req, I, T>;

// [ranges.iterators.forward]

namespace detail {

template <typename>
auto ForwardIterator_fn(long) -> std::false_type;

template <typename I>
auto ForwardIterator_fn(int) -> std::enable_if_t<
        InputIterator<I> &&
        DerivedFrom<iterator_category_t<I>, forward_iterator_tag> &&
        Incrementable<I> &&
        Sentinel<I, I>,
                std::true_type>;

}

template <typename I>
NANO_CONCEPT ForwardIterator = decltype(detail::ForwardIterator_fn<I>(0))::value;

// [ranges.iterators.bidirectional]

namespace detail {

struct BidirectionalIterator_req {
    template <typename I>
    auto requires_(I i)
        -> decltype(same_lv<I>(--i), requires_expr<Same<decltype(i--), I>>{});
};

template <typename>
auto BidirectionalIterator_fn(long) -> std::false_type;

template <typename I>
auto BidirectionalIterator_fn(int) -> std::enable_if_t<
        ForwardIterator<I> &&
        DerivedFrom<iterator_category_t<I>, bidirectional_iterator_tag> &&
        requires_<BidirectionalIterator_req, I>,
                std::true_type>;

} // namespace detail

template <typename I>
NANO_CONCEPT BidirectionalIterator =
    decltype(detail::BidirectionalIterator_fn<I>(0))::value;

// [ranges.iterators.random.access]

namespace detail {

struct RandomAccessIterator_req {
    template <typename I>
    auto requires_(I i, const I j, const iter_difference_t<I> n) -> decltype(
        valid_expr(same_lv<I>(i += n),
                   j + n, requires_expr<Same<decltype(j + n), I>>{},
                   n + j,
#ifndef _MSC_VER
                   requires_expr<Same<decltype(n + j), I>>{}, // FIXME: MSVC doesn't like this when I = int*
#endif
                   same_lv<I>(i -= n),
                   j - n, requires_expr<Same<decltype(j - n), I>>{},
                   j[n],
                   requires_expr<Same<decltype(j[n]), iter_reference_t<I>>>{}));
};

template <typename>
auto RandomAccessIterator_fn(long) -> std::false_type;

template <typename I>
auto RandomAccessIterator_fn(int) -> std::enable_if_t<
     BidirectionalIterator<I> &&
     DerivedFrom<iterator_category_t<I>, random_access_iterator_tag> &&
     StrictTotallyOrdered<I> &&
     SizedSentinel<I, I> &&
     requires_<RandomAccessIterator_req, I>,
             std::true_type>;

} // namespace detail

template <typename I>
NANO_CONCEPT RandomAccessIterator = 
        decltype(detail::RandomAccessIterator_fn<I>(0))::value;

namespace detail {

template <typename>
auto ContiguousIterator_fn(long) -> std::false_type;

template <typename I>
auto ContiguousIterator_fn(int) -> std::enable_if_t<
    RandomAccessIterator<I> &&
    DerivedFrom<iterator_category_t<I>, contiguous_iterator_tag> &&
    std::is_lvalue_reference<iter_reference_t<I>>::value &&
    Same<iter_value_t<I>, remove_cvref_t<iter_reference_t<I>>>,
            std::true_type>;

}

template <typename I>
NANO_CONCEPT ContiguousIterator = decltype(detail::ContiguousIterator_fn<I>(0))::value;


// Extension: used for constraining iterators for existing STL algos

namespace detail {

template <typename I,
          typename T = std::iterator_traits<I>,
          typename = typename T::value_type,
          typename = typename T::difference_type,
          typename = typename T::reference,
          typename = typename T::pointer,
          typename = typename T::iterator_category>
using legacy_iterator_traits_t = void;

template <typename I>
NANO_CONCEPT Cpp98Iterator = exists_v<legacy_iterator_traits_t, I>;

}

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

// [range.indirectcallable.indirectinvocable]

namespace detail {

template <typename, typename = void>
struct iter_common_ref_helper {
};

template <typename T>
struct iter_common_ref_helper<T, std::enable_if_t<Readable<T>>> {
    using type = common_reference_t<iter_reference_t<T>, iter_value_t<T>&>;
};

} // namespace detail

template <typename I>
using iter_common_reference_t =
    typename detail::iter_common_ref_helper<I>::type;

namespace detail {

template <typename, typename>
auto IndirectUnaryInvocable_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectUnaryInvocable_fn(int) -> std::enable_if_t<
        Readable<I> &&
        CopyConstructible<F> &&
        Invocable<F&, iter_value_t<I>&> &&
        Invocable<F&, iter_reference_t<I>> &&
        Invocable<F&, iter_common_reference_t<I>> &&
        CommonReference<
                invoke_result_t<F&, iter_value_t<I>&>,
                invoke_result_t<F&, iter_reference_t<I>&>>,
            std::true_type>;

}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryInvocable =
        decltype(detail::IndirectUnaryInvocable_fn<F, I>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectRegularUnaryInvocable_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectRegularUnaryInvocable_fn(int) -> std::enable_if_t<
        Readable<I> &&
        CopyConstructible<F> &&
        RegularInvocable<F&, iter_value_t<I>&> &&
        RegularInvocable<F&, iter_reference_t<I>> &&
        RegularInvocable<F&, iter_common_reference_t<I>> &&
        CommonReference<
            invoke_result_t<F&, iter_value_t<I>&>,
            invoke_result_t<F&, iter_reference_t<I>&>>,
        std::true_type>;

}


template <typename F, typename I>
NANO_CONCEPT IndirectRegularUnaryInvocable =
        decltype(detail::IndirectRegularUnaryInvocable_fn<F, I>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectUnaryPredicate_fn(long) -> std::false_type;

template <typename F, typename I>
auto IndirectUnaryPredicate_fn(int) -> std::enable_if_t<
        Readable<I> &&
        CopyConstructible<F> &&
        Predicate<F&, iter_value_t<I>&> &&
        Predicate<F&, iter_reference_t<I>> &&
        Predicate<F&, iter_common_reference_t<I>>,
            std::true_type>;

}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryPredicate =
        decltype(detail::IndirectUnaryPredicate_fn<F, I>(0))::value;

namespace detail {

template <typename F, typename I1, typename I2>
auto IndirectRelation_fn(long) -> std::false_type;

template <typename F, typename I1, typename I2>
auto IndirectRelation_fn(int) -> std::enable_if_t<
        Readable<I1> && Readable<I2> && CopyConstructible<F> &&
        Relation<F&, iter_value_t<I1>&, iter_value_t<I2>&>&&
        Relation<F&, iter_value_t<I1>&, iter_reference_t<I2>>&&
        Relation<F&, iter_reference_t<I1>, iter_value_t<I2>&>&&
        Relation<F&, iter_reference_t<I1>, iter_reference_t<I2>>&&
        Relation<F&,
            iter_common_reference_t<I1>,
            iter_common_reference_t<I2>>,
    std::true_type>;

}


template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectRelation =
    decltype(detail::IndirectRelation_fn<F, I1, I2>(0))::value;


namespace detail {

template <typename, typename, typename>
auto IndirectStrictWeakOrder_fn(long) -> std::false_type;

template <typename F, typename I1, typename I2>
auto IndirectStrictWeakOrder_fn(int) -> std::enable_if_t<
        Readable<I1> &&
        Readable<I2> &&
        StrictWeakOrder<F&, iter_value_t<I1>&, iter_value_t<I2>&> &&
        StrictWeakOrder<F&, iter_value_t<I1>&, iter_reference_t<I2>> &&
        StrictWeakOrder<F&, iter_reference_t<I1>, iter_value_t<I2>&> &&
        StrictWeakOrder<F&, iter_reference_t<I1>, iter_reference_t<I2>> &&
        StrictWeakOrder<F&, iter_common_reference_t<I1>, iter_common_reference_t<I2>>,
    std::true_type>;

}

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectStrictWeakOrder =
        decltype(detail::IndirectStrictWeakOrder_fn<F, I1, I2>(0))::value;

template <typename, typename...>
struct indirect_result;

namespace detail {

template <typename Void, typename, typename...>
struct indirect_result_helper {
};

template <bool...>
struct all_readable_helper;

template <>
struct all_readable_helper<> : std::true_type {
};

template <bool First, bool... Rest>
struct all_readable_helper<First, Rest...>
    : std::conditional_t<First, all_readable_helper<Rest...>, std::false_type> {
};

template <typename... Is>
constexpr bool all_readable = all_readable_helper<Readable<Is>...>::value;

template <typename F, typename... Is>
struct indirect_result_helper<
    std::enable_if_t<all_readable<Is...> && Invocable<F, iter_reference_t<Is>...>>,
    F, Is...> : invoke_result<F, iter_reference_t<Is>...> {
    using type = invoke_result_t<F, iter_reference_t<Is>...>;
};

} // namespace detail

template <typename F, typename... Is>
struct indirect_result : detail::indirect_result_helper<void, F, Is...> {
};

template <typename F, typename... Is>
using indirect_result_t = typename indirect_result<F, Is...>::type;

// range.commonalgoreq.indirectlymovable]

namespace detail {

template <typename, typename>
auto IndirectlyMovable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyMovable_fn(int) -> std::enable_if_t<
        Readable<In> &&
        Writable<Out, iter_rvalue_reference_t<In>>,
    std::true_type>;


}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovable =
        decltype(detail::IndirectlyMovable_fn<In, Out>(0))::value;

namespace detail {

template <typename In, typename Out>
auto IndirectlyMovableStorable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyMovableStorable_fn(int) -> std::enable_if_t<
        IndirectlyMovable<In, Out> &&
        Writable<Out, iter_value_t<In>> &&
        Movable<iter_value_t<In>> &&
        Constructible<iter_value_t<In>, iter_rvalue_reference_t<In>> &&
        Assignable<iter_value_t<In>&, iter_rvalue_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovableStorable =
        decltype(detail::IndirectlyMovableStorable_fn<In, Out>(0))::value;

// range.commonalgoreq.indirectlycopyable

namespace detail {

template <typename, typename>
auto IndirectlyCopyable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyCopyable_fn(int) -> std::enable_if_t<
        Readable<In> &&
        Writable<Out, iter_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyable =
    decltype(detail::IndirectlyCopyable_fn<In, Out>(0))::value;

namespace detail {

template <typename, typename>
auto IndirectlyCopyableStorable_fn(long) -> std::false_type;

template <typename In, typename Out>
auto IndirectlyCopyableStorable_fn(int) -> std::enable_if_t<
        IndirectlyCopyable<In, Out> &&
        Writable<Out, const iter_value_t<In>&> &&
        Copyable<iter_value_t<In>> &&
        Constructible<iter_value_t<In>, iter_reference_t<In>> &&
        Assignable<iter_value_t<In>&, iter_reference_t<In>>,
    std::true_type>;

}

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyableStorable =
        decltype(detail::IndirectlyCopyableStorable_fn<In, Out>(0))::value;

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/iter_swap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ITER_SWAP_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ITER_SWAP_HPP_INCLUDED




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

NANO_INLINE_VAR(detail::iter_swap_::fn, iter_swap)

NANO_END_NAMESPACE

#endif

// nanorange/detail/iterator/projected.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.projected]

template <typename I, typename Proj>
struct projected;

namespace detail {

template <typename, typename, typename = void>
struct projected_helper {
};

template <typename I, typename Proj>
struct projected_helper<
    I, Proj,
    std::enable_if_t<Readable<I> && IndirectRegularUnaryInvocable<Proj, I>>> {
    using value_type =
        std::remove_cv_t<std::remove_reference_t<indirect_result_t<Proj&, I>>>;

    // We shouldn't need to define this, as we only need its return type,
    // but GCC gets stroppy sometimes.
    indirect_result_t<Proj&, I> operator*() const { throw 0; }
};

template <typename, typename, typename = void>
struct projected_difference_t_helper {};

template <typename I, typename Proj>
struct projected_difference_t_helper<I, Proj, std::enable_if_t<
    WeaklyIncrementable<I>>> {
    using difference_type = iter_difference_t<I>;
};

} // namespace detail

template <typename I, typename Proj>
struct projected : detail::projected_helper<I, Proj> {
};

template <typename I, typename Proj>
struct incrementable_traits<projected<I, Proj>>
    : detail::projected_difference_t_helper<I, Proj> {};

NANO_END_NAMESPACE

#endif

NANO_BEGIN_NAMESPACE

namespace detail {

struct IndirectlySwappable_req {
    template <typename I1, typename I2>
    auto requires_(I1&& i1, I2&& i2) -> decltype(
        ranges::iter_swap(std::forward<I1>(i1), std::forward<I2>(i2)),
        ranges::iter_swap(std::forward<I2>(i2), std::forward<I1>(i1)),
        ranges::iter_swap(std::forward<I1>(i1), std::forward<I1>(i1)),
        ranges::iter_swap(std::forward<I2>(i2), std::forward<I2>(i2)));
};

} // namespace detail

template <typename I1, typename I2 = I1>
NANO_CONCEPT IndirectlySwappable = Readable<I1>&& Readable<I2>&&
    detail::requires_<detail::IndirectlySwappable_req, I1, I2>;

// [range.commonalgoreq.indirectlycomparable]

template <typename I1, typename I2, typename R = equal_to<>,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT IndirectlyComparable =
    IndirectRelation<R, projected<I1, P1>, projected<I2, P2>>;

// [range.commonalgoreq.permutable]

template <typename I>
NANO_CONCEPT Permutable = ForwardIterator<I>&& IndirectlyMovableStorable<I, I>&&
    IndirectlySwappable<I, I>;

// [range.commonalgoreq.mergeable]

template <typename I1, typename I2, typename Out, typename R = less<>,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT Mergeable =
    InputIterator<I1>&& InputIterator<I2>&& WeaklyIncrementable<Out>&&
        IndirectlyCopyable<I1, Out>&& IndirectlyCopyable<I2, Out>&&
            IndirectStrictWeakOrder<R, projected<I1, P1>, projected<I2, P2>>;

// [range.commonalgoreq.sortable]

template <typename I, typename R = std::less<>, typename P = identity>
NANO_CONCEPT Sortable =
    Permutable<I>&& IndirectStrictWeakOrder<R, projected<I, P>>;

NANO_END_NAMESPACE

#endif







// nanorange/detail/ranges/access.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_ACCESS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_ACCESS_HPP_INCLUDED

// nanorange/detail/ranges/begin_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_BEGIN_END_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_BEGIN_END_HPP_INCLUDED

// nanorange/detail/functional/decay_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_DECAY_COPY_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_DECAY_COPY_HPP_INCLUDED



#include <functional>
#include <type_traits>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
constexpr std::decay_t<T> decay_copy(T &&t) noexcept(
    noexcept(static_cast<std::decay_t<T>>(std::forward<T>(t))))
{
    return std::forward<T>(t);
}

} // namespace detail

NANO_END_NAMESPACE

#endif



#ifdef NANO_HAVE_CPP17
#include <string_view>
#endif

NANO_BEGIN_NAMESPACE

// [range.access.begin]

namespace detail {
namespace begin_ {

template <typename T>
void begin(T&&) = delete;

template <typename T>
void begin(std::initializer_list<T>&&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr void impl(T(&&)[N], priority_tag<2>) = delete;

    template <typename T, std::size_t N>
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype((t) + 0)
    {
        return (t) + 0;
    }

    // Specialisation for rvalue string_views in C++17, as we can't add
    // functions to namespace std
#ifdef NANO_HAVE_CPP17
    template <typename C, typename T>
    static constexpr auto
    impl(std::basic_string_view<C, T> sv, priority_tag<2>) noexcept
        -> decltype(sv.begin())
    {
        return sv.begin();
    }
#endif

    template <typename T>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.begin())))
        -> std::enable_if_t<Iterator<decltype(decay_copy(t.begin()))>,
                            decltype(decay_copy(t.begin()))>
    {
        return decay_copy(t.begin());
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(begin(std::forward<T>(t)))))
        -> std::enable_if_t<
            Iterator<decltype(decay_copy(begin(std::forward<T>(t))))>,
            decltype(decay_copy(begin(std::forward<T>(t))))>
    {
        return decay_copy(begin(std::forward<T>(t)));
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

} // namespace begin_
} // namespace detail

NANO_INLINE_VAR(detail::begin_::fn, begin)

namespace detail {
namespace end_ {

template <typename T>
void end(T&&) = delete;

template <typename T>
void end(std::initializer_list<T>&&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr void impl(T(&&)[N], priority_tag<2>) = delete;

    template <typename T, std::size_t N>
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype(t + N)
    {
        return t + N;
    }

#ifdef NANO_HAVE_CPP17
    template <typename C, typename T>
    static constexpr auto
    impl(std::basic_string_view<C, T> sv, priority_tag<2>) noexcept
        -> decltype(sv.end())
    {
        return sv.end();
    }
#endif

    template <typename T,
              typename S = decltype(decay_copy(std::declval<T&>().end())),
              typename I = decltype(ranges::begin(std::declval<T&>()))>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.end())))
        -> std::enable_if_t<Sentinel<S, I>, decltype(decay_copy(t.end()))>
    {
        return decay_copy(t.end());
    }

    template <typename T,
              typename S = decltype(decay_copy(end(std::declval<T>()))),
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(end(std::forward<T>(t)))))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return decay_copy(end(std::forward<T>(t)));
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

} // namespace end_
} // namespace detail

NANO_INLINE_VAR(detail::end_::fn, end)

// [range.access.cbegin]

namespace detail {
namespace cbegin_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::begin(t))) -> decltype(ranges::begin(t))
    {
        return ranges::begin(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::begin(static_cast<const T&&>(t))))
            -> decltype(ranges::begin(static_cast<const T&&>(t)))
    {
        return ranges::begin(static_cast<const T&&>(t));
    }
};

} // namespace cbegin_
} // namespace detail

NANO_INLINE_VAR(detail::cbegin_::fn, cbegin)

// [ranges.access.cend]

namespace detail {
namespace cend_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::end(t))) -> decltype(ranges::end(t))
    {
        return ranges::end(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::end(static_cast<const T&&>(t))))
            -> decltype(ranges::end(static_cast<const T&&>(t)))
    {
        return ranges::end(static_cast<const T&&>(t));
    }
};

} // namespace cend_
} // namespace detail

NANO_INLINE_VAR(detail::cend_::fn, cend)

NANO_END_NAMESPACE

#endif

// nanorange/iterator/reverse_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_REVERSE_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_REVERSE_ITERATOR_HPP_INCLUDED





// nanorange/iterator/operations.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OPERATIONS_HPP_INCLUDED
#define NANORANGE_ITERATOR_OPERATIONS_HPP_INCLUDED



// nanorange/detail/ranges/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED


// nanorange/detail/ranges/primitives.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_PRIMITIVES_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_PRIMITIVES_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.primitives.size]

template <typename T>
constexpr bool disable_sized_range = false;

namespace detail {
namespace size_ {

template <typename T>
void size(T&&) = delete;

// For some reason MSVC doesn't mind poison pills,
// as long as there are two
template <typename T>
void size(T&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr std::size_t impl(const T(&&)[N], priority_tag<3>) noexcept
    {
        return N;
    }

    template <typename T, std::size_t N>
    static constexpr std::size_t impl(const T (&)[N], priority_tag<3>) noexcept
    {
        return N;
    }

    template <typename T,
              typename I = decltype(decay_copy(std::declval<T>().size()))>
    static constexpr auto impl(T&& t, priority_tag<2>) noexcept(
        noexcept(decay_copy(std::forward<T>(t).size())))
        -> std::enable_if_t<
            Integral<I> && !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(std::forward<T>(t).size());
    }

    template <typename T,
              typename I = decltype(decay_copy(size(std::declval<T>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(size(std::forward<T>(t)))))
        -> std::enable_if_t<
            Integral<I> && !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(size(std::forward<T>(t)));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>())),
              typename S = decltype(ranges::end(std::declval<T>())),
              typename D = decltype(decay_copy(std::declval<S>() -
                                               std::declval<I>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(ranges::end(t) - ranges::begin(t))))
        -> std::enable_if_t<
            !std::is_array<remove_cvref_t<T>>::value && // MSVC sillyness?
                SizedSentinel<S, I> && ForwardIterator<I>,
            D>
    {
        return decay_copy(ranges::end(t) - ranges::begin(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<3>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<3>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<3>{});
    }
};

} // namespace size_
} // namespace detail

NANO_INLINE_VAR(detail::size_::fn, size)

// [range.primitives.empty]

namespace detail {
namespace empty_ {

struct fn {
private:
    template <typename T>
    static constexpr auto
    impl(T&& t,
         priority_tag<2>) noexcept(noexcept((bool(std::forward<T>(t).empty()))))
        -> decltype((bool(std::forward<T>(t).empty())))
    {
        return bool((std::forward<T>(t).empty()));
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(ranges::size(std::forward<T>(t)) == 0))
        -> decltype(ranges::size(std::forward<T>(t)) == 0)
    {
        return ranges::size(std::forward<T>(t)) == 0;
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto
    impl(T&& t,
         priority_tag<0>) noexcept(noexcept(ranges::begin(t) == ranges::end(t)))
        -> std::enable_if_t<ForwardIterator<I>,
                            decltype(ranges::begin(t) == ranges::end(t))>
    {
        return ranges::begin(t) == ranges::end(t);
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

} // namespace empty_
} // namespace detail

NANO_INLINE_VAR(detail::empty_::fn, empty)

namespace detail {

template <typename P>
constexpr bool is_object_pointer_v =
    std::is_pointer<P>::value && std::is_object<test_t<iter_value_t, P>>::value;

namespace data_ {

struct fn {
private:
    template <typename T, typename D = decltype(decay_copy(std::declval<T&>().data()))>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.data())))
        -> std::enable_if_t<is_object_pointer_v<D>, D>
    {
        return t.data();
    }

    template <typename T>
    static constexpr auto
    impl(T&& t,
         priority_tag<0>) noexcept(noexcept(ranges::begin(std::forward<T>(t))))
        -> std::enable_if_t<
            is_object_pointer_v<decltype(ranges::begin(std::forward<T>(t)))>,
            decltype(ranges::begin(std::forward<T>(t)))>
    {
        return ranges::begin(std::forward<T>(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<1>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<1>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<1>{});
    }
};

} // namespace data_
} // namespace detail

NANO_INLINE_VAR(detail::data_::fn, data)

NANO_END_NAMESPACE

#endif

// These are, sadly, needed for view-predicate specialisations,
// because we're not allowed to forward-declare std classes

#include <initializer_list>
#include <set>
#include <unordered_set>

NANO_BEGIN_NAMESPACE

template <typename T>
using iterator_t = decltype(ranges::begin(std::declval<T&>()));

template <typename T>
using sentinel_t = decltype(ranges::end(std::declval<T&>()));

template <typename T>
struct enable_view {
};

struct view_base {
};

// [range.range]

namespace detail {

struct Range_req {
    template <typename T>
    auto requires_(T&& t)
        -> decltype(valid_expr(ranges::begin(std::forward<T>(t)),
                               ranges::end(std::forward<T>(t))));
};

template <typename T>
NANO_CONCEPT RangeImpl = requires_<Range_req, T>;

template <typename>
auto Range_fn(long) -> std::false_type;

template <typename T>
auto Range_fn(int) -> std::enable_if_t<RangeImpl<T&>, std::true_type>;

} // namespace detail

template <typename T>
NANO_CONCEPT Range = decltype(detail::Range_fn<T>(0))::value;

namespace detail {

template <typename T>
NANO_CONCEPT ForwardingRange = Range<T> && RangeImpl<T>;

}

// [range.sized]

namespace detail {

template <typename T, typename Deduced>
auto convertible_to_helper(Deduced)
    -> std::enable_if_t<ConvertibleTo<Deduced, T>, int>;

struct SizedRange_req {
    template <typename T>
    auto requires_(T& t) -> decltype(
        valid_expr(convertible_to_helper<iter_difference_t<iterator_t<T>>>(
            ranges::size(t))));
};

} // namespace detail

template <typename T>
NANO_CONCEPT SizedRange =
    Range<T> &&
    !disable_sized_range<std::remove_cv_t<std::remove_reference_t<T>>> &&
    detail::requires_<detail::SizedRange_req, T>;

// [range.view]

namespace detail {

template <typename, typename = void>
struct view_predicate : std::true_type {};

template <typename T>
constexpr bool view_predicate_v = view_predicate<T>::value;

template <typename T>
using enable_view_t = typename enable_view<T>::type;

template <typename T>
constexpr bool has_enable_view_v = exists_v<enable_view_t, T>;

template <typename T>
struct view_predicate<T, std::enable_if_t<has_enable_view_v<T>>> {
    static constexpr bool value = enable_view<T>::type::value;
};

template <typename T>
struct view_predicate<
    T, std::enable_if_t<!has_enable_view_v<T> && DerivedFrom<T, view_base>>>
    : std::true_type {};

template <typename T>
struct view_predicate<std::initializer_list<T>> : std::false_type {};

template <typename K, typename C, typename A>
struct view_predicate<std::set<K, C, A>> : std::false_type {};

template <typename K, typename C, typename A>
struct view_predicate<std::multiset<K, C, A>>  : std::false_type {};

template <typename K, typename H, typename E, typename A>
struct view_predicate<std::unordered_set<K, H, E, A>> : std::false_type {};

template <typename K, typename H, typename E, typename A>
struct view_predicate<std::unordered_multiset<K, H, E, A>> : std::false_type {};

template <typename>
auto view_predicate_helper_fn(long) -> std::false_type;

template <typename T>
auto view_predicate_helper_fn(int) -> std::enable_if_t<
        !has_enable_view_v<T> &&
        !DerivedFrom<T, view_base> &&
        Range<T> &&
        Range<const T> &&
        !Same<iter_reference_t<iterator_t<T>>, iter_reference_t<iterator_t<const T>>>,
    std::true_type>;

template <typename T>
constexpr bool view_predicate_helper =
    decltype(view_predicate_helper_fn<T>(0))::value;

template <typename T>
struct view_predicate<T, std::enable_if_t<view_predicate_helper<T>>>
   : std::false_type {};

} // namespace detail

template <typename T>
NANO_CONCEPT View = Range<T>&& Semiregular<T>&& detail::view_predicate_v<T>;

// [range.common]
namespace detail {

template <typename>
auto CommonRange_fn(long) -> std::false_type;

template <typename T>
auto CommonRange_fn(int) -> std::enable_if_t<
    Range<T> &&
    Same<iterator_t<T>, sentinel_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT CommonRange = decltype(detail::CommonRange_fn<T>(0))::value;

// [ranges.viewable]

template <typename T>
NANO_CONCEPT ViewableRange = Range<T> && (detail::ForwardingRange<T> ||
                                          View<std::decay_t<T>>);

// [range.input]

namespace detail {

template <typename>
auto InputRange_fn(long) -> std::false_type;

template <typename T>
auto InputRange_fn(int) -> std::enable_if_t<
        Range<T> &&
        InputIterator<iterator_t<T>>,
    std::true_type>;

}

template <typename T>
NANO_CONCEPT InputRange =
    decltype(detail::InputRange_fn<T>(0))::value;

namespace detail {

template <typename, typename >
auto OutputRange_fn(long) -> std::false_type;

template <typename R, typename T>
auto OutputRange_fn(int) -> std::enable_if_t<
        Range<R> && OutputIterator<iterator_t<R>, T>,
        std::true_type>;

}

template <typename R, typename T>
NANO_CONCEPT OutputRange =
    decltype(detail::OutputRange_fn<R, T>(0))::value;

namespace detail {

template <typename>
auto ForwardRange_fn(long) -> std::false_type;

template <typename T>
auto ForwardRange_fn(int) -> std::enable_if_t<
        InputRange<T> && ForwardIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT ForwardRange =
    decltype(detail::ForwardRange_fn<T>(0))::value;

namespace detail {

template <typename>
auto BidirectionalRange_fn(long) -> std::false_type;

template <typename T>
auto BidirectionalRange_fn(int) -> std::enable_if_t<
        ForwardRange<T> && BidirectionalIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT BidirectionalRange =
    decltype(detail::BidirectionalRange_fn<T>(0))::value;

namespace detail {

template <typename>
auto RandomAccessRange_fn(long) -> std::false_type;

template <typename T>
auto RandomAccessRange_fn(int) -> std::enable_if_t<
        BidirectionalRange<T> && RandomAccessIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT RandomAccessRange =
    decltype(detail::RandomAccessRange_fn<T>(0))::value;

namespace detail {

// Not to spec: P0944 requires that R's iterator_t models ContiguousIterator,
// but we only require RandomAccessIterator.
// This is so that std::vector, std::string etc can model ContiguousRange
struct ContiguousRange_req {
    template <typename R>
    auto requires_(R& r) -> decltype(
        requires_expr<Same<decltype(ranges::data(r)), std::add_pointer_t<iter_reference_t<iterator_t<R>>>>>{}
    );
};


template <typename>
auto ContiguousRange_fn(long) -> std::false_type;

template <typename R>
auto ContiguousRange_fn(int) -> std::enable_if_t<
        Range<R> && RandomAccessIterator<iterator_t<R>> &&
        requires_<ContiguousRange_req, R>,
                std::true_type>;

}

template <typename R>
NANO_CONCEPT ContiguousRange =
    decltype(detail::ContiguousRange_fn<R>(0))::value;

template <typename R>
using safe_iterator_t = std::enable_if_t<Range<R>,
        decltype(ranges::begin(std::declval<R>()))>;

NANO_END_NAMESPACE

#endif



NANO_BEGIN_NAMESPACE

namespace detail {
namespace advance_ {

struct fn {
private:
    template <typename T>
    static constexpr T abs(T t)
    {
        if (t < T{0}) {
            return -t;
        }
        return t;
    }

    template <typename R>
    static constexpr auto impl(R& r, iter_difference_t<R> n)
        -> std::enable_if_t<RandomAccessIterator<R>>
    {
        r += n;
    }

    template <typename I>
    static constexpr auto impl(I& i, iter_difference_t<I> n)
        -> std::enable_if_t<BidirectionalIterator<I> &&
                            !RandomAccessIterator<I>>
    {
        constexpr auto zero = iter_difference_t<I>{0};

        if (n > zero) {
            while (n-- > zero) {
                ++i;
            }
        } else {
            while (n++ < zero) {
                --i;
            }
        }
    }

    template <typename I>
    static constexpr auto impl(I& i, iter_difference_t<I> n)
        -> std::enable_if_t<!BidirectionalIterator<I>>
    {
        while (n-- > iter_difference_t<I>{0}) {
            ++i;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, S bound, priority_tag<2>)
        -> std::enable_if_t<Assignable<I&, S>>
    {
        i = std::move(bound);
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, S bound, priority_tag<1>)
        -> std::enable_if_t<SizedSentinel<S, I>>
    {
        fn::impl(i, bound - i);
    }

    template <typename I, typename S>
    static constexpr void impl(I& i, S bound, priority_tag<0>)
    {
        while (i != bound) {
            ++i;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<SizedSentinel<S, I>, iter_difference_t<I>>
    {
        if (fn::abs(n) >= fn::abs(bound - i)) {
            auto dist = bound - i;
            fn::impl(i, bound, priority_tag<2>{});
            return dist;
        } else {
            fn::impl(i, n);
            return n;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<BidirectionalIterator<I> && !SizedSentinel<S, I>,
                            iter_difference_t<I>>
    {
        constexpr iter_difference_t<I> zero{0};
        iter_difference_t<I> counter{0};

        if (n < zero) {
            do {
                --i;
                --counter; // Yes, really
            } while (++n < zero && i != bound);
        } else {
            while (n-- > zero && i != bound) {
                ++i;
                ++counter;
            }
        }

        return counter;
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<!BidirectionalIterator<I> && !SizedSentinel<S, I>,
                            iter_difference_t<I>>
    {
        constexpr iter_difference_t<I> zero{0};
        iter_difference_t<I> counter{0};

        while (n-- > zero && i != bound) {
            ++i;
            ++counter;
        }

        return counter;
    }

public:
    template <typename I>
    constexpr auto operator()(I& i, iter_difference_t<I> n) const
        -> std::enable_if_t<Iterator<I>>
    {
        fn::impl(i, n);
    }

    template <typename I, typename S>
    constexpr auto operator()(I& i, S bound) const
        -> std::enable_if_t<Iterator<I> && Sentinel<S, I>>
    {
        fn::impl(i, bound, priority_tag<2>{});
    }

    template <typename I, typename S>
    constexpr auto operator()(I& i, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<Iterator<I> && Sentinel<S, I>, iter_difference_t<I>>
    {
        return n - fn::impl(i, n, bound);
    }
};

} // namespace advance_
} // namespace detail

NANO_INLINE_VAR(detail::advance_::fn, advance)

namespace detail {
namespace distance_ {

struct fn {
private:
    template <typename I, typename S>
    static constexpr auto impl(I i, S s)
        -> std::enable_if_t<SizedSentinel<S, I>, iter_difference_t<I>>
    {
        return s - i;
    }

    template <typename I, typename S>
    static constexpr auto impl(I i, S s)
        -> std::enable_if_t<!SizedSentinel<S, I>, iter_difference_t<I>>
    {
        iter_difference_t<I> counter{0};
        while (i != s) {
            ++i;
            ++counter;
        }
        return counter;
    }

    template <typename R>
    static constexpr auto impl(R&& r)
        -> std::enable_if_t<SizedRange<R>, iter_difference_t<iterator_t<R>>>
    {
        return static_cast<iter_difference_t<iterator_t<R>>>(ranges::size(r));
    }

    template <typename R>
    static constexpr auto impl(R&& r)
        -> std::enable_if_t<!SizedRange<R>, iter_difference_t<iterator_t<R>>>
    {
        return fn::impl(ranges::begin(r), ranges::end(r));
    }

public:
    template <typename I, typename S>
    constexpr auto operator()(I first, S last) const
        -> std::enable_if_t<Iterator<I> && Sentinel<S, I>, iter_difference_t<I>>
    {
        return fn::impl(std::move(first), std::move(last));
    }

    template <typename R>
    constexpr auto operator()(R&& r) const
        -> std::enable_if_t<Range<R>, iter_difference_t<iterator_t<R>>>
    {
        return fn::impl(std::forward<R>(r));
    }
};

} // namespace distance_
} // namespace detail

NANO_INLINE_VAR(detail::distance_::fn, distance)

namespace detail {
namespace next_ {

struct fn {
    template <typename I>
    constexpr auto operator()(I x) const -> std::enable_if_t<Iterator<I>, I>
    {
        ++x;
        return x;
    }

    template <typename I>
    constexpr auto operator()(I x, iter_difference_t<I> n) const
        -> std::enable_if_t<Iterator<I>, I>
    {
        ranges::advance(x, n);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, S bound) const
        -> std::enable_if_t<Iterator<I> && Sentinel<S, I>, I>
    {
        ranges::advance(x, bound);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<Iterator<I> && Sentinel<S, I>, I>
    {
        ranges::advance(x, n, bound);
        return x;
    }
};

} // namespace next_
} // namespace detail

NANO_INLINE_VAR(detail::next_::fn, next)

namespace detail {
namespace prev_ {

struct fn {
    template <typename I>
    constexpr auto operator()(I x) const
        -> std::enable_if_t<BidirectionalIterator<I>, I>
    {
        --x;
        return x;
    }

    template <typename I>
    constexpr auto operator()(I x, iter_difference_t<I> n) const
        -> std::enable_if_t<BidirectionalIterator<I>, I>
    {
        ranges::advance(x, -n);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<BidirectionalIterator<I> && Sentinel<S, I>, I>
    {
        ranges::advance(x, -n, bound);
        return x;
    }
};

} // namespace prev_
} // namespace detail

NANO_INLINE_VAR(detail::prev_::fn, prev)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace reverse_iterator_ {

template <typename I>
class reverse_iterator {

    static_assert(BidirectionalIterator<I>,
                  "Template argument to reverse_iterator must model "
                  "BidirectionalIterator");

public:
    using iterator_type = I;
    using difference_type = iter_difference_t<I>;
    using value_type = iter_value_t<I>;
    using iterator_category = detail::legacy_iterator_category_t<I>;
    using reference = iter_reference_t<I>;
    using pointer = I;

    constexpr reverse_iterator() = default;

    explicit constexpr reverse_iterator(I x) : current_(std::move(x)) {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, I>, int> = 0>

    constexpr reverse_iterator(const reverse_iterator<U>& i)
        : current_(i.base())
    {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, I>, int> = 0>

    constexpr reverse_iterator& operator=(const reverse_iterator<U>& i)
    {
        current_ = i.base();
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr reference operator*() const { return *prev(current_); }

    constexpr pointer operator->() const { return prev(current_); }

    constexpr reverse_iterator& operator++()
    {
        --current_;
        return *this;
    }

    constexpr reverse_iterator operator++(int)
    {
        reverse_iterator tmp = *this;
        --current_;
        return tmp;
    }

    constexpr reverse_iterator& operator--()
    {
        ++current_;
        return *this;
    }

    constexpr reverse_iterator operator--(int)
    {
        reverse_iterator tmp = *this;
        ++current_;
        return tmp;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator>
    operator+(difference_type n) const
    {
        return reverse_iterator(current_ - n);
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator&>
    operator+=(difference_type n)
    {
        current_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator>
    operator-(difference_type n) const
    {
        return reverse_iterator(current_ + n);
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator&>
    operator-=(difference_type n)
    {
        current_ += n;
        return *this;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reference>
    operator[](difference_type n) const
    {
        return current_[-n - 1];
    }

    friend constexpr iter_rvalue_reference_t<I>
    iter_move(const reverse_iterator& i) noexcept(
        noexcept(ranges::iter_move(std::declval<I&>())) &&
        noexcept(--std::declval<I&>()) &&
        std::is_nothrow_copy_constructible<I>::value)
    {
        return ranges::iter_move(prev(i.current_));
    }

    template <typename I2>
    friend constexpr auto
    iter_swap(const reverse_iterator& x,
              const reverse_iterator<I2>&
                  y) noexcept(noexcept(ranges::iter_swap(std::declval<I>(),
                                                         std::declval<I>())) &&
                              noexcept(--std::declval<I&>()))
        -> std::enable_if_t<IndirectlySwappable<I2, I>>

    {
        ranges::iter_swap(prev(x.current_), prev(y.base()));
    }

private:
    I current_{};
};

template <typename I1, typename I2>
constexpr std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
operator==(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() == y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
operator!=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() != y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator<(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() > y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator>(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() < y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator>=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() <= y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator<=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() >= y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<SizedSentinel<I1, I2>, iter_difference_t<I2>>
operator-(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return y.base() - x.base();
}

template <typename I>
constexpr std::enable_if_t<RandomAccessIterator<I>, reverse_iterator<I>>
operator+(iter_difference_t<I> n, const reverse_iterator<I>& x)
{
    return reverse_iterator<I>(x.base() - n);
}

} // namespace reverse_iterator_

using reverse_iterator_::reverse_iterator;

template <typename I>
constexpr std::enable_if_t<BidirectionalIterator<I>, reverse_iterator<I>>
make_reverse_iterator(I i)
{
    return reverse_iterator<I>(std::move(i));
}

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {
namespace rbegin_ {

template <typename T>
void rbegin(T&&) = delete;

template <typename T>
void rbegin(std::initializer_list<T>) = delete;

struct fn {
private:
    template <typename T,
              typename I = decltype(decay_copy(std::declval<T&>().rbegin()))>
    static constexpr auto
    impl(T& t, priority_tag<2>) noexcept(noexcept(decay_copy(t.rbegin())))
        -> std::enable_if_t<Iterator<I>, I>
    {
        return t.rbegin();
    }

    template <typename T,
              typename I = decltype(decay_copy(rbegin(std::declval<T&&>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rbegin(std::forward<T>(t)))))
        -> std::enable_if_t<Iterator<I>, I>
    {
        return rbegin(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(ranges::end(std::declval<T&&>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(ranges::make_reverse_iterator(ranges::end(std::forward<T>(t)))))
        -> std::enable_if_t<Same<I, S> && BidirectionalIterator<I>,
                            decltype(ranges::make_reverse_iterator(
                                ranges::end(std::forward<T>(t))))>
    {
        return ranges::make_reverse_iterator(ranges::end(std::forward<T>(t)));
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

} // namespace rbegin_
} // namespace detail

NANO_INLINE_VAR(detail::rbegin_::fn, rbegin)

namespace detail {
namespace rend_ {

template <typename T>
void rend(T&&) = delete;

template <typename T>
void rend(std::initializer_list<T>) = delete;

struct fn {
private:
    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&>())),
              typename S = decltype(decay_copy(std::declval<T&>().rend()))>
    static constexpr auto
    impl(T& t, priority_tag<2>) noexcept(noexcept(decay_copy(t.rend())))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return t.rend();
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(decay_copy(rend(std::declval<T&&>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rend(std::forward<T>(t)))))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return rend(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(ranges::end(std::declval<T&&>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(ranges::make_reverse_iterator(ranges::begin(std::forward<T>(t)))))
        -> std::enable_if_t<Same<I, S> && BidirectionalIterator<I>,
                            decltype(ranges::make_reverse_iterator(
                                ranges::begin(std::forward<T>(t))))>
    {
        return ranges::make_reverse_iterator(ranges::begin(std::forward<T>(t)));
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

} // namespace rend_
} // namespace detail

NANO_INLINE_VAR(detail::rend_::fn, rend)

namespace detail {
namespace crbegin_ {

struct fn {
    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::rbegin(t))) -> decltype(ranges::rbegin(t))
    {
        return ranges::rbegin(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::rbegin(static_cast<const T&&>(t))))
            -> decltype(ranges::rbegin(static_cast<const T&&>(t)))
    {
        return ranges::rbegin(static_cast<const T&&>(t));
    }
};

} // namespace crbegin_
} // namespace detail

NANO_INLINE_VAR(detail::crbegin_::fn, crbegin)

namespace detail {
namespace crend_ {

struct fn {
    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::rend(t))) -> decltype(ranges::rend(t))
    {
        return ranges::rend(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::rend(static_cast<const T&&>(t))))
            -> decltype(ranges::rend(static_cast<const T&&>(t)))
    {
        return ranges::rend(static_cast<const T&&>(t));
    }
};

} // namespace crend_
} // namespace detail

NANO_INLINE_VAR(detail::crend_::fn, crend)

NANO_END_NAMESPACE

#endif




// nanorange/functional.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_FUNCTIONAL_HPP_INCLUDED
#define NANORANGE_FUNCTIONAL_HPP_INCLUDED





#endif

// nanorange/ranges/istream_range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANGES_ISTREAM_RANGE_HPP_INCLUDED
#define NANORANGE_RANGES_ISTREAM_RANGE_HPP_INCLUDED


// nanorange/iterator/default_sentinel.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_DEFAULT_SENTINEL_HPP_INCLUDED
#define NANORANGE_ITERATOR_DEFAULT_SENTINEL_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

class default_sentinel {};

NANO_END_NAMESPACE

#endif


#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>,
          typename Distance = std::ptrdiff_t>
class istream_range {
public:

    class iterator {
    public:
        using iterator_category = ranges::input_iterator_tag;
        using difference_type = Distance;
        using value_type = T;
        using reference = const T&;
        using pointer = const T*;
        using char_type = CharT;
        using traits = Traits;

        constexpr iterator() = default;

        explicit iterator(istream_range& rng)
            : rng_(std::addressof(rng))
        {}

        reference operator*() const { return rng_->value_; }

        pointer operator->() const { return std::addressof(rng_->value_); }

        iterator& operator++()
        {
            rng_->next();
            return *this;
        }

        iterator operator++(int)
        {
            auto tmp = *this;
            this->operator++();
            return tmp;
        }

        friend bool operator==(const iterator& lhs, const iterator& rhs)
        {
            return lhs.rng_ == rhs.rng_;
        }

        friend bool operator!=(const iterator& lhs, const iterator& rhs)
        {
            return !(lhs == rhs);
        }

        friend bool operator==(const iterator& lhs, default_sentinel)
        {
            return lhs.done();
        }

        friend bool operator!=(const iterator& lhs, default_sentinel rhs)
        {
            return !(lhs == rhs);
        }

        friend bool operator==(default_sentinel, const iterator& rhs)
        {
            return rhs.done();
        }

        friend bool operator!=(default_sentinel lhs, const iterator& rhs)
        {
            return !(lhs == rhs);
        }

    private:
        bool done() const
        {
            return rng_ == nullptr || rng_->in_stream_ == nullptr;
        }

        istream_range* rng_ = nullptr;
    };

    using sentinel = default_sentinel;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istream_range() = default;

    explicit istream_range(istream_type& s)
        : in_stream_(std::addressof(s))
    {
        next();
    }

    iterator begin() { return iterator{*this}; }

    sentinel end() { return {}; }

private:
    void next()
    {
        if (!(*in_stream_ >> value_)) {
            in_stream_ = nullptr;
        }
    }

    istream_type* in_stream_ = nullptr;
    T value_{};
};

NANO_END_NAMESPACE

#endif


#endif

NANO_BEGIN_NAMESPACE

// [range.alg.adjacent.find]

namespace detail {

struct adjacent_find_fn {
private:
    friend struct unique_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I next = first;
        ++next;

        while (next != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first),
                             nano::invoke(proj, *next))) {
                return first;
            }
            ++first;
            ++next;
        }

        return next;
    }

public:
    template <typename I, typename S, typename Proj = identity,
              typename Pred = equal_to<>>
    constexpr std::enable_if_t<ForwardIterator<I> && Sentinel<S, I> &&
                                   IndirectRelation<Pred, projected<I, Proj>>,
                               I>
    operator()(I first, S last, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return adjacent_find_fn::impl(std::move(first), std::move(last),
                                      pred, proj);
    }

    template <typename Rng, typename Proj = identity,
              typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
            IndirectRelation<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return adjacent_find_fn::impl(nano::begin(rng), nano::end(rng),
                                      pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::adjacent_find_fn, adjacent_find)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/all_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ALL_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ALL_OF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.alg.all_of]

namespace detail {

struct all_of_fn {
private:
    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr bool impl(I first, S last, Pred& pred, Proj& proj)
    {
        while (first != last) {
            if (!nano::invoke(pred, nano::invoke(proj, *first))) {
                return false;
            }
            ++first;
        }
        return true;
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return all_of_fn::impl(std::move(first), std::move(last),
                               pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return all_of_fn::impl(nano::begin(rng), nano::end(rng),
                               pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::all_of_fn, all_of)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/any_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ANY_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ANY_OF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [ranges.alg.any_of]

namespace detail {

struct any_of_fn {
private:
    friend struct is_permutation_fn;
    friend struct none_of_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr bool impl(I first, S last, Pred& pred, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first)) == true) {
                return true;
            }
            ++first;
        }
        return false;
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(std::move(first), std::move(last),
                               pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(nano::begin(rng), nano::end(rng),
                               pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::any_of_fn, any_of)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/binary_search.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_BINARY_SEARCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_BINARY_SEARCH_HPP_INCLUDED

// nanorange/algorithm/lower_bound.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_LOWER_BOUND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_LOWER_BOUND_HPP_INCLUDED

// nanorange/algorithm/partition_point.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2

//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015


//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//

#ifndef NANORANGE_ALGORITHM_PARTITION_POINT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_POINT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_point_fn {
private:
    friend struct lower_bound_fn;
    friend struct upper_bound_fn;

    template <typename I, typename Pred, typename Proj>
    static constexpr I impl_n(I first, iter_difference_t<I> n, Pred& pred,
                              Proj& proj)
    {
        while (n != 0) {
            const auto half = n/2;

            auto middle = nano::next(first, half);

            if (nano::invoke(pred, nano::invoke(proj, *middle))) {
                first = std::move(++middle);
                n -= half + 1;
            } else {
                n = half;
            }
        }

        return first;
    }

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr std::enable_if_t<SizedSentinel<S, I>, I>
    impl(I first, S last, Pred& pred, Proj& proj)
    {
        const auto n = nano::distance(first, std::move(last));
        return partition_point_fn::impl_n(std::move(first), n, pred, proj);
    }

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr std::enable_if_t<!SizedSentinel<S, I>, I>
    impl(I first, S last, Pred& pred, Proj& proj)
    {
        // Probe exponentially for either end-of-range or an iterator
        // that is past the partition point (i.e., does not satisfy pred).
        iter_difference_t<I> n{1};

        while (true) {
            auto m = first;
            auto d = nano::advance(m, n, last);
            if (m == last || !nano::invoke(pred, nano::invoke(proj, *m))) {
                n -= d;
                return partition_point_fn::impl_n(std::move(first), n,
                                                  pred, proj);
            }
            first = std::move(m);
            n *= 2;
        }
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
    constexpr operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return partition_point_fn::impl(std::move(first), std::move(last),
                                        pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return partition_point_fn::impl(nano::begin(rng), nano::end(rng),
                                        pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::partition_point_fn, partition_point)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct lower_bound_fn {
private:
    friend struct binary_search_fn;
    friend struct equal_range_fn;

    template <typename Comp, typename T>
    struct compare {
        Comp& comp;
        const T& val;

        template <typename U>
        constexpr bool operator()(U&& u) const
        {
            return nano::invoke(comp, std::forward<U>(u), val);
        }
    };


    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, const T& value, Comp& comp, Proj& proj)
    {
        const auto comparator = compare<Comp, T>{comp, value};
        return partition_point_fn::impl(std::move(first), std::move(last),
                                        comparator, proj);
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
        I>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return lower_bound_fn::impl(std::move(first), std::move(last),
                                    value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return lower_bound_fn::impl(nano::begin(rng), nano::end(rng),
                                    value, comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::lower_bound_fn, lower_bound)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct binary_search_fn {
private:
    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr bool impl(I first, S last, const T& value, Comp& comp,
                               Proj& proj)
    {
        first = lower_bound_fn::impl(std::move(first), last, value, comp, proj);
        return (first != last && !nano::invoke(comp, value, nano::invoke(proj, *first)));
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
       ForwardIterator<I> &&
       Sentinel<S, I> &&
       IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    bool>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return binary_search_fn::impl(std::move(first), std::move(last),
                                      value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    bool>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return binary_search_fn::impl(nano::begin(rng), nano::end(rng),
                                      value, comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::binary_search_fn, binary_search)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_COPY_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename I, typename O>
struct copy_result {
    I in;
    O out;
};

namespace detail {

struct copy_fn {
private:
    // If we know the distance between first and last, we can use that
    // information to (potentially) allow better codegen
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<SizedSentinel<S, I>, copy_result<I, O>>
    impl(I first, S last, O result, priority_tag<1>)
    {
        const auto dist = last - first;

        for (iter_difference_t<I> i = 0; i < dist; ++i) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr copy_result<I, O> impl(I first, S last, O result,
                                            priority_tag<0>)
    {
        while (first != last) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<InputIterator<I> && Sentinel<S, I> &&
                                   WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<I, O>,
                               copy_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return copy_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<InputRange<Rng> && WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<iterator_t<Rng>, O>,
                               copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return copy_fn::impl(nano::begin(rng), nano::end(rng),
                             std::move(result), priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_fn, copy)

template <typename I, typename O>
using copy_n_result = copy_result<I, O>;

namespace detail {

struct copy_n_fn {
    template <typename I, typename O>
    constexpr std::enable_if_t<InputIterator<I> && WeaklyIncrementable<O> &&
                                   IndirectlyCopyable<I, O>,
                               copy_n_result<I, O>>
    operator()(I first, iter_difference_t<I> n, O result) const
    {
        for (iter_difference_t<I> i{}; i < n; i++) {
            *result = *first;
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_n_fn, copy_n)

template <typename I, typename O>
using copy_if_result = copy_result<I, O>;

namespace detail {

struct copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename Proj>
    static constexpr copy_if_result<I, O> impl(I first, S last, O result,
                                               Pred pred, Proj proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                *result = *first;
                ++result;
            }
            ++first;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename Proj = identity,
              typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && WeaklyIncrementable<O> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>> &&
            IndirectlyCopyable<I, O>,
        copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred, Proj proj = Proj{}) const
    {
        return copy_if_fn::impl(std::move(first), std::move(last),
                                std::move(result), std::move(pred),
                                std::move(proj));
    }

    template <typename Rng, typename O, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> && WeaklyIncrementable<O> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, Proj proj = Proj{}) const
    {
        return copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                std::move(result), std::move(pred),
                                std::move(proj));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_if_fn, copy_if)

template <typename I, typename O>
using copy_backward_result = copy_result<I, O>;

namespace detail {

struct copy_backward_fn {
private:
    template <typename I1, typename S1, typename I2>
    static constexpr copy_backward_result<I1, I2>
    impl(I1 first, S1 last, I2 result)
    {
        I1 last_it = nano::next(first, std::move(last));
        I1 it = last_it;

        while (it != first) {
            *--result = *--it;
        }

        return {std::move(last_it), std::move(result)};
    }

public:
    template <typename I1, typename S1, typename I2>
    constexpr std::enable_if_t<BidirectionalIterator<I1> && Sentinel<S1, I1> &&
                                   BidirectionalIterator<I2> &&
                                   IndirectlyCopyable<I1, I2>,
                               copy_backward_result<I1, I2>>
    operator()(I1 first, S1 last, I2 result) const
    {
        return copy_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename I>
    constexpr std::enable_if_t<BidirectionalRange<Rng> &&
                                   BidirectionalIterator<I> &&
                                   IndirectlyCopyable<iterator_t<Rng>, I>,
                               copy_backward_result<safe_iterator_t<Rng>, I>>
    operator()(Rng&& rng, I result) const
    {
        return copy_backward_fn::impl(nano::begin(rng), nano::end(rng),
                                      std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::copy_backward_fn, copy_backward)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/count.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_COUNT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_COUNT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [rng.alg.count]

namespace detail {

struct count_if_fn {
private:
    friend struct count_fn;
    friend struct is_permutation_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr iter_difference_t<I> impl(I first, S last, Pred& pred,
                                               Proj& proj)
    {
        iter_difference_t<I> counter = 0;

        for (; first != last; ++first) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                ++counter;
            }
        }

        return counter;
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        iter_difference_t<I>>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return count_if_fn::impl(std::move(first), std::move(last),
                                 pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        iter_difference_t<iterator_t<Rng>>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return count_if_fn::impl(nano::begin(rng), nano::end(rng),
                                 pred, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::count_if_fn, count_if)

namespace detail {

struct count_fn {
private:
    template <typename ValueType>
    struct equal_to_pred {
        const ValueType& val;

        template <typename T>
        constexpr bool operator()(const T& t) const
        {
            return t == val;
        }
    };

public:
    template <typename I, typename S, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
        iter_difference_t<I>>
    operator()(I first, S last, const T& value, Proj proj = Proj{}) const
    {
        const auto pred = equal_to_pred<T>{value};
        return count_if_fn::impl(std::move(first), std::move(last),
                                 pred, proj);
    }

    template <typename Rng, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T*>,
        iter_difference_t<iterator_t<Rng>>>
    operator()(Rng&& rng, const T& value, Proj proj = Proj{}) const
    {
        const auto pred = equal_to_pred<T>{value};
        return count_if_fn::impl(nano::begin(rng), nano::end(rng),
                                 pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::count_fn, count)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/equal.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_EQUAL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_EQUAL_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct equal_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr bool impl4(I1 first1, S1 last1, I2 first2, S2 last2,
                                Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2) {
            if (!nano::invoke(pred, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                return false;
            }
            ++first1;
            ++first2;
        }

        return first1 == last1 && first2 == last2;
    }

    template <typename I1, typename S1, typename I2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr bool impl3(I1 first1, S1 last1, I2 first2, Pred pred,
                                Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1) {
            if (!nano::invoke(pred, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                return false;
            }
            ++first1;
            ++first2;
        }

        return true;
    }

public:
    // Four-legged, sized sentinels
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<I2> &&
            Sentinel<S2, I2> &&
            IndirectlyComparable<I1, I2, Pred, Proj1, Proj2> &&
            SizedSentinel<S1, I1> && SizedSentinel<S2, I2>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if (last1 - first1 != last2 - first2) {
            return false;
        }

        // Ranges are the same size, so call the 3-legged version
        // and save ourselves a comparison
        return equal_fn::impl3(std::move(first1), std::move(last1),
                               std::move(first2), pred,
                               proj1, proj2);
    }

    // Four-legged, unsized sentinels
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<I2> &&
            Sentinel<S2, I2> &&
            IndirectlyComparable<I1, I2, Pred, Proj1, Proj2> &&
            !(SizedSentinel<S1, I1> && SizedSentinel<S2, I2>),
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return equal_fn::impl4(std::move(first1), std::move(last1),
                               std::move(first2), std::move(last2),
                               pred, proj1, proj2);
    }

    // Three legged
    template <typename I1, typename S1, typename I2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<std::decay_t<I2>> &&
                !InputRange<I2> &&
            IndirectlyComparable<I1, std::decay_t<I2>, Pred, Proj1, Proj2>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return equal_fn::impl3(std::move(first1), std::move(last1),
                               std::forward<I2>(first2), pred, proj1, proj2);
    }

    // Two ranges, both sized
    template <typename Rng1, typename Rng2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> && InputRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred,
                                 Proj1, Proj2> &&
            SizedRange<Rng1> && SizedRange<Rng2>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if (nano::distance(rng1) != nano::distance(rng2)) {
            return false;
        }

        return equal_fn::impl3(nano::begin(rng1), nano::end(rng1),
                               nano::begin(rng2), pred, proj1, proj2);
    }

    // Two ranges, not both sized
    template <typename Rng1, typename Rng2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> && InputRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred,
                                 Proj1, Proj2> &&
            !(SizedRange<Rng1> && SizedRange<Rng2>),
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return equal_fn::impl4(nano::begin(rng1), nano::end(rng1),
                               nano::begin(rng2), nano::end(rng2),
                               pred, proj1, proj2);
    }

    // Range and a half
    template <typename Rng1, typename I2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputRange<Rng1> && InputIterator<std::decay_t<I2>> &&
                !InputRange<I2> &&
            IndirectlyComparable<iterator_t<Rng1>, std::decay_t<I2>, Pred, Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return equal_fn::impl3(nano::begin(rng1), nano::end(rng1),
                               std::forward<I2>(first2), pred, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::equal_fn, equal)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/equal_range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_EQUAL_RANGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_EQUAL_RANGE_HPP_INCLUDED


// nanorange/algorithm/upper_bound.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UPPER_BOUND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UPPER_BOUND_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct upper_bound_fn {
private:
    friend struct equal_range_fn;

    template <typename Comp, typename T>
    struct compare {
        Comp& comp;
        const T& val;

        template <typename U>
        constexpr bool operator()(U&& u) const
        {
            return !nano::invoke(comp, val, std::forward<U>(u));
        }
    };


    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, const T& value, Comp& comp, Proj& proj)
    {
        const auto comparator = compare<Comp, T>{comp, value};
        return partition_point_fn::impl(std::move(first), std::move(last),
                                        comparator, proj);
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    I>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return upper_bound_fn::impl(std::move(first), std::move(last),
                                    value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return upper_bound_fn::impl(nano::begin(rng), nano::end(rng),
                                    value, comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::upper_bound_fn, upper_bound)

NANO_END_NAMESPACE

#endif

// nanorange/view/subrange.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED
#define NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED


// nanorange/view/interface.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_INTERFACE_HPP_INCLUDED
#define NANORANGE_VIEW_INTERFACE_HPP_INCLUDED


// nanorange/iterator/common_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_COMMON_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_COMMON_ITERATOR_HPP_INCLUDED

// nanorange/iterator/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_CONCEPTS_HPP_INCLUDED
#define NANORANGE_ITERATOR_CONCEPTS_HPP_INCLUDED





#endif


NANO_BEGIN_NAMESPACE

// [range.iterators.common]

namespace common_iterator_ {

template <typename I, typename S>
class common_iterator {
    static_assert(Iterator<I>, "");
    static_assert(Sentinel<S, I>, "");
    static_assert(!Same<I, S>, "");

    template <typename II, typename SS>
    friend class common_iterator;

    class op_arrow_proxy {
        iter_value_t<I> keep_;

        constexpr op_arrow_proxy(iter_reference_t<I>&& x) : keep_(std::move(x)) {}

    public:
        constexpr const iter_value_t<I>* operator->() const
        {
            return std::addressof(keep_);
        }
    };

    template <typename II>
    using op_arrow_t = decltype(std::declval<const II&>().operator->());

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<2>)
        -> std::enable_if_t<
            std::is_pointer<II>::value || detail::exists_v<op_arrow_t, II>, I>
    {
        return i;
    }

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<1>)
        -> std::enable_if_t<std::is_reference<iter_reference_t<const II>>::value,
                            std::add_pointer_t<iter_reference_t<const II>>>
    {
        auto&& tmp = *i;
        return std::addressof(tmp);
    }

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<0>)
        -> op_arrow_proxy
    {
        return {i};
    }

public:
    using difference_type = iter_difference_t<I>;

    constexpr common_iterator() : is_sentinel_{false}, iter_{} {}

    constexpr common_iterator(I i) : is_sentinel_{false}, iter_(i) {}

    constexpr common_iterator(S s) : is_sentinel_{true}, sentinel_{s} {}

    template <
        typename II, typename SS,
        std::enable_if_t<ConvertibleTo<II, I> && ConvertibleTo<SS, S>, int> = 0>
    constexpr common_iterator(const common_iterator<II, SS>& other)
        : is_sentinel_{other.is_sentinel_},
          iter_(other.iter_),
          sentinel_(other.sentinel_)
    {}

    template <typename II, typename SS>
    constexpr std::enable_if_t<ConvertibleTo<II, I> && ConvertibleTo<SS, S>,
                               common_iterator&>
    operator=(const common_iterator<II, SS>& other)
    {
        is_sentinel_ = other.is_sentinel_;
        iter_ = other.iter_;
        sentinel_ = other.sentinel_;
        return *this;
    }

    constexpr decltype(auto) operator*() { return *iter_; }

    template <typename II = I,
              std::enable_if_t<detail::Dereferenceable<const I>, int> = 0>
    constexpr decltype(auto) operator*() const
    {
        return *iter_;
    }

    template <typename II = I>
    constexpr auto operator-> () const
        -> decltype(common_iterator::do_op_arrow(std::declval<const II&>(),
                                                 detail::priority_tag<2>{}))
    {
        return do_op_arrow(iter_, detail::priority_tag<2>{});
    }

    constexpr common_iterator& operator++()
    {
        ++iter_;
        return *this;
    }

    template <typename II = I, std::enable_if_t<!ForwardIterator<II>, int> = 0>
    constexpr  decltype(auto) operator++(int)
    {
        return iter_++;
    }

    template <typename II = I, std::enable_if_t<ForwardIterator<II>, int> = 0>
    constexpr common_iterator operator++(int)
    {
        common_iterator tmp = *this;
        ++iter_;
        return tmp;
    }

    friend constexpr iter_rvalue_reference_t<I> iter_move(const common_iterator& i)
    {
        return ranges::iter_move(i.iter_);
    }

    template <typename I2, typename S2>
    friend constexpr std::enable_if_t<IndirectlySwappable<I2, I>>
    iter_swap(const common_iterator& x, const common_iterator<I2, S2>& y)
    {
        return ranges::iter_swap(x.iter_, y.iter_);
    }

    // private:
    // TODO: Some sort of variant-like union
    bool is_sentinel_{};
    I iter_{};
    S sentinel_{};
};

template <typename I1, typename I2, typename S1, typename S2,
          std::enable_if_t<!EqualityComparableWith<I1, I2>, int> = 0>
constexpr bool operator==(const common_iterator<I1, S1>& x,
                          const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_ ? (y.is_sentinel_ || y.iter_ == x.sentinel_)
                          : (!y.is_sentinel_ || x.iter_ == y.sentinel_);
}

template <typename I1, typename I2, typename S1, typename S2,
          std::enable_if_t<EqualityComparableWith<I1, I2>, int> = 0>
constexpr bool operator==(const common_iterator<I1, S1>& x,
                          const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_
               ? (y.is_sentinel_ || y.iter_ == x.sentinel_)
               : (y.is_sentinel_ ? x.iter_ == y.sentinel_ : x.iter_ == y.iter_);
}

template <typename I1, typename I2, typename S1, typename S2>
constexpr bool operator!=(const common_iterator<I1, S1>& x,
                          const common_iterator<I2, S2>& y)
{
    return !(x == y);
}

template <typename I2, typename I1, typename S1, typename S2>
constexpr
std::enable_if_t<SizedSentinel<I1, I2> && SizedSentinel<S1, I2> &&
                     SizedSentinel<S2, I2>,
                 iter_difference_t<I2>>
operator-(const common_iterator<I1, S1>& x, const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_
               ? (y.is_sentinel_ ? 0 : x.sentinel_ - y.iter_)
               : (y.is_sentinel_ ? x.iter_ - y.sentinel_ : x.iter_ - y.iter_);
}

} // namespace common_iterator_

using common_iterator_::common_iterator;

template <typename I, typename S>
struct readable_traits<common_iterator<I, S>> {
    using value_type = iter_value_t<I>;
};

template <typename I, typename S>
struct iterator_category<common_iterator<I, S>>
    : std::conditional<ForwardIterator<I>, forward_iterator_tag,
                       input_iterator_tag> {
};

NANO_END_NAMESPACE

namespace std {

template <typename I, typename S>
struct iterator_traits<::nano::common_iterator<I, S>> {
    using difference_type =
        ::nano::iter_difference_t<::nano::common_iterator<I, S>>;
    using value_type = ::nano::iter_value_t<::nano::common_iterator<I, S>>;
    using pointer =
        std::add_pointer_t<::nano::iter_reference_t<::nano::common_iterator<I, S>>>;
    using reference = ::nano::iter_reference_t<::nano::common_iterator<I, S>>;
    using iterator_category =
        std::conditional_t<::nano::ForwardIterator<I>,
                           std::forward_iterator_tag,
                           std::input_iterator_tag>;
};

} // namespace std

#endif


NANO_BEGIN_NAMESPACE

// [ranges.view_interface]

namespace detail {

template <typename, typename = void>
struct range_common_iterator_impl;

template <typename R>
struct range_common_iterator_impl<
    R, std::enable_if_t<Range<R> && !CommonRange<R>>> {
    using type = common_iterator<iterator_t<R>, sentinel_t<R>>;
};

template <typename R>
struct range_common_iterator_impl<R, std::enable_if_t<CommonRange<R>>> {
    using type = iterator_t<R>;
};

template <typename R>
using range_common_iterator_t = typename range_common_iterator_impl<R>::type;

} // namespace detail

template <typename D>
class view_interface {

    static_assert(std::is_class<D>::value, "");

private:
    constexpr D& derived() noexcept { return static_cast<D&>(*this); }

    constexpr const D& derived() const noexcept
    {
        return static_cast<const D&>(*this);
    }

public:
    template <typename R = D>
    NANO_NODISCARD constexpr auto empty() const
        -> std::enable_if_t<ForwardRange<const R>, bool>
    {
        return ranges::begin(derived()) == ranges::end(derived());
    }

    template <typename R = D, typename = decltype(ranges::empty(std::declval<const R&>()))>
    constexpr explicit operator bool() const
    {
        return !ranges::empty(derived());
    }

    // FIXME: This is to spec (P0896R2) but seems wrong when begin() does not return a pointer type
    template <typename R = D, typename = std::enable_if_t<ContiguousIterator<iterator_t<R>>>>
    constexpr auto data()
    {
        return ranges::begin(derived());
    }

    template <typename R = const D, typename = std::enable_if_t<
            Range<R> &&
            ContiguousIterator<iterator_t<R>>>>
    constexpr auto data() const
    {
        return ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<
            ForwardRange<const R> &&
            SizedSentinel<sentinel_t<const R>, iterator_t<const R>>
    >>
    constexpr auto size() const
    {
        return ranges::end(derived()) - ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<ForwardRange<R>>>
    constexpr decltype(auto) front()
    {
        return *ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<ForwardRange<const R>>>
    constexpr decltype(auto) front() const
    {
        return *ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<
            BidirectionalRange<R> && CommonRange<R>>>
    constexpr decltype(auto) back()
    {
        return *ranges::prev(ranges::end(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<
            BidirectionalRange<const R> && CommonRange<const R>>>
    constexpr decltype(auto) back() const
    {
        return *ranges::prev(ranges::end(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<RandomAccessRange<R>>>
    constexpr decltype(auto) operator[](iter_difference_t<iterator_t<R>> n)
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = const D,  typename = std::enable_if_t<RandomAccessRange<R>>>
    constexpr decltype(auto) operator[](iter_difference_t<iterator_t<R>> n) const
    {
        return ranges::begin(derived())[n];
    }

    template <typename C, typename R = D,
              typename = std::enable_if_t<
                  ForwardRange<C> && !View<C> &&
                  ConvertibleTo<iter_reference_t<iterator_t<const R>>,
                                iter_value_t<iterator_t<C>>> &&
                  Constructible<C, detail::range_common_iterator_t<const R>,
                                detail::range_common_iterator_t<const R>>>>
    operator C() const
    {
        using I = detail::range_common_iterator_t<D>;
        return C(I{ranges::begin(derived())}, I{ranges::end(derived())});
    }
};

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

// [ranges.subrange]

enum class subrange_kind : bool { unsized, sized };

namespace detail {

template <typename I, typename S, bool = SizedSentinel<S, I>>
struct default_subrange_kind {
    static constexpr subrange_kind kind = subrange_kind::unsized;
};

template <typename I, typename S>
struct default_subrange_kind<I, S, true> {
    static constexpr subrange_kind kind = subrange_kind::sized;
};

}

namespace subrange_ {

template <typename I, typename S = I,
          subrange_kind = detail::default_subrange_kind<I, S>::kind>
class subrange;

}

using subrange_::subrange;

namespace detail {

// libstdc++ < 7 does not have a SFINAE-friendly std::tuple_size,
// meaning that doing virtually anything with it is a hard error, including
// testing the PairLike concept below. As a workaround, we'll define our own
// tuple_size (only for std::tuple and std::pair) if we're using libstdc++ v5 or
// v6.
//
// FIXME: Do this better.
//
// Note:
// __GNUC__ tells us we're using GCC or Clang
// !_LIBCPP_VERSION tells us we're not using libc++
// !_GLIBCXX_RELEASE tells us we're not using libstdc++ >= 7, which is when this
// symbol was added
#if defined(__GNUC__) && !defined(_LIBCPP_VERSION) && !defined(_GLIBCXX_RELEASE)
template <typename>
struct sfinae_tuple_size {};

template <typename T>
struct sfinae_tuple_size<const T>
    : sfinae_tuple_size<T> {};

template <typename T, typename U>
struct sfinae_tuple_size<std::pair<T, U>>
    : std::integral_constant<std::size_t, 2> {};

template <typename... Args>
struct sfinae_tuple_size<std::tuple<Args...>>
    : std::integral_constant<std::size_t, sizeof...(Args)> {};
#else
template <typename T>
using sfinae_tuple_size = std::tuple_size<T>;
#endif

struct PairLike_req {
    template <std::size_t I, typename T>
    int test_func(const std::tuple_element_t<I, T>&);

    template <typename T>
    auto requires_(T t) -> decltype(
        valid_expr(std::enable_if_t<Integral<decltype(sfinae_tuple_size<T>::value)>, int>{},
                   std::enable_if_t<sfinae_tuple_size<T>::value == 2, int>{},
                   decltype(this->test_func<0, T>(std::get<0>(t))){},
                   decltype(this->test_func<1, T>(std::get<1>(t))){}));
};

template <typename T>
NANO_CONCEPT PairLike = detail::requires_<detail::PairLike_req, T>;

struct PairLikeConvertibleTo_req {
    template <typename T, typename U, typename V>
    auto requires_(T&& t) -> decltype(
        valid_expr(convertible_to_helper<U>(std::get<0>(std::forward<T>(t))),
                   convertible_to_helper<V>(std::get<1>(std::forward<T>(t)))));
};

template <typename T, typename U, typename V>
NANO_CONCEPT PairlikeConvertibleTo =
    !Range<T> && PairLike<std::decay_t<T>> &&
    detail::requires_<PairLikeConvertibleTo_req, T, U, V>;

template <typename T, typename U, typename V>
NANO_CONCEPT PairLikeConvertibleFrom = !Range<T> && Same<T, std::decay_t<T>> &&
                                       PairLike<T> && Constructible<T, U, V>;

template <typename T>
NANO_CONCEPT IteratorSentinelPair =
    !Range<T> && Same<T, std::decay_t<T>> && PairLike<T> &&
    Sentinel<std::tuple_element_t<1, T>, std::tuple_element_t<0, T>>;

template <typename T, typename U>
NANO_CONCEPT NotSameAs = !Same<remove_cvref_t<T>, remove_cvref_t<U>>;

template <typename I, typename S, bool StoreSize = false>
struct subrange_data {
    I begin_{};
    S end_{};
};

template <typename I, typename S>
struct subrange_data<I, S, true> {
    I begin_{};
    S end_{};
    iter_difference_t<I> size_ = 0;
};

// MSVC gets confused if enable_if conditions in template param lists are too
// complex, so give it some help by calculating the constraints in a helper
// variable
template <typename R, typename I, typename S, subrange_kind K>
auto subrange_range_constructor_constraint_helper_fn(long) -> std::false_type;

template <typename R, typename I, typename S, subrange_kind K>
auto subrange_range_constructor_constraint_helper_fn(int) -> std::enable_if_t<
                ForwardingRange<R>&&
                ConvertibleTo<iterator_t<R>, I> &&
                ConvertibleTo<sentinel_t<R>, S>, std::true_type>;

template <typename R, typename I, typename S, subrange_kind K>
constexpr bool subrange_range_constructor_constraint_helper =
    decltype(subrange_range_constructor_constraint_helper_fn<R, I, S, K>(0))::value;

} // namespace detail

namespace subrange_ {

template <typename I, typename S, subrange_kind K>
class subrange : public view_interface<subrange<I, S, K>> {
    static_assert(Iterator<I>, "");
    static_assert(Sentinel<S, I>, "");
    static_assert(K == subrange_kind::sized || !SizedSentinel<S, I>, "");

private:
    static constexpr bool StoreSize =
            K == subrange_kind::sized && !SizedSentinel<S, I>;

    detail::subrange_data<I, S, StoreSize> data_{};

    using base = view_interface<subrange>;

public:
    using iterator = I;
    using sentinel = S;

    subrange() = default;

    template <bool SS = StoreSize, typename = std::enable_if_t<!SS>>
    constexpr subrange(I i, S s)
            : data_{std::move(i), std::move(s)} {}

    template <subrange_kind KK = K,
            typename = std::enable_if_t<KK == subrange_kind::sized>>
    constexpr subrange(I i, S s, iter_difference_t<I> n)
            : data_{std::move(i), std::move(s), n} {}

    template <typename R, bool SS = StoreSize,
            std::enable_if_t<detail::NotSameAs<R, subrange>, int> = 0,
            std::enable_if_t<
                    detail::subrange_range_constructor_constraint_helper<R, I, S, K>
                    && SS && SizedRange<R>, int> = 0>
    constexpr subrange(R&& r)
            : subrange(ranges::begin(r), ranges::end(r), ranges::size(r)) {}

    template <typename R, bool SS = StoreSize,
            std::enable_if_t<detail::NotSameAs<R, subrange>, int> = 0,
            std::enable_if_t<
                    detail::subrange_range_constructor_constraint_helper<R, I, S, K>
                     && !SS, int> = 0>
    constexpr subrange(R&& r)
            : subrange(ranges::begin(r), ranges::end(r)) {}

    template <typename R, subrange_kind KK = K, std::enable_if_t<
            detail::ForwardingRange<R>&&
            ConvertibleTo<iterator_t<R>, I>&&
            ConvertibleTo<sentinel_t<R>, S>&&
            KK == subrange_kind::sized, int> = 0>
    constexpr subrange(R&& r, iter_difference_t<I> n)
            : subrange(ranges::begin(r), ranges::end(r), n) {}

    template <typename PairLike_, bool SS = StoreSize,
            std::enable_if_t<detail::NotSameAs<PairLike_, subrange>, int> = 0,
            std::enable_if_t<
                detail::PairlikeConvertibleTo<PairLike_, I, S> && !SS,
                    int> = 0>
    constexpr subrange(PairLike_&& r)
            : subrange{std::get<0>(std::forward<PairLike_>(r)),
                       std::get<1>(std::forward<PairLike_>(r))} {}

    template <typename PairLike_, subrange_kind KK = K,
            std::enable_if_t<detail::PairlikeConvertibleTo<PairLike_, I, S> &&
                    KK == subrange_kind::sized,
                    int> = 0>
    constexpr subrange(PairLike_&& r, iter_difference_t<I> n)
            : subrange{std::get<0>(std::forward<PairLike_>(r)),
                       std::get<1>(std::forward<PairLike_>(r)), n} {}

    template <typename PairLike_,
            std::enable_if_t<detail::NotSameAs<PairLike_, subrange>, int> = 0,
            std::enable_if_t<detail::PairLikeConvertibleFrom<
                                PairLike_, const I&, const S&>, int> = 0>
    constexpr operator PairLike_() const
    {
        return PairLike_(begin(), end());
    }

    // The above has hidden the conversion operator in view_interface.
    // There doesn't seem to be any obvious syntax to bring it back into
    // scope, so we'll just reimplement it here

    template <typename C, typename R = subrange,
              std::enable_if_t<detail::NotSameAs<C, subrange>, int> = 0,
              std::enable_if_t<ForwardRange<C> && !View<C>, int> = 0,
              typename = std::enable_if_t<
                    ConvertibleTo<iter_reference_t<iterator_t<const R>>,
                                  iter_value_t<iterator_t<C>>> &&
                    Constructible<C, detail::range_common_iterator_t<const R>,
                                     detail::range_common_iterator_t<const R>>>>
    operator C() const
    {
        using CI = detail::range_common_iterator_t<R>;
        return C(CI{ranges::begin(*this)}, CI{ranges::end(*this)});
    }

    constexpr I begin() const { return data_.begin_; }

    constexpr S end() const { return data_.end_; }

    NANO_NODISCARD constexpr bool empty() const
    {
        return data_.begin_ == data_.end_;
    }

    template <subrange_kind KK = K, bool SS = StoreSize>
    constexpr auto size() const
    -> std::enable_if_t<KK == subrange_kind::sized && SS,
            iter_difference_t<I>>
    {
        return data_.size_;
    }

    template <subrange_kind KK = K, bool SS = StoreSize>
    constexpr auto size() const
    -> std::enable_if_t<KK == subrange_kind::sized && !SS,
            iter_difference_t<I>>
    {
        return data_.end_ - data_.begin_;
    }

    NANO_NODISCARD constexpr subrange next(iter_difference_t<I> n = 1) const
    {
        auto tmp = *this;
        tmp.advance(n);
        return tmp;
    }

    template <typename II = I>
    NANO_NODISCARD constexpr auto prev(iter_difference_t<I> n = 1) const
    -> std::enable_if_t<BidirectionalIterator<II>, subrange>
    {
        auto tmp = *this;
        tmp.advance(-n);
        return tmp;
    }

    template <bool SS = StoreSize>
    constexpr auto advance(iter_difference_t<I> n)
    -> std::enable_if_t<SS, subrange&>
    {
        data_.size_ -= n - ranges::advance(data_.begin_, n, data_.end_);
        return *this;
    }

    template <bool SS = StoreSize>
    constexpr auto advance(iter_difference_t<I> n)
    -> std::enable_if_t<!SS, subrange&>
    {
        ranges::advance(data_.begin_, n, data_.end_);
        return *this;
    }

    // Work around ICE in GCC5 if these functions take a subrange by value,
    // while still allowing them to be found by ADL for rvalues
#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ < 6
    friend constexpr I begin(subrange&& r) { return r.begin(); }

    friend constexpr S end(subrange&& r) { return r.end(); }
#else
    friend constexpr I begin(subrange r) { return r.begin(); }

    friend constexpr S end(subrange r) { return r.end(); }
#endif
};

#ifdef NANO_HAVE_DEDUCTION_GUIDES

template <typename R, std::enable_if_t<detail::ForwardingRange<R> && !SizedRange<R>, int> = 0>
subrange(R&&)->subrange<iterator_t<R>, sentinel_t<R>>;

template <typename R, std::enable_if_t<detail::ForwardingRange<R> && SizedRange<R>, int> = 0>
subrange(R&&, std::nullptr_t = nullptr)
    ->subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>;

template <typename R, std::enable_if_t<detail::ForwardingRange<R>, int> = 0>
subrange(R&&, iter_difference_t<iterator_t<R>>) ->
    subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>;

#endif

} // namespace subrange_

namespace detail {

template <std::size_t, typename, typename, subrange_kind>
struct subrange_get_helper;

template <typename I, typename S, subrange_kind K>
struct subrange_get_helper<0, I, S, K> {
    constexpr I operator()(const subrange<I, S, K>& s) const
    {
        return s.begin();
    }
};

template <typename I, typename S, subrange_kind K>
struct subrange_get_helper<1, I, S, K> {
    constexpr S operator()(const subrange<I, S, K>& s) const { return s.end(); }
};

} // namespace detail

template <std::size_t N, typename I, typename S, subrange_kind K>
constexpr auto get(const subrange<I, S, K>& r)
    -> decltype(detail::subrange_get_helper<N, I, S, K>{}(r))
{
    return detail::subrange_get_helper<N, I, S, K>{}(r);
}

// Extensions for C++14 compilers without CTAD
// These basically replicate the subrange constructors above

template <typename I, typename S>
constexpr auto make_subrange(I i, S s)
    -> std::enable_if_t<Iterator<I> && Sentinel<S, I>,
                        decltype(subrange<I, S>{std::move(i), std::move(s)})>
{
    return {std::move(i), std::move(s)};
}

template <typename I, typename S>
constexpr auto make_subrange(I i, S s, iter_difference_t<I> n)
    -> std::enable_if_t<Iterator<I> && Sentinel<S, I>,
                        decltype(subrange<I, S>{std::move(i), std::move(s), n})>
{
    return {std::move(i), std::move(s), n};
}

template <typename R>
constexpr auto make_subrange(R&& r)
    -> std::enable_if_t<detail::ForwardingRange<R> && !SizedRange<R>,
                        subrange<iterator_t<R>, sentinel_t<R>>>
{
    return {std::forward<R>(r)};
}

template <typename R>
constexpr auto make_subrange(R&& r)
    -> std::enable_if_t<detail::ForwardingRange<R> && SizedRange<R>,
                        subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>>
{
    return {std::forward<R>(r)};
}

template <typename R>
constexpr auto make_subrange(R&& r, iter_difference_t<R> n)
-> std::enable_if_t<detail::ForwardingRange<R>,
        subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>>
{
    return {std::forward<R>(r), n};
}

template <typename R>
using safe_subrange_t =
    std::enable_if_t<detail::ForwardingRange<R>, subrange<iterator_t<R>>>;

NANO_END_NAMESPACE

namespace std {

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_size<::nano::subrange<I, S, K>>
    : public integral_constant<size_t, 2> {
};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<0, ::nano::subrange<I, S, K>> {
public:
    using type = I;
};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<1, ::nano::subrange<I, S, K>> {
public:
    using type = S;
};

} // namespace std

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct equal_range_fn {
private:
    template <typename I, typename S, typename T, typename Comp, typename Proj>
    static constexpr subrange<I> impl(I first, S last, const T& value,
                                      Comp& comp, Proj& proj)
    {
        return {lower_bound_fn::impl(first, last, value, comp, proj),
                upper_bound_fn::impl(first, last, value, comp, proj)};
    }

public:
    template <typename I, typename S, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<I, Proj>>,
    subrange<I>>
    constexpr operator()(I first, S last, const T& value, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return equal_range_fn::impl(std::move(first), std::move(last),
                                    value, comp, proj);
    }

    template <typename Rng, typename T, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, const T*, projected<iterator_t<Rng>, Proj>>,
    safe_subrange_t<Rng>>
    constexpr operator()(Rng&& rng, const T& value, Comp comp = Comp{},
                         Proj proj = Proj{}) const
    {
        return equal_range_fn::impl(nano::begin(rng), nano::end(rng),
                                    value, comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::equal_range_fn, equal_range)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/fill.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FILL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FILL_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct fill_fn {
private:
    template <typename T, typename O, typename S>
    static constexpr O impl(O first, S last, const T& value)
    {
        while (first != last) {
            *first = value;
            ++first;
        }

        return first;
    }

public:
    template <typename T, typename O, typename S>
    constexpr std::enable_if_t<OutputIterator<O, const T&> && Sentinel<S, O>, O>
    operator()(O first, S last, const T& value) const
    {
        return fill_fn::impl(std::move(first), std::move(last), value);
    }

    template <typename T, typename Rng>
    constexpr std::enable_if_t<OutputRange<Rng, const T&>, safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& value) const
    {
        return fill_fn::impl(nano::begin(rng), nano::end(rng), value);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::fill_fn, fill)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/fill_n.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FILL_N_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FILL_N_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct fill_n_fn {
    template <typename T, typename O>
    constexpr std::enable_if_t<OutputIterator<O, const T&>, O>
    operator()(O first, iter_difference_t<O> n, const T& value) const
    {
        for (iter_difference_t<O> i{0}; i < n; ++i, ++first) {
            *first = value;
        }
        return first;
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::fill_n_fn, fill_n)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/find.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FIND_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FIND_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [ranges.alg.find]

namespace detail {

struct find_if_fn {
private:
    friend struct find_fn;
    friend struct find_if_not_fn;

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                return first;
            }
            ++first;
        }
        return first;
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(std::move(first), std::move(last), pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(nano::begin(rng), nano::end(rng), pred, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::find_if_fn, find_if)

namespace detail {

struct find_fn {
private:
    template <typename ValueType>
    struct equal_to_pred {
        const ValueType& val;

        template <typename T>
        constexpr bool operator()(const T& t) const
        {
            return t == val;
        }
    };

public:
    template <typename I, typename S, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
        I>
    operator()(I first, S last, const T& value, Proj proj = Proj{}) const
    {
        const equal_to_pred<T> pred{value};
        return find_if_fn::impl(std::move(first), std::move(last), pred, proj);
    }

    template <typename Rng, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T*>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& value, Proj proj = Proj{}) const
    {
        const equal_to_pred<T> pred{value};
        return find_if_fn::impl(nano::begin(rng), nano::end(rng), pred, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::find_fn, find)

namespace detail {

struct find_if_not_fn {
private:
    template <typename Pred>
    struct not_pred {
        Pred& p;

        template <typename T>
        constexpr bool operator()(T&& t) const
        {
            return !nano::invoke(p, std::forward<T>(t));
        }
    };

public:
    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        const auto find_if_pred = not_pred<Pred>{pred};
        return find_if_fn::impl(std::move(first), std::move(last),
                                find_if_pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        const auto find_if_pred = not_pred<Pred>{pred};
        return find_if_fn::impl(nano::begin(rng), nano::end(rng),
                                find_if_pred, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::find_if_not_fn, find_if_not)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/find_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FIND_END_HPP_INCLUDED



// nanorange/algorithm/search.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SEARCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SEARCH_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct search_fn {
private:
    friend struct find_end_fn;

    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2 = identity>
    static constexpr subrange<I1>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
         Proj2& proj2)
    {
        while (true) {
            auto it1 = first1;
            auto it2 = first2;

            while (true) {
                if (it2 == last2) {
                    return {first1, it1};
                }
                if (it1 == last1) {
                    return {it1, it1};
                }
                if (!nano::invoke(pred, nano::invoke(proj1, *it1), nano::invoke(proj2, *it2))) {
                    break;
                }
                ++it1; ++it2;
            }


            ++first1;
        }
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
         ForwardIterator<I1> &&
         Sentinel<S1, I1> &&
         ForwardIterator<I2> &&
         Sentinel<S2, I2> &&
         IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
         subrange<I1>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
               Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return search_fn::impl(std::move(first1), std::move(last1),
                               std::move(first2), std::move(last2),
                               pred, proj1, proj2);
    }

    template <typename Rng1, typename Rng2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng1> &&
            ForwardRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred, Proj1, Proj2>,
            safe_subrange_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return search_fn::impl(nano::begin(rng1), nano::end(rng1),
                               nano::begin(rng2), nano::end(rng2),
                               pred, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::search_fn, search)

NANO_END_NAMESPACE

#endif

NANO_BEGIN_NAMESPACE

// [ranges.alg.find.end]
namespace detail {

// TODO: For BiDir iterators, we can be smarter and search backwards
struct find_end_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2>
    static constexpr subrange<I1>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
         Proj2& proj2)
    {
        if (first2 == last2) {
            auto last_it = nano::next(first1, last1);
            return {last_it, last_it};
        }

        auto result = search_fn::impl(std::move(first1), last1, first2, last2, pred, proj1, proj2);

        if (result.empty()) {
            return result;
        }

        while (true) {
            auto new_result = search_fn::impl(next(result.begin()), last1, first2, last2, pred, proj1, proj2);
            if (new_result.empty()) {
                return result;
            } else {
                result = std::move(new_result);
            }
        }
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
            typename Pred = equal_to<>, typename Proj1 = identity,
            typename Proj2 = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I1> &&
        Sentinel<S1, I1> &&
        ForwardIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
        subrange<I1>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_end_fn::impl(std::move(first1), std::move(last1),
                                 std::move(first2), std::move(last2),
                                 pred, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng1> &&
            ForwardRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred, Proj1, Proj2>,
            safe_subrange_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_end_fn::impl(nano::begin(rng1), nano::end(rng1),
                                 nano::begin(rng2), nano::end(rng2),
                                 pred, proj1, proj2);
    }

};

}

NANO_INLINE_VAR(detail::find_end_fn, find_end)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/find_first_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FIND_FIRST_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FIND_FIRST_OF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.alg.find.first.of]

namespace detail {

struct find_first_of_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr I1 impl(I1 first1, S1 last1, I2 first2, S2 last2,
                             Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        for (; first1 != last1; ++first1) {
            for (I2 it = first2; it != last2; ++it) {
                if (nano::invoke(pred, nano::invoke(proj1, *first1),
                                 nano::invoke(proj2, *it))) {
                    return first1;
                }
            }
        }

        return first1;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Proj1 = identity, typename Proj2 = identity,
              typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && ForwardIterator<I2> &&
            Sentinel<S2, I2> &&
            IndirectRelation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
        I1>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_first_of_fn::impl(std::move(first1), std::move(last1),
                                      std::move(first2), std::move(last2),
                                      pred, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        InputRange<Rng1> && ForwardRange<Rng2> &&
            IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>,
                             projected<iterator_t<Rng2>, Proj2>>,
        safe_iterator_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_first_of_fn::impl(nano::begin(rng1), nano::end(rng1),
                                      nano::begin(rng2), nano::end(rng2),
                                      pred, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::find_first_of_fn, find_first_of)

NANO_END_NAMESPACE

#endif
// nanorange/algorithm/for_each.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_FOR_EACH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_FOR_EACH_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.alg.foreach]

template <typename I, typename F>
struct for_each_result {
    I in;
    F fun;
};

namespace detail {

struct for_each_fn {
private:
    template <typename I, typename S, typename Proj, typename Fun>
    static constexpr for_each_result<I, Fun>
    impl(I first, S last, Fun& fun, Proj& proj)
    {
        while (first != last) {
            nano::invoke(fun, nano::invoke(proj, *first));
            ++first;
        }
        return {first, std::move(fun)};
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Fun>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryInvocable<Fun, projected<I, Proj>>,
        for_each_result<I, Fun>>
    operator()(I first, S last, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(std::move(first), std::move(last),
                                 fun, proj);
    }

    template <typename Rng, typename Proj = identity, typename Fun>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryInvocable<Fun, projected<iterator_t<Rng>, Proj>>,
        for_each_result<safe_iterator_t<Rng>, Fun>>
    operator()(Rng&& rng, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(nano::begin(rng), nano::end(rng),
                                 fun, proj);
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::for_each_fn, for_each)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/generate.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_GENERATE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_GENERATE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct generate_fn {
private:
    template <typename O, typename S, typename F>
    static constexpr O impl(O first, S last, F& gen)
    {
        while (first != last) {
            *first = gen();
            ++first;
        }

        return first;
    }

public:
    template <typename O, typename S, typename F>
    constexpr std::enable_if_t<Iterator<O> && Sentinel<S, O> &&
                                   CopyConstructible<F> && Invocable<F&> &&
                                   Writable<O, invoke_result_t<F&>>,
                               O>
    operator()(O first, S last, F gen) const
    {
        return generate_fn::impl(std::move(first), std::move(last), gen);
    }

    template <typename Rng, typename F>
    constexpr std::enable_if_t<Invocable<F&> &&
                                   OutputRange<Rng, invoke_result_t<F&>>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, F gen) const
    {
        return generate_fn::impl(nano::begin(rng), nano::end(rng), gen);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::generate_fn, generate)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/generate_n.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_GENERATE_N_HPP_INCLUDED
#define NANORANGE_ALGORITHM_GENERATE_N_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct generate_n_fn {
    template <typename O, typename F>
    constexpr std::enable_if_t<Iterator<O> && CopyConstructible<F> &&
                                   Invocable<F&> &&
                                   Writable<O, invoke_result_t<F&>>,
                               O>
    operator()(O first, iter_difference_t<O> n, F gen) const
    {
        for (iter_difference_t<O> i{0}; i < n; ++i, ++first) {
            *first = gen();
        }

        return first;
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::generate_n_fn, generate_n)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/includes.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_INCLUDES_HPP_INCLUDED
#define NANORANGE_ALGORITHM_INCLUDES_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct includes_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
     static constexpr bool impl(I1 first1, S1 last1, I2 first2, S2 last2,
                                Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first2 != last2) {
            // If range1 is done but we still have elements in range2, then
            // it is not a subset
            if (first1 == last1)  {
                return false;
            }

            // If the current element of r2 is less than the current
            // element of r1, then it is not in r1 => not a subset
            if (nano::invoke(comp, nano::invoke(proj2, *first2),
                             nano::invoke(proj1, *first1))) {
                return false;
            }

            // Now we know that that !(r2 < r1). If we also have !(r1 < r2),
            // then it must be equal, so in range1 -- so move onto the next
            // element
            if (!nano::invoke(comp, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                ++first2;
            }

            ++first1;
        }

        return true;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = less<>, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectStrictWeakOrder<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return includes_fn::impl(std::move(first1), std::move(last1),
                                 std::move(first2), std::move(last2),
                                 comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        IndirectStrictWeakOrder<Comp,
                                projected<iterator_t<Rng1>, Proj1>,
                                projected<iterator_t<Rng2>, Proj2>>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return includes_fn::impl(nano::begin(rng1), nano::end(rng1),
                                 nano::begin(rng2), nano::end(rng2),
                                 comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::includes_fn, includes)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/is_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_HEAP_HPP_INCLUDED

// nanorange/algorithm/is_heap_until.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
//
// Copyright Eric Niebler 2014
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_IS_HEAP_UNTIL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_HEAP_UNTIL_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct is_heap_until_fn {
private:
    friend struct is_heap_fn;

    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, const iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        iter_difference_t<I> p = 0, c = 1;

        I pp = first;

        while (c < n) {
            I cp = first + c;

            if (nano::invoke(comp, nano::invoke(proj, *pp),
                             nano::invoke(proj, *cp))) {
                return cp;
            }

            ++c;
            ++cp;

            if (c == n || nano::invoke(comp, nano::invoke(proj, *pp),
                                       nano::invoke(proj, *cp))) {
                return cp;
            }

            ++p;
            ++pp;

            c = 2 * p + 1;
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> &&
            IndirectStrictWeakOrder<Comp, projected<I, Proj>>,
        I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        auto n = nano::distance(first, last);
        return is_heap_until_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessRange<Rng> &&
            IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_heap_until_fn::impl(nano::begin(rng), nano::distance(rng),
                                      comp, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_heap_until_fn, is_heap_until)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct is_heap_fn {
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    std::enable_if_t<RandomAccessIterator<I> && Sentinel<S, I> &&
                         IndirectStrictWeakOrder<Comp, projected<I, Proj>>,
                     bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return is_heap_until_fn::impl(std::move(first), n, comp, proj) == last;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
            IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_heap_until_fn::impl(nano::begin(rng), nano::distance(rng),
                                      comp, proj) == nano::end(rng);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_heap_fn, is_heap)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/is_partitioned.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_PARTITIONED_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PARTITIONED_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct is_partitioned_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr bool impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if_not(std::move(first), last, pred, proj);
        return nano::find_if(std::move(first), last, pred, proj) == last;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
    operator()(I first, S last, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return is_partitioned_fn::impl(std::move(first), std::move(last),
                                       pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
    operator()(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{}) const
    {
        return is_partitioned_fn::impl(nano::begin(rng), nano::end(rng),
                                       pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::is_partitioned_fn, is_partitioned)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/is_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_PERMUTATION_HPP_INCLUDED



// nanorange/algorithm/mismatch.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MISMATCH_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MISMATCH_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.mismatch]

template <typename I1, typename I2>
struct mismatch_result {
    I1 in1;
    I2 in2;
};

namespace detail {

struct mismatch_fn {
private:
    friend struct is_permutation_fn;

    template <typename I1, typename S1, typename I2, typename Proj1,
              typename Proj2, typename Pred>
    static constexpr mismatch_result<I1, I2>
    impl3(I1 first1, S1 last1, I2 first2, Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 &&
               nano::invoke(pred, nano::invoke(proj1, *first1),
                            nano::invoke(proj2, *first2))) {
            ++first1;
            ++first2;
        }

        return {first1, first2};
    }

    template <typename I1, typename S1, typename I2, typename S2,
              typename Proj1, typename Proj2, typename Pred>
    static constexpr mismatch_result<I1, I2>
    impl4(I1 first1, S1 last1, I2 first2, S2 last2, Pred& pred, Proj1& proj1,
          Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2 &&
               nano::invoke(pred, nano::invoke(proj1, *first1),
                            nano::invoke(proj2, *first2))) {
            ++first1;
            ++first2;
        }

        return {first1, first2};
    }

public:
    // three legged
    template <typename I1, typename S1, typename I2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = equal_to<>>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<std::decay_t<I2>> &&
        !InputRange<I1> &&
        IndirectRelation<Pred, projected<I1, Proj1>, projected<std::decay_t<I2>, Proj2>>,
        mismatch_result<I1, std::decay_t<I2>>>
    operator()(I1 first1, S1 last1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl3(std::move(first1), std::move(last1),
                                  std::forward<I2>(first2), pred,
                                  proj1, proj2);
    }

    // range and a half
    template <typename Rng1, typename I2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = equal_to<>>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputRange<Rng1> && InputIterator<std::decay_t<I2>> &&
                !InputRange<I2> &&
            IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>,
                             projected<std::decay_t<I2>, Proj2>>,
        mismatch_result<safe_iterator_t<Rng1>, std::decay_t<I2>>>
    operator()(Rng1&& rng1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                  std::forward<I2>(first2), pred,
                                  proj1, proj2);
    }

    // four legged
    template <typename I1, typename S1, typename I2, typename S2,
              typename Proj1 = identity, typename Proj2 = identity,
              typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<I2> &&
            Sentinel<S2, I2> &&
            IndirectRelation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
        mismatch_result<I1, I2>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl4(std::move(first1), std::move(last1),
                                  std::move(first2), std::move(last2),
                                  pred, proj1, proj2);
    }

    // two ranges
    template <typename Rng1, typename Rng2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        InputRange<Rng1> && InputRange<Rng2> &&
            IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>,
                             projected<iterator_t<Rng2>, Proj2>>,
        mismatch_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return mismatch_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                  nano::begin(rng2), nano::end(rng2),
                                  pred, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::mismatch_fn, mismatch)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct is_permutation_fn {
private:
    template <typename Pred, typename Val>
    struct comparator
    {
        Pred& pred;
        const Val& val;

        template <typename T>
        constexpr bool operator()(const T& t) const
        {
            return nano::invoke(pred, t, val);
        }
    };

    template <typename I1, typename S1, typename I2, typename S2, typename Pred,
              typename Proj1, typename Proj2>
    static constexpr bool process_tail(I1 first1, S1 last1, I2 first2, S2 last2,
                                       Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        for (auto it = first1; it != last1; ++it) {
            comparator<Pred, decltype(nano::invoke(proj1, *it))>
                comp{pred, nano::invoke(proj1, *it)};

            // Check whether we have already seen this value
            if (any_of_fn::impl(first1, it, comp, proj1)) {
                continue;
            }

            // Count how many times *it appears in range2
            const auto count1 = count_if_fn::impl(first2, last2, comp, proj2);

            // If we have a count of zero, we know the ranges are different
            if (count1 == 0) {
                return false;
            }

            // Count how many times *it appears in the remainder of range1
            // (we can start from one)
            const auto count2 = iter_difference_t<I1>{1} +
                count_if_fn::impl(nano::next(it), last1, comp, proj1);

            if (count1 != count2) {
                return false;
            }
        }

        return true;
    }

    template <typename I1, typename S1, typename I2, typename Pred,
        typename Proj1, typename Proj2>
    static constexpr bool impl3(I1 first1, S1 last1, I2 first2,
                                Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        // Strip equal prefixes from both ranges
        auto result = mismatch_fn::impl3(std::move(first1), last1,
                                         std::move(first2),
                                         pred, proj1, proj2);
        first1 = std::move(result).in1;
        first2 = std::move(result).in2;

        if (first1 == last1) {
            return true;
        }

        // If we have only one value left in range1, it can't be in range2
        const auto d = nano::distance(first1, last1);
        if (d == 1) {
            return false;
        }

        auto last2 = nano::next(first2, d);

        return is_permutation_fn::process_tail(std::move(first1), std::move(last1),
                                               std::move(first2), std::move(last2),
                                               pred, proj1, proj2);
    }

    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2>
    static constexpr bool impl4(I1 first1, S1 last1, I2 first2, S2 last2,
                                Pred& pred, Proj1& proj1, Proj2& proj2)
    {
        // Strip equal prefixes from both ranges
        auto result = mismatch_fn::impl4(std::move(first1), last1,
                                         std::move(first2), last2,
                                         pred, proj1, proj2);
        first1 = std::move(result).in1;
        first2 = std::move(result).in2;

        // If we have reached the end of both ranges, they were the same
        if (first1 == last1 && first2 == last2) {
            return true;
        }

        // If we have different numbers of elements left in the ranges,
        // they are not permutations of one another
        if (nano::distance(first1, last1) != nano::distance(first2, last2)) {
            return false;
        }

        return is_permutation_fn::process_tail(std::move(first1), std::move(last1),
                                               std::move(first2), std::move(last2),
                                               pred, proj1, proj2);
    }

public:
    // Four-legged
    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred = equal_to<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr
        std::enable_if_t<ForwardIterator<I1> && Sentinel<S1, I1> &&
                             ForwardIterator<I2> && Sentinel<S2, I2> &&
                             IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
                         bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if /*constexpr*/ (SizedSentinel<S1, I1> && SizedSentinel<S2, I2>) {
            if (nano::distance(first1, last1) != nano::distance(first2, last2)) {
                return false;
            }
            return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                            std::move(first2), pred,
                                            proj1, proj2);
        }

        return is_permutation_fn::impl4(std::move(first1), std::move(last1),
                                        std::move(first2), std::move(last2),
                                        pred, proj1, proj2);
    }

    // Three-legged
    template <typename I1, typename S1, typename I2,
        typename Pred = equal_to<>, typename Proj1 = identity,
        typename Proj2 = identity>
    NANO_DEPRECATED
    constexpr
    std::enable_if_t<ForwardIterator<I1> && Sentinel<S1, I1> &&
                     ForwardIterator<I2> &&
                     IndirectlyComparable<I1, I2, Pred, Proj1, Proj2>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return is_permutation_fn::impl3(std::move(first1), std::move(last1),
                                        std::move(first2), pred,
                                        proj1, proj2);

    }

    // Two ranges
    template <typename Rng1, typename Rng2, typename Pred = equal_to<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng1> && ForwardRange<Rng2> &&
            IndirectlyComparable<iterator_t<Rng1>, iterator_t<Rng2>, Pred,
                                 Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        if (SizedRange<Rng1> && SizedRange<Rng2>) {
            if (nano::distance(rng1) != nano::distance(rng2)) {
                return false;
            }

            return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                            nano::begin(rng2), pred,
                                            proj1, proj2);
        }

        return is_permutation_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                        nano::begin(rng2), nano::end(rng2),
                                        pred, proj1, proj2);
    }

    // Range and a half
    template <typename Rng1, typename I2, typename Pred = equal_to<>,
        typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED
    constexpr std::enable_if_t<
        ForwardRange<Rng1> && ForwardIterator<std::decay_t<I2>> &&
        !Range<I2> &&
        IndirectlyComparable<iterator_t<Rng1>, I2, Pred, Proj1, Proj2>,
        bool>
    operator()(Rng1&& rng1, I2&& first2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const {
        return is_permutation_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                        std::forward<I2>(first2), pred,
                                        proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::is_permutation_fn, is_permutation)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/is_sorted.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_SORTED_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_SORTED_HPP_INCLUDED

// nanorange/algorithm/is_sorted_until.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_IS_SORTED_UNTIL_HPP_INCLUDED
#define NANORANGE_ALGORITHM_IS_SORTED_UNTIL_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct is_sorted_until_fn {
private:
    friend struct is_sorted_fn;

    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I n = next(first);

        while (n != last) {
            if (nano::invoke(comp, nano::invoke(proj, *n),
                              nano::invoke(proj, *first))) {
                return n;
            }
            ++first;
            ++n;
        }

        return n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(std::move(first), std::move(last),
                                        comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(nano::begin(rng), nano::end(rng),
                                        comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::is_sorted_until_fn, is_sorted_until)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct is_sorted_fn {
    template <typename I, typename S, typename Comp = less<>,
            typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>, bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(std::move(first), last,
                                        comp, proj) == last;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return is_sorted_until_fn::impl(nano::begin(rng), nano::end(rng),
                                        comp, proj) == nano::end(rng);
    }
};

}

NANO_INLINE_VAR(detail::is_sorted_fn, is_sorted)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/stl/lexicographical_compare.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_LEXICOGRAPHICAL_COMPARE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_LEXICOGRAPHICAL_COMPARE_HPP_INCLUDED




#include <algorithm>

NANO_BEGIN_NAMESPACE

namespace detail {

struct lexicographical_compare_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr bool impl(I1 first1, S1 last1, I2 first2, S2 last2,
                                Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2) {
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                              nano::invoke(proj2, *first2))) {
                return true;
            }
            if (nano::invoke(comp, nano::invoke(proj2, *first2),
                             nano::invoke(proj1, *first1))) {
                return false;
            }
            ++first1; ++first2;
        }

        return first1 == last1 && first2 != last2;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectStrictWeakOrder<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
        bool>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return lexicographical_compare_fn::impl(
                std::move(first1), std::move(last1),
                std::move(first2), std::move(last2),
                comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng1>, Proj1>,
                                      projected<iterator_t<Rng2>, Proj2>>,
            bool>
    operator()(Rng1&& rng1, Rng2&& rng2, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return lexicographical_compare_fn::impl(
                nano::begin(rng1), nano::end(rng1),
                nano::begin(rng2), nano::end(rng2),
                comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::lexicographical_compare_fn, lexicographical_compare)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/make_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MAKE_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MAKE_HEAP_HPP_INCLUDED

// nanorange/detail/algorithm/heap_sift.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
#ifndef NANORANGE_DETAIL_ALGORITHM_HEAP_SIFT_HPP
#define NANORANGE_DETAIL_ALGORITHM_HEAP_SIFT_HPP





///////////////////////////////////////////////////////////////////////////
// detail::sift_up_n and detail::sift_down_n
// (heap implementation details)
//

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename I, typename Comp, typename Proj>
constexpr void sift_up_n(I first, iter_difference_t<I> n, Comp& comp,
                         Proj& proj)
{
    if (n > 1) {
        I last = first + n;
        n = (n - 2) / 2;
        I i = first + n;
        if (nano::invoke(comp, nano::invoke(proj, *i),
                         nano::invoke(proj, *--last))) {
            iter_value_t<I> v = nano::iter_move(last);
            do {
                *last = nano::iter_move(i);
                last = i;
                if (n == 0) {
                    break;
                }
                n = (n - 1) / 2;
                i = first + n;
            } while (nano::invoke(comp, nano::invoke(proj, *i),
                                  nano::invoke(proj, v)));
            *last = std::move(v);
        }
    }
}

template <typename I, typename Comp, typename Proj>
constexpr void sift_down_n(I first, iter_difference_t<I> n, I start, Comp& comp,
                           Proj& proj)
{
    // left-child of start is at 2 * start + 1
    // right-child of start is at 2 * start + 2
    auto child = start - first;

    if (n < 2 || (n - 2) / 2 < child) {
        return;
    }

    child = 2 * child + 1;
    I child_i = first + child;

    if ((child + 1) < n && nano::invoke(comp, nano::invoke(proj, *child_i),
                                        nano::invoke(proj, *(child_i + 1)))) {
        // right-child exists and is greater than left-child
        ++child_i;
        ++child;
    }

    // check if we are in heap-order
    if (nano::invoke(comp, nano::invoke(proj, *child_i),
                     nano::invoke(proj, *start))) {
        // we are, start is larger than its largest child
        return;
    }

    iter_value_t<I> top = nano::iter_move(start);
    do {
        // we are not in heap-order, swap the parent with it's largest child
        *start = nano::iter_move(child_i);
        start = child_i;

        if ((n - 2) / 2 < child) {
            break;
        }

        // recompute the child based off of the updated parent
        child = 2 * child + 1;
        child_i = first + child;

        if ((child + 1) < n &&
            nano::invoke(comp, nano::invoke(proj, *child_i),
                         nano::invoke(proj, *(child_i + 1)))) {
            // right-child exists and is greater than left-child
            ++child_i;
            ++child;
        }

        // check if we are in heap-order
    } while (!nano::invoke(comp, nano::invoke(proj, *child_i),
                           nano::invoke(proj, top)));
    *start = std::move(top);
}

} // namespace detail

NANO_END_NAMESPACE

#endif



NANO_BEGIN_NAMESPACE

namespace detail {

struct make_heap_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n > 1) {
            // start from the first parent, there is no need to consider
            // children
            for (auto start = (n - 2) / 2; start >= 0; --start) {
                detail::sift_down_n(first, n, first + start, comp, proj);
            }
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> && Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return make_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<RandomAccessRange<Rng> &&
                                   Sortable<iterator_t<Rng>, Comp>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return make_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                  proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::make_heap_fn, make_heap)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/max.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MAX_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MAX_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct max_fn {
private:
    template <typename Rng, typename Comp, typename Proj>
    static constexpr iter_value_t<iterator_t<Rng>>
    impl(Rng&& rng, Comp& comp, Proj& proj)
    {
        auto first = nano::begin(rng);
        const auto last = nano::end(rng);

        // Empty ranges not allowed
        auto result = *first;

        while(++first != last) {
            auto&& val = *first;
            if (nano::invoke(comp, nano::invoke(proj, result),
                              nano::invoke(proj, val))) {
                result = std::forward<decltype(val)>(val);
            }
        }

        return result;
    }

public:
    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
    const T&>
    operator()(const T& a, const T& b, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        // *sigh*, this should be fixed in STL2
        return !nano::invoke(comp, nano::invoke(proj, a),
                            nano::invoke(proj, b)) ? a : b;
    }

    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            Copyable<T> &&
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
            T>
    operator()(std::initializer_list<T> rng, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return max_fn::impl(rng, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            InputRange<Rng> &&
            Copyable<iter_value_t<iterator_t<Rng>>> &&
    IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
    iter_value_t<iterator_t<Rng>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return max_fn::impl(std::forward<Rng>(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::max_fn, max)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/max_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MAX_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MAX_ELEMENT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct max_element_fn {
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I i = nano::next(first);
        while (i != last) {
            if (!nano::invoke(comp, nano::invoke(proj, *i),
                              nano::invoke(proj, *first))) {
                first = i;
            }
            ++i;
        }

        return first;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
            typename Proj = identity>
    constexpr std::enable_if_t<
            ForwardIterator<I> &&
            Sentinel<S, I> &&
    IndirectStrictWeakOrder<Comp, projected<I, Proj>>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return max_element_fn::impl(std::move(first), std::move(last),
                                    comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng> &&
            IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return max_element_fn::impl(nano::begin(rng), nano::end(rng),
                                    comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::max_element_fn, max_element)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/merge.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MERGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MERGE_HPP_INCLUDED


// nanorange/algorithm/transform.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_TRANSFORM_HPP_INCLUDED
#define NANORANGE_ALGORITHM_TRANSFORM_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using unary_transform_result = copy_result<I, O>;

template <typename I1, typename I2, typename O>
struct binary_transform_result {
    I1 in1;
    I2 in2;
    O out;
};


namespace detail {

struct transform_fn {
private:
    template <typename I, typename S, typename O, typename F, typename Proj>
    static constexpr unary_transform_result<I, O>
    unary_impl(I first, S last, O result, F& op, Proj& proj)
    {
        while (first != last) {
            *result = nano::invoke(op, nano::invoke(proj, *first));
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I1, typename S1, typename I2, typename O, typename F,
              typename Proj1, typename Proj2>
    static constexpr binary_transform_result<I1, I2, O>
    binary_impl3(I1 first1, S1 last1, I2 first2, O result, F& op, Proj1& proj1,
                 Proj2& proj2)
    {
        while (first1 != last1) {
            *result = nano::invoke(op, nano::invoke(proj1, *first1),
                                   nano::invoke(proj2, *first2));
            ++first1;
            ++first2;
            ++result;
        }

        return {std::move(first1), std::move(first2), std::move(result)};
    }

    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename F, typename Proj1, typename Proj2>
    static constexpr binary_transform_result<I1, I2, O>
    binary_impl4(I1 first1, S1 last1, I2 first2, S2 last2, O result, F& op,
                 Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2) {
            *result = nano::invoke(op, nano::invoke(proj1, *first1),
                                   nano::invoke(proj2, *first2));
            ++first1;
            ++first2;
            ++result;
        }

        return {std::move(first1), std::move(first2), std::move(result)};
    }

public:
    // Unary op, iterators
    template <typename I, typename S, typename O, typename F,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && WeaklyIncrementable<O> &&
            CopyConstructible<F> &&
            Writable<O, indirect_result_t<F&, projected<I, Proj>>>,
        unary_transform_result<I, O>>
    operator()(I first, S last, O result, F op, Proj proj = Proj{}) const
    {
        return transform_fn::unary_impl(std::move(first), std::move(last),
                                        std::move(result), op, proj);
    }

    // Unary op, range
    template <typename Rng, typename O, typename F, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && WeaklyIncrementable<O> && CopyConstructible<F> &&
            Writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng>, Proj>>>,
        unary_transform_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, F op, Proj proj = Proj{}) const
    {
        return transform_fn::unary_impl(nano::begin(rng), nano::end(rng),
                                        std::move(result), op, proj);
    }

    // Binary op, four-legged
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename F, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<I2> &&
            Sentinel<S2, I2> && WeaklyIncrementable<O> &&
            CopyConstructible<F> &&
            Writable<O, indirect_result_t<F&, projected<I1, Proj1>,
                                          projected<I2, Proj2>>>,
        binary_transform_result<I1, I2, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result, F op,
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl4(std::move(first1), std::move(last1),
                                          std::move(first2), std::move(last2),
                                          std::move(result), op, proj1, proj2);
    }

    // Binary op, two ranges
    template <typename Rng1, typename Rng2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> && InputRange<Rng2> && WeaklyIncrementable<O> &&
            CopyConstructible<F> &&
            Writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng1>, Proj1>,
                                       projected<iterator_t<Rng2>, Proj2>>>,
        binary_transform_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, F op, Proj1 proj1 = Proj1{},
               Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl4(nano::begin(rng1), nano::end(rng1),
                                          nano::begin(rng2), nano::end(rng2),
                                          std::move(result), op, proj1, proj2);
    }

    // Binary op, three-legged
    template <typename I1, typename S1, typename I2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputIterator<I1> && Sentinel<S1, I1> && InputIterator<std::decay_t<I2>> &&
            !InputRange<I2> &&
            WeaklyIncrementable<O> && CopyConstructible<F> &&
            Writable<O, indirect_result_t<F&, projected<I1, Proj1>,
                                          projected<std::decay_t<I2>, Proj2>>>,
        binary_transform_result<I1, std::decay_t<I2>, O>>
    operator()(I1 first1, S1 last1, I2&& first2, O result, F op,
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl3(std::move(first1), std::move(last1),
                                          std::forward<I2>(first2), std::move(result),
                                          op, proj1, proj2);
    }

    // binary op, range-and-a-half
    template <typename Rng1, typename I2, typename O, typename F,
              typename Proj1 = identity, typename Proj2 = identity>
    NANO_DEPRECATED constexpr std::enable_if_t<
        InputRange<Rng1> && InputIterator<std::decay_t<I2>> &&
                !InputRange<I2> && WeaklyIncrementable<O> &&
            CopyConstructible<F> &&
            Writable<O,
                     indirect_result_t<F&, projected<iterator_t<Rng1>, Proj1>,
                                       projected<std::decay_t<I2>, Proj2>>>,
        binary_transform_result<safe_iterator_t<Rng1>, std::decay_t<I2>, O>>
    operator()(Rng1&& rng1, I2&& first2, O result, F op, Proj1 proj1 = Proj1{},
               Proj2 proj2 = Proj2{}) const
    {
        return transform_fn::binary_impl3(nano::begin(rng1), nano::end(rng1),
                                          std::forward<I2>(first2), std::move(result),
                                          op, proj1, proj2);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::transform_fn, transform)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

template <typename I1, typename I2, typename O>
using merge_result = binary_transform_result<I1, I2, O>;

namespace detail {

struct merge_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr merge_result<I1, I2, O>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, O result, Comp& comp,
         Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1) {
            // If we've reached the end of the second range, copy any remaining
            // elements from the first range directly
            if (first2 == last2) {
                auto res = nano::copy(std::move(first1),
                                      std::move(last1),
                                      std::move(result));
                first1 = std::move(res.in);
                result = std::move(res.out);
                break;
            }

            // (Only) if the element from range2 compares less than the element
            // from range1, copy it. Otherwise copy the element from the first
            if (nano::invoke(comp, nano::invoke(proj2, *first2),
                             nano::invoke(proj1, *first1))) {
                *result = *first2;
                ++first2;
            } else {
                *result = *first1;
                ++first1;
            }
            ++result;
        }

        // We've reached the end of range1, so copy any remaining elements
        // from range2
        auto res = nano::copy(std::move(first2), std::move(last2),
                              std::move(result));
        first2 = std::move(res.in);
        result = std::move(res.out);

        return {std::move(first1), std::move(first2),  std::move(result)};
    }

public:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        WeaklyIncrementable<O> &&
        Mergeable<I1, I2, O, Comp, Proj1, Proj1>,
        merge_result<I1, I2, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return merge_fn::impl(std::move(first1), std::move(last1),
                              std::move(first2), std::move(last2),
                              std::move(result), comp,
                              proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        WeaklyIncrementable<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp, Proj1, Proj2>,
        merge_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return merge_fn::impl(nano::begin(rng1), nano::end(rng1),
                              nano::begin(rng2), nano::end(rng2),
                              std::move(result), comp,
                              proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::merge_fn, merge)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/min.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MIN_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MIN_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct min_fn {
private:
    template <typename Rng, typename Comp, typename Proj>
    static constexpr iter_value_t<iterator_t<Rng>>
    impl(Rng&& rng, Comp& comp, Proj& proj)
    {
        auto first = nano::begin(rng);
        const auto last = nano::end(rng);

        // Empty ranges not allowed
        auto result = *first;

        while(++first != last) {
            auto&& val = *first;
            if (nano::invoke(comp, nano::invoke(proj, val),
                             nano::invoke(proj, result))) {
                result = std::forward<decltype(val)>(val);
            }
        }

        return result;
    }

public:
    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
        const T&>
    operator()(const T& a, const T& b, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return nano::invoke(comp, nano::invoke(proj, b),
                            nano::invoke(proj, a)) ? b : a;
    }

    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        Copyable<T> &&
        IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
        T>
    operator()(std::initializer_list<T> rng, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return min_fn::impl(rng, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        Copyable<iter_value_t<iterator_t<Rng>>> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        iter_value_t<iterator_t<Rng>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return min_fn::impl(std::forward<Rng>(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::min_fn, min)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/min_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MIN_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MIN_ELEMENT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct min_element_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return first;
        }

        I i = nano::next(first);
        while (i != last) {
            if (nano::invoke(comp, nano::invoke(proj, *i),
                             nano::invoke(proj, *first))) {
                first = i;
            }
            ++i;
        }

        return first;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return min_element_fn::impl(std::move(first), std::move(last),
                                    comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return min_element_fn::impl(nano::begin(rng), nano::end(rng),
                                    comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::min_element_fn, min_element)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/minmax.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MINMAX_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MINMAX_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename T>
struct minmax_result {
    T min;
    T max;
};

namespace detail {

struct minmax_fn {
private:
    template <typename Rng, typename Comp, typename Proj,
              typename T = iter_value_t<iterator_t<Rng>>>
    static constexpr minmax_result<T>
    impl(Rng&& rng, Comp& comp, Proj& proj)
    {
        auto first = nano::begin(rng);
        const auto last = nano::end(rng);

        // Empty ranges not allowed
        auto temp = *first;
        minmax_result<T> result{temp, std::move(temp)};

        if (++first != last) {
            {
                auto&& val = *first;
                if (nano::invoke(comp, nano::invoke(proj, val),
                                 nano::invoke(proj, result.min))) {
                    result.min = std::forward<decltype(val)>(val);
                } else if (!nano::invoke(comp, nano::invoke(proj, val),
                                         nano::invoke(proj, result.max))){
                    result.max = std::forward<decltype(val)>(val);
                }
            }

            while (++first != last) {
                T val1 = *first;

                // Last iteration
                if (++first == last) {
                    if (nano::invoke(comp, nano::invoke(proj, val1),
                                     nano::invoke(proj, result.min))) {
                        result.min = std::move(val1);
                    }
                    else if (!nano::invoke(comp, nano::invoke(proj, val1),
                                           nano::invoke(proj, result.max))){
                        result.max = std::move(val1);
                    }
                    break;
                }

                auto&& val2 = *first;
                if (nano::invoke(comp, nano::invoke(proj, val2),
                                 nano::invoke(proj, val1))) {
                    if (nano::invoke(comp, nano::invoke(proj, val2),
                                     nano::invoke(proj, result.min))) {
                        result.min = std::forward<decltype(val2)>(val2);
                    }
                    if (!nano::invoke(comp, nano::invoke(proj, val1),
                                      nano::invoke(proj, result.max))) {
                        result.max = std::move(val1);
                    }
                } else {
                    if (nano::invoke(comp, nano::invoke(proj, val1),
                                     nano::invoke(proj, result.min))) {
                        result.min = std::move(val1);
                    }
                    if (!nano::invoke(comp, nano::invoke(proj, val2),
                                      nano::invoke(proj, result.max))) {
                        result.max = std::forward<decltype(val2)>(val2);
                    }
                }
            }
        }

        return result;
    }

public:
    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
        minmax_result<const T&>>
    operator()(const T& a, const T& b, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        if (nano::invoke(comp, nano::invoke(proj, b), nano::invoke(proj, a))) {
            return {b, a};
        } else {
            return {a, b};
        }
    }

    template <typename T, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
            Copyable<T> &&
            IndirectStrictWeakOrder<Comp, projected<const T*, Proj>>,
        minmax_result<T>>
    operator()(std::initializer_list<T> rng, Comp comp = Comp{},
               Proj proj = Proj{}) const
    {
        return minmax_fn::impl(rng, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        Copyable<iter_value_t<iterator_t<Rng>>> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        minmax_result<iter_value_t<iterator_t<Rng>>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_fn::impl(std::forward<Rng>(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::minmax_fn, minmax)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/minmax_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct minmax_element_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr minmax_result<I> impl(I first, S last, Comp& comp, Proj& proj)
    {
        minmax_result<I> result{first, first};

        if (first == last || ++first == last) {
            return result;
        }

       if (nano::invoke(comp, nano::invoke(proj, *first),
                        nano::invoke(proj, *result.min))) {
           result.min = first;
       } else if (!nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.max))){
           result.max = first;
       }

       while (++first != last) {
           I it = first;

           // Last iteration
           if (++first == last) {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.min))) {
                   result.min = std::move(it);
               }
               else if (!nano::invoke(comp, nano::invoke(proj, *it),
                                      nano::invoke(proj, *result.max))) {
                   result.max = std::move(it);
               }
               break;
           }

           if (nano::invoke(comp, nano::invoke(proj, *first),
                            nano::invoke(proj, *it))) {
               if (nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.min))) {
                   result.min = first;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *it),
                                 nano::invoke(proj, *result.max))) {
                   result.max = it;
               }
           }
           else {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.min))) {
                   result.min = it;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *first),
                                 nano::invoke(proj, *result.max))) {
                   result.max = first;
               }
           }
       }

        return result;
    }


public:
    template <typename I, typename S, typename Comp = less<>,
            typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>,
        minmax_result<I>>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_element_fn::impl(std::move(first), std::move(last),
                                    comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        minmax_result<safe_iterator_t<Rng>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_element_fn::impl(nano::begin(rng), nano::end(rng),
                                       comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::minmax_element_fn, minmax_element)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MOVE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using move_result = copy_result<I, O>;

namespace detail {

struct move_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<SizedSentinel<S, I>, move_result<I, O>>
    impl(I first, S last, O result, priority_tag<1>)
    {
        const auto dist = last - first;

        for (iter_difference_t<I> i{0}; i < dist; i++) {
            *result = nano::iter_move(first);
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr move_result<I, O> impl(I first, S last, O result,
                                            priority_tag<0>)
    {
        while (first != last) {
            *result = nano::iter_move(first);
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<InputIterator<I> && Sentinel<S, I> &&
                                   WeaklyIncrementable<O> &&
                                   IndirectlyMovable<I, O>,
                               move_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_fn::impl(std::move(first), std::move(last),
                             std::move(result), priority_tag<1>{});
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<InputRange<Rng> && WeaklyIncrementable<O> &&
                                   IndirectlyMovable<iterator_t<Rng>, O>,
                               move_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return move_fn::impl(nano::begin(rng), nano::end(rng),
                             std::move(result), priority_tag<1>{});
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::move_fn, move)

template <typename I1, typename I2>
using move_backward_result = copy_result<I1, I2>;

namespace detail {

struct move_backward_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr move_backward_result<I, O> impl(I first, S sent, O result)
    {
        auto last = nano::next(first, std::move(sent));
        auto it = last;

        while (it != first) {
            *--result = nano::iter_move(--it);
        }

        return {std::move(last), std::move(result)};
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<BidirectionalIterator<I> && Sentinel<S, I> &&
                                   BidirectionalIterator<O> &&
                                   IndirectlyMovable<I, O>,
                               move_backward_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return move_backward_fn::impl(std::move(first), std::move(last),
                                      std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<BidirectionalRange<Rng> &&
                                   BidirectionalIterator<O> &&
                                   IndirectlyMovable<iterator_t<Rng>, O>,
                               move_backward_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return move_backward_fn::impl(nano::begin(rng), nano::end(rng),
                                      std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::move_backward_fn, move_backward)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/next_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Taken from Range-V3
//
// Copyright Eric Niebler 2014-2018
//
//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_NEXT_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_NEXT_PERMUTATION_HPP_INCLUDED

// nanorange/algorithm/reverse.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REVERSE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REVERSE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct reverse_fn {
private:
    template <typename I>
    static constexpr I impl(I first, I last)
    {
        I ret = last;
        while (first != last && first !=  --last) {
            nano::iter_swap(first, last);
            ++first;
        }

        return ret;
    }

    template <typename I, typename S>
    static constexpr std::enable_if_t<
        !Same<I, S>, I>
    impl(I first, S bound)
    {
        I last = next(first, bound);
        return reverse_fn::impl(std::move(first), std::move(last));
    }

public:
    template <typename I, typename S>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I>,
        I>
    operator()(I first, S last) const
    {
        return reverse_fn::impl(std::move(first), std::move(last));
    }

    template <typename Rng>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const
    {
        return reverse_fn::impl(nano::begin(rng), nano::end(rng));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::reverse_fn, reverse)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct next_permutation_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr bool impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return false;
        }

        const I last_it = nano::next(first, last);
        I i = last_it;

        if (first == --i) {
            return false;
        }

        while (true) {
            I ip1 = i;

            if (nano::invoke(comp, nano::invoke(proj, *--i),
                             nano::invoke(proj, *ip1))) {
                I j = last_it;
                while (!nano::invoke(comp, nano::invoke(proj, *i),
                                     nano::invoke(proj, *--j)));

                nano::iter_swap(i, j);
                nano::reverse(ip1, last_it);
                return true;
            }

            if (i == first) {
                nano::reverse(first, last);
                return false;
            }
        }
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return next_permutation_fn::impl(std::move(first), std::move(last),
                                         comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>, bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return next_permutation_fn::impl(nano::begin(rng), nano::end(rng),
                                         comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::next_permutation_fn, next_permutation)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/none_of.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_NONE_OF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_NONE_OF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [ranges.alg.none_of]

namespace detail {

struct none_of_fn {

    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(first, last, pred, proj);
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(nano::begin(rng), nano::end(rng),
                                pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::none_of_fn, none_of)

NANO_END_NAMESPACE

#endif
// nanorange/algorithm/partial_sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Copyright Eric Niebler 2014
// Copyright Casey Carter 2015
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)



#ifndef NANORANGE_ALGORITHM_PARTIAL_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTIAL_SORT_HPP_INCLUDED


// nanorange/algorithm/sort_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SORT_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SORT_HEAP_HPP_INCLUDED

// nanorange/algorithm/pop_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_POP_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_POP_HEAP_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct pop_heap_fn {
private:
    friend struct sort_heap_fn;

    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n > 1) {
            nano::iter_swap(first, first + (n - 1));
            detail::sift_down_n(first, n - 1, first, comp, proj);
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> && Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return pop_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<RandomAccessRange<Rng> &&
                                   Sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return pop_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                 proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::pop_heap_fn, pop_heap)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct sort_heap_fn {
private:
    template <typename I, typename Comp, typename Proj>
    static constexpr I impl(I first, iter_difference_t<I> n, Comp& comp,
                            Proj& proj)
    {
        if (n < 2) {
            return first + n;
        }

        for (auto i = n; i > 1; --i) {
            pop_heap_fn::impl(first, i, comp, proj);
        }

        return first + n;
    }

public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> && Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        return sort_heap_fn::impl(std::move(first), n, comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<RandomAccessRange<Rng> &&
                                   Sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return sort_heap_fn::impl(nano::begin(rng), nano::distance(rng), comp,
                                  proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::sort_heap_fn, sort_heap)

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct partial_sort_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr I impl(I first, I middle, S last, Comp& comp, Proj& proj)
    {
        nano::make_heap(first, middle, comp, proj);
        const auto len = nano::distance(first, middle);
        I i = middle;

        while (i != last) {
            if (nano::invoke(comp, nano::invoke(proj, *i), nano::invoke(proj, *first))) {
                nano::iter_swap(i, first);
                detail::sift_down_n(first, len, first, comp, proj);
            }
            ++i;
        }
        nano::sort_heap(first, middle, comp, proj);
        return i;
    }

public:
    template <typename I, typename S, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, I>
    operator()(I first, I middle, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return partial_sort_fn::impl(std::move(first), std::move(middle),
                                     std::move(last), comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>, safe_iterator_t<Rng>>
    operator()(Rng&& rng, iterator_t<Rng> middle, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return partial_sort_fn::impl(nano::begin(rng), std::move(middle),
                                     nano::end(rng), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::partial_sort_fn, partial_sort)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/partial_sort_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTIAL_SORT_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTIAL_SORT_COPY_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct partial_sort_copy_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr I2 impl(I1 first, S1 last, I2 result_first,
                             S2 result_last, Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        I2 r = result_first;
        if (r == result_last) {
            return r;
        }

        while (r != result_last && first != last) {
            *r = *first;
            ++r;
            ++first;
        }

        nano::make_heap(result_first, r, comp, proj2);
        const auto len = nano::distance(result_first, r);

        while (first != last) {
            iter_reference_t<I1>&& x = *first;
            if (nano::invoke(comp, nano::invoke(proj1, x), nano::invoke(proj2, *result_first))) {
                *result_first = std::forward<iter_reference_t<I1>>(x);
                detail::sift_down_n(result_first, len, result_first, comp, proj2);
            }
            ++first;
        }

        nano::sort_heap(result_first, r, comp, proj2);

        return r;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2,
              typename Comp = less<>, typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        RandomAccessIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectlyCopyable<I1, I2> &&
        Sortable<I2, Comp, Proj2> &&
        IndirectStrictWeakOrder<Comp, projected<I1, Proj1>, projected<I2, Proj2>>,
    I2>
    operator()(I1 first, S1 last, I2 result_first, S2 result_last, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return partial_sort_copy_fn::impl(std::move(first), std::move(last),
                                   std::move(result_first), std::move(result_last),
                                   comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        RandomAccessRange<Rng2> &&
        IndirectlyCopyable<iterator_t<Rng1>, iterator_t<Rng2>> &&
        Sortable<iterator_t<Rng2>, Comp, Proj2> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng1>, Proj1>, projected<iterator_t<Rng2>, Proj2>>,
    safe_iterator_t<Rng2>>
    operator()(Rng1&& rng, Rng2&& result_rng, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return partial_sort_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                          nano::begin(result_rng), nano::end(result_rng),
                                          comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::partial_sort_copy_fn, partial_sort_copy)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/partition.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct partition_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if_not(std::move(first), last, pred, proj);

        if (first == last) {
            return first;
        }

        auto n = nano::next(first);

        while (n != last) {
            if (nano::invoke(pred, nano::invoke(proj, *n))) {
                nano::iter_swap(n, first);
                ++first;
            }
            ++n;
        }

        return first;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(std::move(first), std::move(last),
                                  pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return partition_fn::impl(nano::begin(rng), nano::end(rng),
                                  pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::partition_fn, partition)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/partition_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PARTITION_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PARTITION_COPY_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O1, typename O2>
struct partition_copy_result {
    I in;
    O1 out1;
    O2 out2;
};

namespace detail {

struct partition_copy_fn {
private:
    template <typename I, typename S, typename O1, typename O2,
              typename Pred, typename Proj>
    static constexpr partition_copy_result<I, O1, O2>
    impl(I first, S last, O1 out_true, O2 out_false, Pred& pred, Proj& proj)
    {
        while (first != last) {
            auto&& val = *first;
            if (nano::invoke(pred, nano::invoke(proj, val))) {
                *out_true = std::forward<decltype(val)>(val);
                ++out_true;
            } else {
                *out_false = std::forward<decltype(val)>(val);
                ++out_false;
            }
            ++first;
        }

        return {std::move(first), std::move(out_true), std::move(out_false)};
    }

public:
    template <typename I, typename S, typename O1, typename O2,
              typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        WeaklyIncrementable<O1> &&
        WeaklyIncrementable<O2> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>> &&
        IndirectlyCopyable<I, O1> &&
        IndirectlyCopyable<I, O2>,
        partition_copy_result<I, O1, O2>>
    operator()(I first, S last, O1 out_true, O2 out_false, Pred pred,
               Proj proj = Proj{}) const
    {
        return partition_copy_fn::impl(std::move(first), std::move(last),
                                       std::move(out_true), std::move(out_false),
                                       pred, proj);
    }

    template <typename Rng, typename O1, typename O2,
            typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        WeaklyIncrementable<O1> &&
        WeaklyIncrementable<O2> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>> &&
        IndirectlyCopyable<iterator_t<Rng>, O1> &&
        IndirectlyCopyable<iterator_t<Rng>, O2>,
        partition_copy_result<safe_iterator_t<Rng>, O1, O2>>
    operator()(Rng&& rng, O1 out_true, O2 out_false, Pred pred,
            Proj proj = Proj{}) const
    {
        return partition_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                       std::move(out_true), std::move(out_false),
                                       pred, proj);
    }
};

}

NANO_INLINE_VAR(detail::partition_copy_fn, partition_copy)

NANO_END_NAMESPACE

#endif



// nanorange/algorithm/prev_permutation.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Taken from Range-V3
//
// Copyright Eric Niebler 2014-2018
//
//===-------------------------- algorithm ---------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_PREV_PERMUTATION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PREV_PERMUTATION_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct prev_permutation_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr bool impl(I first, S last, Comp& comp, Proj& proj)
    {
        if (first == last) {
            return false;
        }

        const I last_it = nano::next(first, last);
        I i = last_it;

        if (first == --i) {
            return false;
        }

        while (true) {
            I ip1 = i;

            if (nano::invoke(comp, nano::invoke(proj, *ip1),
                             nano::invoke(proj, *--i))) {
                I j = last_it;

                while (!nano::invoke(comp, nano::invoke(proj, *--j),
                                     nano::invoke(proj, *i)));

                nano::iter_swap(i, j);
                nano::reverse(ip1, last_it);
                return true;
            }

            if (i == first) {
                nano::reverse(first, last_it);
                return false;
            }
        }
    }


public:
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, bool>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return prev_permutation_fn::impl(std::move(first), std::move(last),
                                         comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>, bool>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return prev_permutation_fn::impl(nano::begin(rng), nano::end(rng),
                                         comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::prev_permutation_fn, prev_permutation)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/push_heap.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_PUSH_HEAP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_PUSH_HEAP_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct push_heap_fn {
    template <typename I, typename S, typename Comp = less<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> && Sentinel<S, I> && Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(first, last);
        detail::sift_up_n(first, n, comp, proj);
        return first + n;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<RandomAccessRange<Rng> &&
                                   Sortable<iterator_t<Rng>, Comp, Proj>,
                               safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        const auto n = nano::distance(rng);
        detail::sift_up_n(nano::begin(rng), n, comp, proj);
        return nano::begin(rng) + n;
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::push_heap_fn, push_heap)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/remove.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_HPP_INCLUDED





NANO_BEGIN_NAMESPACE

namespace detail {

struct remove_fn {
private:
    template <typename I, typename S, typename T, typename Proj>
    static constexpr I impl(I first, S last, const T& value, Proj& proj)
    {
        first = nano::find(std::move(first), last, value, proj);

        if (first == last) {
            return first;
        }

        for (auto i = next(first); i != last; ++i) {
            if (!(nano::invoke(proj, *i) == value)) {
                *first = nano::iter_move(i);
                ++first;
            }
        }

        return first;
    }

public:
    template <typename I, typename S, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> && Sentinel<S, I> && Permutable<I> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
        I>
    operator()(I first, S last, const T& value, Proj proj = Proj{}) const
    {
        return remove_fn::impl(std::move(first), std::move(last), value, proj);
    }

    template <typename Rng, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> && Permutable<iterator_t<Rng>> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T*>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& value, Proj proj = Proj{}) const
    {
        return remove_fn::impl(nano::begin(rng), nano::end(rng), value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_fn, remove)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/remove_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_COPY_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using remove_copy_result = copy_result<I, O>;

namespace detail {

struct remove_copy_fn {
private:
    template <typename I, typename S, typename O, typename T, typename Proj>
    static constexpr remove_copy_result<I, O>
    impl(I first, S last, O result, const T& value, Proj& proj)
    {
        while (first != last) {
            auto&& ref = *first;
            if (!(nano::invoke(proj, ref) == value)) {
                *result = std::forward<decltype(ref)>(ref);
                ++result;
            }
            ++first;
        }
        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && WeaklyIncrementable<O> &&
            IndirectlyCopyable<I, O> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
        remove_copy_result<I, O>>
    operator()(I first, S last, O result, const T& value,
               Proj proj = Proj{}) const
    {
        return remove_copy_fn::impl(std::move(first), std::move(last),
                                    std::move(result), value, proj);
    }

    template <typename Rng, typename O, typename T, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && WeaklyIncrementable<O> &&
            IndirectlyCopyable<iterator_t<Rng>, O> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T*>,
        remove_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, const T& value, Proj proj = Proj{}) const
    {
        return remove_copy_fn::impl(nano::begin(rng), nano::end(rng),
                               std::move(result), value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_copy_fn, remove_copy)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/remove_copy_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_COPY_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_COPY_IF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using remove_copy_if_result = copy_result<I, O>;

namespace detail {

struct remove_copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename Proj>
    static constexpr remove_copy_if_result<I, O>
    impl(I first, S last, O result, Pred& pred, Proj& proj)
    {
        while (first != last) {
            auto&& ref = *first;
            if (!nano::invoke(pred, nano::invoke(proj, ref))) {
                *result = std::forward<decltype(ref)>(ref);
                ++result;
            }
            ++first;
        }
        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename Pred,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && WeaklyIncrementable<O> &&
            IndirectlyCopyable<I, O> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        remove_copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred,
               Proj proj = Proj{}) const
    {
        return remove_copy_if_fn::impl(std::move(first), std::move(last),
                                    std::move(result), pred, proj);
    }

    template <typename Rng, typename O, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && WeaklyIncrementable<O> &&
            IndirectlyCopyable<iterator_t<Rng>, O> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        remove_copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, Proj proj = Proj{}) const
    {
        return remove_copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                       std::move(result), pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_copy_if_fn, remove_copy_if)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/remove_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REMOVE_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REMOVE_IF_HPP_INCLUDED





NANO_BEGIN_NAMESPACE

namespace detail {

struct remove_if_fn {
private:
    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, Proj& proj)
    {
        first = nano::find_if(std::move(first), last, pred, proj);

        if (first == last) {
            return first;
        }

        for (auto i = next(first); i != last; ++i) {
            if (!nano::invoke(pred, nano::invoke(proj, *i))) {
                *first = nano::iter_move(i);
                ++first;
            }
        }

        return first;
    }

public:
    template <typename I, typename S, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> && Sentinel<S, I> && Permutable<I> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return remove_if_fn::impl(std::move(first), std::move(last), pred, proj);
    }

    template <typename Rng, typename Pred, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> && Permutable<iterator_t<Rng>> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return remove_if_fn::impl(nano::begin(rng), nano::end(rng), pred, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::remove_if_fn, remove_if)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/replace.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct replace_fn {
private:
    template <typename I, typename S, typename T1, typename T2, typename Proj>
    static constexpr I impl(I first, S last, const T1& old_value,
                            const T2& new_value, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(proj, *first) == old_value) {
                *first = new_value;
            }
            ++first;
        }

        return std::move(first);
    }

public:
    template <typename I, typename S, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && Writable<I, const T2&> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T1*>,
        I>
    operator()(I first, S last, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_fn::impl(std::move(first), std::move(last), old_value,
                                new_value, proj);
    }

    template <typename Rng, typename T1, typename T2, typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && Writable<iterator_t<Rng>, const T2&> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T1*>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_fn::impl(nano::begin(rng), nano::end(rng), old_value,
                                new_value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_fn, replace)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/replace_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_COPY_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using replace_copy_result = copy_result<I, O>;

namespace detail {

struct replace_copy_fn {
private:
    template <typename I, typename S, typename O, typename T1, typename T2,
              typename Proj>
    static constexpr replace_copy_result<I, O>
    impl(I first, S last, O result, const T1& old_value, const T2& new_value,
         Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(proj, *first) == old_value) {
                *result = new_value;
            } else {
                *result = *first;
            }
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && OutputIterator<O, const T2&> &&
            IndirectlyCopyable<I, O> &&
            IndirectRelation<equal_to<>, projected<I, Proj>, const T1*>,
        replace_copy_result<I, O>>
    operator()(I first, S last, O result, const T1& old_value,
               const T2& new_value, Proj proj = Proj{}) const
    {
        return replace_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result), old_value, new_value,
                                     proj);
    }

    template <typename Rng, typename O, typename T1, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && OutputIterator<O, const T2&> &&
            IndirectlyCopyable<iterator_t<Rng>, O> &&
            IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>,
                             const T1*>,
        replace_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, const T1& old_value, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                     std::move(result), old_value, new_value,
                                     proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_copy_fn, replace_copy)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/replace_copy_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_COPY_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_COPY_IF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using replace_copy_if_result = copy_result<I, O>;

namespace detail {

struct replace_copy_if_fn {
private:
    template <typename I, typename S, typename O, typename Pred, typename T,
              typename Proj>
    static constexpr replace_copy_if_result<I, O>
    impl(I first, S last, O result, Pred& pred, const T& new_value, Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                *result = new_value;
            } else {
                *result = *first;
            }
            ++first;
            ++result;
        }

        return {std::move(first), std::move(result)};
    }

public:
    template <typename I, typename S, typename O, typename Pred, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && OutputIterator<O, const T&> &&
            IndirectlyCopyable<I, O> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        replace_copy_if_result<I, O>>
    operator()(I first, S last, O result, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_if_fn::impl(std::move(first), std::move(last),
                                        std::move(result), pred, new_value,
                                        proj);
    }

    template <typename Rng, typename O, typename Pred, typename T,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && OutputIterator<O, const T&> &&
            IndirectlyCopyable<iterator_t<Rng>, O> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        replace_copy_if_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_copy_if_fn::impl(nano::begin(rng), nano::end(rng),
                                        std::move(result), pred, new_value,
                                        proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_copy_if_fn, replace_copy_if)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/replace_if.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REPLACE_IF_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REPLACE_IF_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct replace_if_fn {
private:
    template <typename I, typename S, typename Pred, typename T, typename Proj>
    static constexpr I impl(I first, S last, Pred& pred, const T& new_value,
                            Proj& proj)
    {
        while (first != last) {
            if (nano::invoke(pred, nano::invoke(proj, *first))) {
                *first = new_value;
            }
            ++first;
        }

        return std::move(first);
    }

public:
    template <typename I, typename S, typename T, typename Pred,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputIterator<I> && Sentinel<S, I> && Writable<I, const T&> &&
            IndirectUnaryPredicate<Pred, projected<I, Proj>>,
        I>
    operator()(I first, S last, Pred pred, const T& new_value,
               Proj proj = Proj{}) const
    {
        return replace_if_fn::impl(std::move(first), std::move(last), pred,
                                   new_value, proj);
    }

    template <typename Rng, typename Pred, typename T2,
              typename Proj = identity>
    constexpr std::enable_if_t<
        InputRange<Rng> && Writable<iterator_t<Rng>, const T2&> &&
            IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, const T2& new_value,
               Proj proj = Proj{}) const
    {
        return replace_if_fn::impl(nano::begin(rng), nano::end(rng), pred,
                                   new_value, proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::replace_if_fn, replace_if)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/reverse_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_REVERSE_COPY_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using reverse_copy_result = copy_result<I, O>;

namespace detail {

struct reverse_copy_fn {
private:
    template <typename I, typename O>
    static constexpr reverse_copy_result<I, O> impl(I first, I last, O result)
    {
        auto ret = last;
        while (last != first) {
            *result = *--last;
            ++result;
        }

        return {std::move(ret), std::move(result)};
    }

    template <typename I, typename S, typename O>
    static constexpr std::enable_if_t<
        !Same<I, S>, reverse_copy_result<I, O>>
    impl(I first, S bound, O result)
    {
        return reverse_copy_fn::impl(std::move(first), nano::next(first, bound),
                                     std::move(result));
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<
        BidirectionalIterator<I> &&
        Sentinel<S, I> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<I, O>,
        reverse_copy_result<I, O>>
    operator()(I first, S last, O result) const
    {
        return reverse_copy_fn::impl(std::move(first), std::move(last),
                                     std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<
        BidirectionalRange<Rng> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<iterator_t<Rng>, O>,
        reverse_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, O result) const
    {
        return reverse_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                     std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::reverse_copy_fn, reverse_copy)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/rotate.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace detail {

struct rotate_fn {
private:
    template <typename I, typename S>
    static constexpr subrange<I> impl(I first, I middle, S last)
    {
        if (first == middle) {
            auto ret = next(first, last);
            return {ret, ret};
        }
        if (middle == last) {
            return {first, middle};
        }

        I next = middle;

        do {
            nano::iter_swap(first++, next++);
            if (first == middle) {
                middle = next;
            }
        } while (next != last);

        I ret = first;
        next = middle;

        while (next != last) {
            nano::iter_swap(first++, next++);
            if (first == middle) {
                middle = next;
            } else if (next == last) {
                next = middle;
            }
        }

        return {std::move(ret), std::move(next)};
    }

public:
    template <typename I, typename S>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        Permutable<I>,
        subrange<I>>
    operator()(I first, I middle, S last) const
    {
        return rotate_fn::impl(std::move(first), std::move(middle), std::move(last));
    }

    template <typename Rng>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        Permutable<iterator_t<Rng>>,
        safe_subrange_t<Rng>>
    operator()(Rng&& rng, iterator_t<Rng> middle) const
    {
        return rotate_fn::impl(nano::begin(rng), std::move(middle), nano::end(rng));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::rotate_fn, rotate)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/rotate_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_ROTATE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_ROTATE_COPY_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using rotate_copy_result = copy_result<I, O>;

namespace detail {

struct rotate_copy_fn {
private:
    template <typename I, typename S, typename O>
    static constexpr rotate_copy_result<I, O>
    impl(I first, I middle, S last, O result)
    {
        auto ret = nano::copy(middle, std::move(last), std::move(result));
        ret.out = nano::copy(std::move(first), std::move(middle), ret.out).out;
        return ret;
    }

public:
    template <typename I, typename S, typename O>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<I, O>,
        rotate_copy_result<I, O>>
    operator()(I first, I middle, S last, O result) const
    {
        return rotate_copy_fn::impl(std::move(first), std::move(middle),
                                    std::move(last), std::move(result));
    }

    template <typename Rng, typename O>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        WeaklyIncrementable<O> &&
        IndirectlyCopyable<iterator_t<Rng>, O>,
        rotate_copy_result<safe_iterator_t<Rng>, O>>
    operator()(Rng&& rng, iterator_t<Rng> middle, O result) const
    {
        return rotate_copy_fn::impl(nano::begin(rng), std::move(middle),
                                    nano::end(rng), std::move(result));
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::rotate_copy_fn, rotate_copy)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/search_n.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SEARCH_N_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SEARCH_N_HPP_INCLUDED




NANO_BEGIN_NAMESPACE


namespace detail {

struct search_n_fn {
private:
    template <typename I, typename S, typename T, typename Pred, typename Proj>
    static constexpr subrange<I>
    impl(I first, S last, iter_difference_t<I> count, const T& value, Pred pred,
         Proj& proj)
    {
        if (count == iter_difference_t<I>{0}) {
            return {first, first};
        }

        for (; first != last; ++first) {
            if (!nano::invoke(pred, nano::invoke(proj, *first), value)) {
                continue;
            }

            I save = first;
            iter_difference_t<I> running_count{1};

            while (true) {
                if (running_count++ == count) {
                    // Success
                    return {save, nano::next(first)};
                }

                if (++first == last) {
                    // We have run out of elements
                    return {first, first};
                }

                if (!nano::invoke(pred, nano::invoke(proj, *first), value)) {
                    break;
                }
            }
        }

        return {first, first};
    }

public:
    template <typename I, typename S, typename T, typename Pred = equal_to<>,
        typename Proj = identity>
    constexpr auto operator()(I first, S last, iter_difference_t<I> count,
                              const T& value, Pred pred = Pred{},
                              Proj proj = Proj{}) const
    -> std::enable_if_t<ForwardIterator<I> && Sentinel<S, I> &&
                        IndirectlyComparable<I, const T*, Pred, Proj>,
        subrange<I>>
    {
        return search_n_fn::impl(std::move(first), std::move(last), count,
                                 value, pred, proj);
    }

    template <typename Rng, typename T, typename Pred = equal_to<>,
        typename Proj = identity>
    constexpr auto
    operator()(Rng&& rng, iter_difference_t<iterator_t<Rng>> count,
               const T& value, Pred pred = Pred{}, Proj proj = Proj{}) const
    -> std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectlyComparable<iterator_t<Rng>, const T*, Pred, Proj>,
        safe_subrange_t<Rng>>
    {
        return search_n_fn::impl(nano::begin(rng), nano::end(rng), count, value, pred,
                                 proj);
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::search_n_fn, search_n)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/set_difference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SET_DIFFERENCE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SET_DIFFERENCE_HPP_INCLUDED





NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using set_difference_result = copy_result<I, O>;

namespace detail {

struct set_difference_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr set_difference_result<I1, O>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, O result,
         Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1) {
            if (first2 == last2) {
                // We've reached the end of range2, so copy all the remaining
                // elements from range1 and exit
                auto res = nano::copy(std::move(first1),  std::move(last1),
                                      std::move(result));
                first1 = std::move(res.in);
                result = std::move(res.out);

                break;
            }

            // If the element from r1 compares less than the one from r2, then
            // copy it
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                             nano::invoke(proj2, *first2))) {
                *result = *first1;
                ++first1;
                ++result;
            } else{
                // We now know that !(r1 < r2). If !(r2 < r1) as well, then
                // elements are equal and we can skip
                if (!nano::invoke(comp, nano::invoke(proj2, *first2),
                                  nano::invoke(proj1, *first1))) {
                    ++first1;
                }
                ++first2;
            }
        }

        return {std::move(first1), std::move(result)};
    }


public:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        WeaklyIncrementable<O> &&
        Mergeable<I1, I2, O, Comp, Proj1, Proj2>,
        set_difference_result<I1, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_difference_fn::impl(std::move(first1), std::move(last1),
                                       std::move(first2), std::move(last2),
                                       std::move(result), comp,
                                       proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        WeaklyIncrementable<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp, Proj1, Proj2>,
        set_difference_result<safe_iterator_t<Rng1>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_difference_fn::impl(nano::begin(rng1), nano::end(rng1),
                                       nano::begin(rng2), nano::end(rng2),
                                       std::move(result), comp,
                                       proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::set_difference_fn, set_difference)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/set_intersection.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SET_INTERSECTION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SET_INTERSECTION_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct set_intersection_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr O impl(I1 first1, S1 last1, I2 first2, S2 last2,
                            O result, Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1 && first2 != last2)
        {
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                             nano::invoke(proj2, *first2))) {
                ++first1;
            } else {
                if (!nano::invoke(comp, nano::invoke(proj2, *first2),
                                 nano::invoke(proj1, *first1))) {
                    *result = *first1;
                    ++result;
                    ++first1;
                }
                ++first2;
            }
        }

        return result;
    }

public:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        WeaklyIncrementable<O> &&
        Mergeable<I1, I2, O, Comp, Proj1, Proj2>, O>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_intersection_fn::impl(std::move(first1), std::move(last1),
                                         std::move(first2), std::move(last2),
                                         std::move(result), comp, proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        WeaklyIncrementable<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp, Proj1, Proj2>,
         O>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_intersection_fn::impl(nano::begin(rng1), nano::end(rng1),
                                         nano::begin(rng2), nano::end(rng2),
                                         std::move(result), comp, proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::set_intersection_fn, set_intersection)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/set_symmetric_difference.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SET_SYMMETRIC_DIFFERENCE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SET_SYMMETRIC_DIFFERENCE_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename I1, typename I2, typename O>
using set_symmetric_difference_result = binary_transform_result<I1, I2, O>;

namespace detail {

struct set_symmetric_difference_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
            typename Comp, typename Proj1, typename Proj2>
    static constexpr set_symmetric_difference_result<I1, I2, O>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, O result,
         Comp& comp, Proj1& proj1, Proj2& proj2)
    {
        while (true) {
            if (first1 == last1) {
                auto copy_res = nano::copy(std::move(first2), std::move(last2),
                                           std::move(result));


                return {std::move(first1), std::move(copy_res.in),
                        std::move(copy_res.out)};
            }

            if (first2 == last2) {
                auto copy_res = nano::copy(std::move(first1),  std::move(last1),
                                           std::move(result));
                return {std::move(copy_res.in), std::move(first2),
                        std::move(copy_res.out)};
            }

            // If r1 is less than r2, copy it to the output
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                             nano::invoke(proj2, *first2))) {
                *result = *first1;
                ++result;
                ++first1;
            } else {
                // We now know that !(r1 < r2). If !(r2 < r1) as well then
                // the elements are equal -- so skip
                if (!nano::invoke(comp, nano::invoke(proj2, *first2),
                                  nano::invoke(proj1, *first1))) {
                    ++first1;
                } else {
                    // Otherwise copy first2
                    *result = *first2;
                    ++result;
                }
                ++first2;
            }
        }
    }

public:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        WeaklyIncrementable<O> &&
        Mergeable<I1, I2, O, Comp, Proj1, Proj2>,
        set_symmetric_difference_result<I1, I2, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_symmetric_difference_fn::impl(std::move(first1), std::move(last1),
                                                 std::move(first2), std::move(last2),
                                                 std::move(result), comp,
                                                 proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        WeaklyIncrementable<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp, Proj1, Proj2>,
        set_symmetric_difference_result<safe_iterator_t<Rng1>,
                                        safe_iterator_t<Rng2>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_symmetric_difference_fn::impl(nano::begin(rng1), nano::end(rng1),
                                                 nano::begin(rng2), nano::end(rng2),
                                                 std::move(result), comp,
                                                 proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::set_symmetric_difference_fn, set_symmetric_difference)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/set_union.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SET_UNION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SET_UNION_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

template <typename I1, typename I2, typename O>
using set_union_result = binary_transform_result<I1, I2, O>;

namespace detail {

struct set_union_fn {
private:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp, typename Proj1, typename Proj2>
    static constexpr set_union_result<I1, I2, O>
    impl(I1 first1, S1 last1, I2 first2, S2 last2, O result, Comp& comp,
         Proj1& proj1, Proj2& proj2)
    {
        while (first1 != last1) {
            // If we've reached the end of the second range, copy any remaining
            // elements from the first range and quit
            if (first2 == last2) {
                auto copy_res = nano::copy(std::move(first1),  std::move(last1),
                                           std::move(result));

                first1 = std::move(copy_res.in);
                result = std::move(copy_res.out);

                break;
            }

            // If this element from r1 is less than the current element from r2,
            // copy it and move on
            if (nano::invoke(comp, nano::invoke(proj1, *first1),
                             nano::invoke(proj2, *first2))) {
                *result = *first1;
                ++first1;
            } else {
                // Now, we know that !(r1 < r2). If we also have !(r2 < r1) then
                // the elements compare equal, so skip it
                if (!nano::invoke(comp, nano::invoke(proj2, *first2),
                                  nano::invoke(proj1, *first1))) {
                    ++first1;
                }
                *result = *first2;
                ++first2;
            }
            ++result;
        }

        // We've run out of elements of range1, so copy all the remaining
        // elements of range2
        auto copy_res = nano::copy(std::move(first2), std::move(last2),
                                   std::move(result));


        return {std::move(first1), std::move(copy_res.in),
                std::move(copy_res.out)};
    }

public:
    template <typename I1, typename S1, typename I2, typename S2, typename O,
              typename Comp = less<>, typename Proj1 = identity,
              typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        InputIterator<I2> &&
        Sentinel<S2, I2> &&
        WeaklyIncrementable<O> &&
        Mergeable<I1, I2, O, Comp, Proj1, Proj2>,
        set_union_result<I1, I2, O>>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2, O result,
               Comp comp = Comp{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_union_fn::impl(std::move(first1), std::move(last1),
                                  std::move(first2), std::move(last2),
                                  std::move(result), comp,
                                  proj1, proj2);
    }

    template <typename Rng1, typename Rng2, typename O, typename Comp = less<>,
              typename Proj1 = identity, typename Proj2 = identity>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        InputRange<Rng2> &&
        WeaklyIncrementable<O> &&
        Mergeable<iterator_t<Rng1>, iterator_t<Rng2>, O, Comp, Proj1, Proj2>,
        set_union_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>, O>>
    operator()(Rng1&& rng1, Rng2&& rng2, O result, Comp comp = Comp{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return set_union_fn::impl(nano::begin(rng1), nano::end(rng1),
                                  nano::begin(rng2), nano::end(rng2),
                                  std::move(result), comp,
                                  proj1, proj2);
    }
};

}

NANO_INLINE_VAR(detail::set_union_fn, set_union)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/shuffle.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SHUFFLE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SHUFFLE_HPP_INCLUDED


// nanorange/random.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANDOM_HPP_INCLUDED
#define NANORANGE_RANDOM_HPP_INCLUDED

// nanorange/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_CONCEPTS_HPP_INCLUDED






#endif


#include <random>

NANO_BEGIN_NAMESPACE

//  [rand.req.urng]

namespace detail {

struct UniformRandomBitGenerator_req {
    template <typename G>
    auto requires_() -> decltype(valid_expr(
        G::min(),
        requires_expr<Same<decltype(G::min()), invoke_result_t<G&>>>{},
        G::max(),
        requires_expr<Same<decltype(G::max()), invoke_result_t<G&>>>{}));
};

template <typename>
auto UniformRandomBitGenerator_fn(long) -> std::false_type;

template <typename G>
auto UniformRandomBitGenerator_fn(int) -> std::enable_if_t<
        Invocable<G&> &&
        UnsignedIntegral<invoke_result_t<G&>> &&
        requires_<UniformRandomBitGenerator_req, G>,
    std::true_type>;

} // namespace detail

template <typename G>
NANO_CONCEPT UniformRandomBitGenerator =
    decltype(detail::UniformRandomBitGenerator_fn<G>(0))::value;

NANO_END_NAMESPACE

#endif


NANO_BEGIN_NAMESPACE

namespace detail {

struct shuffle_fn {
private:
    template <typename I, typename S, typename Gen>
    static constexpr I impl(I first, S last, Gen&& g)
    {
        using diff_t = iter_difference_t<I>;
        using distr_t = std::uniform_int_distribution<diff_t>;
        using param_t = typename distr_t::param_type;

        distr_t D;
        const auto n = last - first; // OK, we have SizedSentinel

        for (diff_t i = 0; i < n; i++) {
            nano::iter_swap(first + i, first + D(g, param_t(0, i)));
        }

        return next(first, last);
    }

public:
    template <typename I, typename S, typename Gen>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        UniformRandomBitGenerator<std::remove_reference_t<Gen>> &&
        ConvertibleTo<invoke_result_t<Gen&>, iter_difference_t<I>>,
        I>
    operator()(I first, S last, Gen&& gen) const
    {
        return shuffle_fn::impl(std::move(first), std::move(last),
                                std::forward<Gen>(gen));
    }

    template <typename Rng, typename Gen>
    constexpr std::enable_if_t<
            RandomAccessRange<Rng> &&
            UniformRandomBitGenerator<std::remove_reference_t<Gen>> &&
            ConvertibleTo<invoke_result_t<Gen&>, iter_difference_t<iterator_t<Rng>>>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Gen&& gen) const
    {
        return shuffle_fn::impl(nano::begin(rng), nano::end(rng),
                                std::forward<Gen>(gen));
    }
};

}

NANO_INLINE_VAR(detail::shuffle_fn, shuffle)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SORT_HPP_INCLUDED

// nanorange/detail/algorithm/pqdsort.hpp
//
// Copyright Orson Peters 2017.
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Modified from Boost.Sort by Orson Peters
// https://github.com/boostorg/sort/blob/develop/include/boost/sort/pdqsort/pdqsort.hpp

#ifndef NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED
#define NANORANGE_DETAIL_ALGORITHM_PDQSORT_HPP_INCLUDED






NANO_BEGIN_NAMESPACE

namespace detail {

// Partitions below this size are sorted using insertion sort.
constexpr int pdqsort_insertion_sort_threshold = 24;

// Partitions above this size use Tukey's ninther to select the pivot.
constexpr int pdqsort_ninther_threshold = 128;

// When we detect an already sorted partition, attempt an insertion sort that
// allows this amount of element moves before giving up.
constexpr int pqdsort_partial_insertion_sort_limit = 8;

// Must be multiple of 8 due to loop unrolling, and < 256 to fit in unsigned
// char.
constexpr int pdqsort_block_size = 64;

// Cacheline size, assumes power of two.
constexpr int pdqsort_cacheline_size = 64;

template <typename>
struct is_default_compare : std::false_type {};
template <typename T>
struct is_default_compare<nano::less<T>> : std::true_type {};
template <typename T>
struct is_default_compare<nano::greater<T>> : std::true_type {};
template <typename T>
struct is_default_compare<std::less<T>> : std::true_type {};
template <typename T>
struct is_default_compare<std::greater<T>> : std::true_type {};

template <typename T>
constexpr bool is_default_compare_v = is_default_compare<T>::value;

// Returns floor(log2(n)), assumes n > 0.
template <class T>
constexpr int log2(T n)
{
    int log = 0;
    while (n >>= 1)
        ++log;
    return log;
}

// Sorts [begin, end) using insertion sort with the given comparison function.
template <typename I, typename Comp, typename Proj>
constexpr void insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = iter_value_t<I>;

    if (begin == end) {
        return;
    }

    for (I cur = begin + 1; cur != end; ++cur) {
        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (sift != begin &&
                     nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
        }
    }
}

// Sorts [begin, end) using insertion sort with the given comparison function.
// Assumes
// *(begin - 1) is an element smaller than or equal to any element in [begin,
// end).
template <typename I, typename Comp, typename Proj>
constexpr void unguarded_insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = iter_value_t<I>;

    if (begin == end) {
        return;
    }

    for (I cur = begin + 1; cur != end; ++cur) {
        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
        }
    }
}

// Attempts to use insertion sort on [begin, end). Will return false if more
// than partial_insertion_sort_limit elements were moved, and abort sorting.
// Otherwise it will successfully sort and return true.
template <typename I, typename Comp, typename Proj>
constexpr bool partial_insertion_sort(I begin, I end, Comp& comp, Proj& proj)
{
    using T = iter_value_t<I>;

    if (begin == end) {
        return true;
    }

    iter_difference_t<I> limit = 0;
    for (I cur = begin + 1; cur != end; ++cur) {
        if (limit > pqdsort_partial_insertion_sort_limit) {
            return false;
        }

        I sift = cur;
        I sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for an element already
        // positioned correctly.
        if (nano::invoke(comp, nano::invoke(proj, *sift),
                         nano::invoke(proj, *sift_1))) {
            T tmp = nano::iter_move(sift);

            do {
                *sift-- = nano::iter_move(sift_1);
            } while (sift != begin &&
                     nano::invoke(comp, nano::invoke(proj, tmp),
                                  nano::invoke(proj, *--sift_1)));

            *sift = std::move(tmp);
            limit += cur - sift;
        }
    }

    return true;
}

template <typename I, typename Comp, typename Proj>
constexpr void sort2(I a, I b, Comp& comp, Proj& proj)
{
    if (nano::invoke(comp, nano::invoke(proj, *b), nano::invoke(proj, *a))) {
        nano::iter_swap(a, b);
    }
}

// Sorts the elements *a, *b and *c using comparison function comp.
template <typename I, typename Comp, typename Proj>
constexpr void sort3(I a, I b, I c, Comp& comp, Proj& proj)
{
    sort2(a, b, comp, proj);
    sort2(b, c, comp, proj);
    sort2(a, b, comp, proj);
}

template <typename I>
constexpr void swap_offsets(I first, I last, unsigned char* offsets_l,
                            unsigned char* offsets_r, int num, bool use_swaps)
{
    using T = iter_value_t<I>;
    if (use_swaps) {
        // This case is needed for the descending distribution, where we need
        // to have proper swapping for pdqsort to remain O(n).
        for (int i = 0; i < num; ++i) {
            nano::iter_swap(first + offsets_l[i], last - offsets_r[i]);
        }
    } else if (num > 0) {
        I l = first + offsets_l[0];
        I r = last - offsets_r[0];
        T tmp(nano::iter_move(l));
        *l = nano::iter_move(r);

        for (int i = 1; i < num; ++i) {
            l = first + offsets_l[i];
            *r = nano::iter_move(l);
            r = last - offsets_r[i];
            *l = nano::iter_move(r);
        }
        *r = std::move(tmp);
    }
}

// Partitions [begin, end) around pivot *begin using comparison function comp.
// Elements equal to the pivot are put in the right-hand partition. Returns the
// position of the pivot after partitioning and whether the passed sequence
// already was correctly partitioned. Assumes the pivot is a median of at least
// 3 elements and that [begin, end) is at least insertion_sort_threshold long.
// Uses branchless partitioning.
template <typename I, typename Comp, typename Pred>
constexpr std::pair<I, bool> partition_right_branchless(I begin, I end,
                                                        Comp& comp, Pred& pred)
{
    using T = iter_value_t<I>;

    // Move pivot into local for speed.
    T pivot(nano::iter_move(begin));
    I first = begin;
    I last = end;

    // Find the first element greater than or equal than the pivot (the median
    // of 3 guarantees this exists).
    while (nano::invoke(comp, nano::invoke(pred, *++first),
                        nano::invoke(pred, pivot)))
        ;

    // Find the first element strictly smaller than the pivot. We have to guard
    // this search if there was no element before *first.
    if (first - 1 == begin) {
        while (first < last && !nano::invoke(comp, nano::invoke(pred, *--last),
                                             nano::invoke(pred, pivot)))
            ;
    } else {
        while (!nano::invoke(comp, nano::invoke(pred, *--last),
                             nano::invoke(pred, pivot)))
            ;
    }

    // If the first pair of elements that should be swapped to partition are the
    // same element, the passed in sequence already was correctly partitioned.
    bool already_partitioned = first >= last;
    if (!already_partitioned) {
        nano::iter_swap(first, last);
        ++first;
    }

    // The following branchless partitioning is derived from "BlockQuicksort:
    // How Branch Mispredictions don't affect Quicksort" by Stefan Edelkamp and
    // Armin Weiss.
    alignas(pdqsort_cacheline_size) unsigned char
        offsets_l_storage[pdqsort_block_size] = {};
    alignas(pdqsort_cacheline_size) unsigned char
        offsets_r_storage[pdqsort_block_size] = {};
    unsigned char* offsets_l = offsets_l_storage;
    unsigned char* offsets_r = offsets_r_storage;
    int num_l = 0, num_r = 0, start_l = 0, start_r = 0;

    while (last - first > 2 * pdqsort_block_size) {
        // Fill up offset blocks with elements that are on the wrong side.
        if (num_l == 0) {
            start_l = 0;
            I it = first;
            for (unsigned char i = 0; i < pdqsort_block_size;) {
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
                offsets_l[num_l] = i++;
                num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                       nano::invoke(pred, pivot));
                ++it;
            }
        }
        if (num_r == 0) {
            start_r = 0;
            I it = last;
            for (unsigned char i = 0; i < pdqsort_block_size;) {
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
                offsets_r[num_r] = ++i;
                num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                      nano::invoke(pred, pivot));
            }
        }

        // Swap elements and update block sizes and first/last boundaries.
        int num = (nano::min)(num_l, num_r);
        swap_offsets(first, last, offsets_l + start_l, offsets_r + start_r, num,
                     num_l == num_r);
        num_l -= num;
        num_r -= num;
        start_l += num;
        start_r += num;
        if (num_l == 0)
            first += pdqsort_block_size;
        if (num_r == 0)
            last -= pdqsort_block_size;
    }

    iter_difference_t<I> l_size = 0, r_size = 0;
    iter_difference_t<I> unknown_left =
        (last - first) - ((num_r || num_l) ? pdqsort_block_size : 0);
    if (num_r) {
        // Handle leftover block by assigning the unknown elements to the other
        // block.
        l_size = unknown_left;
        r_size = pdqsort_block_size;
    } else if (num_l) {
        l_size = pdqsort_block_size;
        r_size = unknown_left;
    } else {
        // No leftover block, split the unknown elements in two blocks.
        l_size = unknown_left / 2;
        r_size = unknown_left - l_size;
    }

    // Fill offset buffers if needed.
    if (unknown_left && !num_l) {
        start_l = 0;
        I it = first;
        for (unsigned char i = 0; i < l_size;) {
            offsets_l[num_l] = i++;
            num_l += !nano::invoke(comp, nano::invoke(pred, *it),
                                   nano::invoke(pred, pivot));
            ++it;
        }
    }
    if (unknown_left && !num_r) {
        start_r = 0;
        I it = last;
        for (unsigned char i = 0; i < r_size;) {
            offsets_r[num_r] = ++i;
            num_r += nano::invoke(comp, nano::invoke(pred, *--it),
                                  nano::invoke(pred, pivot));
        }
    }

    int num = (nano::min)(num_l, num_r);
    swap_offsets(first, last, offsets_l + start_l, offsets_r + start_r, num,
                 num_l == num_r);
    num_l -= num;
    num_r -= num;
    start_l += num;
    start_r += num;
    if (num_l == 0)
        first += l_size;
    if (num_r == 0)
        last -= r_size;

    // We have now fully identified [first, last)'s proper position. Swap the
    // last elements.
    if (num_l) {
        offsets_l += start_l;
        while (num_l--)
            nano::iter_swap(first + offsets_l[num_l], --last);
        first = last;
    }
    if (num_r) {
        offsets_r += start_r;
        while (num_r--)
            nano::iter_swap(last - offsets_r[num_r], first), ++first;
        last = first;
    }

    // Put the pivot in the right place.
    I pivot_pos = first - 1;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return std::make_pair(std::move(pivot_pos), already_partitioned);
}

// Partitions [begin, end) around pivot *begin using comparison function comp.
// Elements equal to the pivot are put in the right-hand partition. Returns the
// position of the pivot after partitioning and whether the passed sequence
// already was correctly partitioned. Assumes the pivot is a median of at least
// 3 elements and that [begin, end) is at least insertion_sort_threshold long.
template <typename I, typename Comp, typename Proj>
constexpr std::pair<I, bool> partition_right(I begin, I end, Comp& comp,
                                             Proj& proj)
{
    using T = iter_value_t<I>;

    // Move pivot into local for speed.
    T pivot(nano::iter_move(begin));

    I first = begin;
    I last = end;

    // Find the first element greater than or equal than the pivot (the median
    // of 3 guarantees this exists).
    while (nano::invoke(comp, nano::invoke(proj, *++first),
                        nano::invoke(proj, pivot))) {
    }

    // Find the first element strictly smaller than the pivot. We have to guard
    // this search if there was no element before *first.
    if (first - 1 == begin) {
        while (first < last && !nano::invoke(comp, nano::invoke(proj, *--last),
                                             nano::invoke(proj, pivot))) {
        }
    } else {
        while (!nano::invoke(comp, nano::invoke(proj, *--last),
                             nano::invoke(proj, pivot))) {
        }
    }

    // If the first pair of elements that should be swapped to partition are the
    // same element, the passed in sequence already was correctly partitioned.
    bool already_partitioned = first >= last;

    // Keep swapping pairs of elements that are on the wrong side of the pivot.
    // Previously swapped pairs guard the searches, which is why the first
    // iteration is special-cased above.
    while (first < last) {
        nano::iter_swap(first, last);
        while (nano::invoke(comp, nano::invoke(proj, *++first),
                            nano::invoke(proj, pivot)))
            ;
        while (!nano::invoke(comp, nano::invoke(proj, *--last),
                             nano::invoke(proj, pivot)))
            ;
    }

    // Put the pivot in the right place.
    I pivot_pos = first - 1;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return std::make_pair(std::move(pivot_pos), already_partitioned);
}

// Similar function to the one above, except elements equal to the pivot are put
// to the left of the pivot and it doesn't check or return if the passed
// sequence already was partitioned. Since this is rarely used (the many equal
// case), and in that case pdqsort already has O(n) performance, no block
// quicksort is applied here for simplicity.
template <typename I, typename Comp, typename Proj>
constexpr I partition_left(I begin, I end, Comp& comp, Proj& proj)
{
    using T = iter_value_t<I>;

    T pivot(nano::iter_move(begin));
    I first = begin;
    I last = end;

    while (nano::invoke(comp, nano::invoke(proj, pivot),
                        nano::invoke(proj, *--last)))
        ;

    if (last + 1 == end) {
        while (first < last && !nano::invoke(comp, nano::invoke(proj, pivot),
                                             nano::invoke(proj, *++first)))
            ;
    } else {
        while (!nano::invoke(comp, nano::invoke(proj, pivot),
                             nano::invoke(proj, *++first)))
            ;
    }

    while (first < last) {
        nano::iter_swap(first, last);
        while (nano::invoke(comp, nano::invoke(proj, pivot),
                            nano::invoke(proj, *--last)))
            ;
        while (!nano::invoke(comp, nano::invoke(proj, pivot),
                             nano::invoke(proj, *++first)))
            ;
    }

    I pivot_pos = last;
    *begin = nano::iter_move(pivot_pos);
    *pivot_pos = std::move(pivot);

    return pivot_pos;
}

template <bool Branchless, typename I, typename Comp, typename Proj>
constexpr void pdqsort_loop(I begin, I end, Comp& comp, Proj& proj,
                            int bad_allowed, bool leftmost = true)
{
    using diff_t = iter_difference_t<I>;

    // Use a while loop for tail recursion elimination.
    while (true) {
        diff_t size = nano::distance(begin, end);

        // Insertion sort is faster for small arrays.
        if (size < pdqsort_insertion_sort_threshold) {
            if (leftmost) {
                insertion_sort(begin, end, comp, proj);
            } else {
                unguarded_insertion_sort(begin, end, comp, proj);
            }
            return;
        }

        // Choose pivot as median of 3 or pseudomedian of 9.
        diff_t s2 = size / 2;
        if (size > pdqsort_ninther_threshold) {
            sort3(begin, begin + s2, end - 1, comp, proj);
            sort3(begin + 1, begin + (s2 - 1), end - 2, comp, proj);
            sort3(begin + 2, begin + (s2 + 1), end - 3, comp, proj);
            sort3(begin + (s2 - 1), begin + s2, begin + (s2 + 1), comp, proj);
            nano::iter_swap(begin, begin + s2);
        } else {
            sort3(begin + s2, begin, end - 1, comp, proj);
        }

        // If *(begin - 1) is the end of the right partition of a previous
        // partition operation there is no element in [begin, end) that is
        // smaller than *(begin - 1). Then if our pivot compares equal to
        // *(begin - 1) we change strategy, putting equal elements in the left
        // partition, greater elements in the right partition. We do not have to
        // recurse on the left partition, since it's sorted (all equal).
        if (!leftmost && !nano::invoke(comp, nano::invoke(proj, *(begin - 1)),
                                       nano::invoke(proj, *begin))) {
            begin = partition_left(begin, end, comp, proj) + 1;
            continue;
        }

        // Partition and get results.
        std::pair<I, bool> part_result =
            Branchless ? partition_right_branchless(begin, end, comp, proj)
                       : partition_right(begin, end, comp, proj);
        I pivot_pos = part_result.first;
        bool already_partitioned = part_result.second;

        // Check for a highly unbalanced partition.
        diff_t l_size = pivot_pos - begin;
        diff_t r_size = end - (pivot_pos + 1);
        bool highly_unbalanced = l_size < size / 8 || r_size < size / 8;

        // If we got a highly unbalanced partition we shuffle elements to break
        // many patterns.
        if (highly_unbalanced) {
            // If we had too many bad partitions, switch to heapsort to
            // guarantee O(n log n).
            if (--bad_allowed == 0) {
                nano::make_heap(begin, end, comp, proj);
                nano::sort_heap(begin, end, comp, proj);
                return;
            }

            if (l_size >= pdqsort_insertion_sort_threshold) {
                nano::iter_swap(begin, begin + l_size / 4);
                nano::iter_swap(pivot_pos - 1, pivot_pos - l_size / 4);

                if (l_size > pdqsort_ninther_threshold) {
                    nano::iter_swap(begin + 1, begin + (l_size / 4 + 1));
                    nano::iter_swap(begin + 2, begin + (l_size / 4 + 2));
                    nano::iter_swap(pivot_pos - 2,
                                    pivot_pos - (l_size / 4 + 1));
                    nano::iter_swap(pivot_pos - 3,
                                    pivot_pos - (l_size / 4 + 2));
                }
            }

            if (r_size >= pdqsort_insertion_sort_threshold) {
                nano::iter_swap(pivot_pos + 1, pivot_pos + (1 + r_size / 4));
                nano::iter_swap(end - 1, end - r_size / 4);

                if (r_size > pdqsort_ninther_threshold) {
                    nano::iter_swap(pivot_pos + 2,
                                    pivot_pos + (2 + r_size / 4));
                    nano::iter_swap(pivot_pos + 3,
                                    pivot_pos + (3 + r_size / 4));
                    nano::iter_swap(end - 2, end - (1 + r_size / 4));
                    nano::iter_swap(end - 3, end - (2 + r_size / 4));
                }
            }
        } else {
            // If we were decently balanced and we tried to sort an already
            // partitioned sequence try to use insertion sort.
            if (already_partitioned &&
                partial_insertion_sort(begin, pivot_pos, comp, proj) &&
                partial_insertion_sort(pivot_pos + 1, end, comp, proj))
                return;
        }

        // Sort the left partition first using recursion and do tail recursion
        // elimination for the right-hand partition.
        detail::pdqsort_loop<Branchless>(begin, pivot_pos, comp, proj,
                                         bad_allowed, leftmost);
        begin = pivot_pos + 1;
        leftmost = false;
    }
}

template <typename I, typename Comp, typename Proj,
          bool Branchless = is_default_compare_v<std::remove_const_t<Comp>>&&
              Same<Proj, identity>&& std::is_arithmetic<iter_value_t<I>>::value>
constexpr void pdqsort(I begin, I end, Comp& comp, Proj& proj)
{
    if (begin == end) {
        return;
    }

    detail::pdqsort_loop<Branchless>(std::move(begin), std::move(end), comp,
                                     proj,
                                     detail::log2(nano::distance(begin, end)));
}

} // namespace detail

NANO_END_NAMESPACE

#endif

NANO_BEGIN_NAMESPACE

namespace detail {

struct sort_fn {
    template <typename I, typename S, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessIterator<I> &&
        Sentinel<S, I> &&
        Sortable<I, Comp, Proj>, I>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        I last_it = nano::next(first, last);
        detail::pdqsort(std::move(first), last_it, comp, proj);
        return last_it;
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        RandomAccessRange<Rng> &&
        Sortable<iterator_t<Rng>, Comp, Proj>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        iterator_t<Rng> last_it = nano::next(nano::begin(rng), nano::end(rng));
        detail::pdqsort(nano::begin(rng), last_it, comp, proj);
        return last_it;
    }
};

}

NANO_INLINE_VAR(detail::sort_fn, sort)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/swap_ranges.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_SWAP_RANGES_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SWAP_RANGES_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I1, typename I2>
using swap_ranges_result = mismatch_result<I1, I2>;

namespace detail {

struct swap_ranges_fn {
    template <typename I1, typename S1, typename I2, typename S2>
    static constexpr swap_ranges_result<I1, I2>
    impl4(I1 first1 ,S1 last1, I2 first2, S2 last2)
    {
        while (first1 != last1 && first2 != last2) {
            nano::iter_swap(first1, first2);
            ++first1; ++first2;
        }
        return {std::move(first1), std::move(first2)};
    }

    template <typename I1, typename S1, typename I2>
    static constexpr swap_ranges_result<I1, I2>
    impl3(I1 first1, S1 last1, I2 first2)
    {
        while (first1 != last1) {
            nano::iter_swap(first1, first2);
            ++first1; ++first2;
        }
        return {std::move(first1), std::move(first2)};
    }

public:
    template <typename I1, typename S1, typename I2, typename S2>
    constexpr std::enable_if_t<
        ForwardIterator<I1> &&
        Sentinel<S1, I1> &&
        ForwardIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectlySwappable<I1, I2>,
        swap_ranges_result<I1, I2>>
    operator()(I1 first1 ,S1 last1, I2 first2, S2 last2) const
    {
        return swap_ranges_fn::impl4(std::move(first1), std::move(last1),
                                     std::move(first2), std::move(last2));
    }

    template <typename I1, typename S1, typename I2>
    NANO_DEPRECATED
    constexpr std::enable_if_t<
            ForwardIterator<I1> &&
            Sentinel<S1, I1> &&
            ForwardIterator<I2> &&
            IndirectlySwappable<I1, I2>,
            swap_ranges_result<I1, I2>>
    operator()(I1 first1 ,S1 last1, I2 first2) const
    {
        return swap_ranges_fn::impl3(std::move(first1), std::move(last1),
                                     std::move(first2));
    }

    template <typename Rng1, typename Rng2>
    constexpr std::enable_if_t<
            ForwardRange<Rng1> &&
            ForwardRange<Rng2> &&
            IndirectlySwappable<iterator_t<Rng1>, iterator_t<Rng2>>,
            swap_ranges_result<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>>>
    operator()(Rng1&& rng1, Rng2&& rng2) const
    {
        return swap_ranges_fn::impl4(nano::begin(rng1), nano::end(rng1),
                                     nano::begin(rng2), nano::end(rng2));
    }

    template <typename Rng1, typename I2>
    NANO_DEPRECATED
    constexpr std::enable_if_t<
            ForwardRange<Rng1> &&
            ForwardIterator<I2> &&
            IndirectlySwappable<iterator_t<Rng1>, I2>,
            swap_ranges_result<safe_iterator_t<Rng1>, I2>>
    operator()(Rng1&& rng1, I2 first2) const
    {
        return swap_ranges_fn::impl3(nano::begin(rng1), nano::end(rng1),
                                     std::move(first2));
    }
};

}

NANO_INLINE_VAR(detail::swap_ranges_fn, swap_ranges)

NANO_END_NAMESPACE

#endif


// nanorange/algorithm/unique.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UNIQUE_HPP_INCLUDED





NANO_BEGIN_NAMESPACE

namespace detail {

struct unique_fn {
private:
    template <typename I, typename S, typename R, typename Proj>
    static constexpr I impl(I first, S last, R& comp, Proj& proj)
    {
        first = adjacent_find_fn::impl(std::move(first), last, comp, proj);

        if (first == last) {
            return first;
        }

        for (I n = next(first, 2, last); n != last; ++n) {
            if (!nano::invoke(comp, nano::invoke(proj, *first),
                              nano::invoke(proj, *n))) {
                *++first = iter_move(n);
            }
        }

        return ++first;
    }

public:
    template <typename I, typename S, typename R = equal_to<>,
              typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectRelation<R, projected<I, Proj>> &&
        Permutable<I>, I>
    operator()(I first, S last, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(std::move(first), std::move(last),
                               comp, proj);
    }

    template <typename Rng, typename R = equal_to<>, typename Proj = identity>
    constexpr std::enable_if_t<
            ForwardRange<Rng> &&
            IndirectRelation<R, projected<iterator_t<Rng>, Proj>> &&
            Permutable<iterator_t<Rng>>,
            safe_iterator_t<Rng>>
    operator()(Rng&& rng, R comp = {}, Proj proj = Proj{}) const
    {
        return unique_fn::impl(nano::begin(rng), nano::end(rng),
                               comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::unique_fn, unique)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/unique_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_UNIQUE_COPY_HPP_INCLUDED
#define NANORANGE_ALGORITHM_UNIQUE_COPY_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using unique_copy_result = copy_result<I, O>;

namespace detail {

struct unique_copy_fn {
private:
    template <typename I, typename S, typename O,
              typename Comp, typename Proj>
    static constexpr unique_copy_result<I, O>
    impl(I first, S last, O result, Comp& comp, Proj& proj)
    {
        if (first != last) {
            iter_value_t<I> saved = *first;
            *result = saved;
            ++result;

            while (++first != last) {
                auto&& v = *first;
                if (!nano::invoke(comp, nano::invoke(proj, v), nano::invoke(proj, saved))) {
                    saved = std::forward<decltype(v)>(v);
                    *result = saved;
                    ++result;
                }
            }
        }

        return {std::move(first), std::move(result)};
    }

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<2>) -> std::enable_if_t<
        ForwardIterator<I>, std::true_type>;

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<1>) -> std::enable_if_t<
        InputIterator<O> && Same<iter_value_t<I>, iter_value_t<O>>,
        std::true_type>;

    template <typename I, typename O>
    static auto constraint_helper(priority_tag<0>) -> std::enable_if_t<
        IndirectlyCopyableStorable<I, O>, std::true_type>;

public:
    template <typename I, typename S, typename O, typename Comp = equal_to<>,
              typename Proj = identity>
    constexpr auto operator()(I first, S last, O result,
                              Comp comp = Comp{}, Proj proj = Proj{}) const
        -> std::enable_if_t<
               InputIterator<I> &&
               Sentinel<S, I> &&
               WeaklyIncrementable<O> &&
               IndirectRelation<Comp, projected<I, Proj>> &&
               IndirectlyCopyable<I, O> &&
               decltype(constraint_helper<I, O>(priority_tag<2>{}))::value,
        unique_copy_result<I, O>>
    {
        return unique_copy_fn::impl(std::move(first), std::move(last),
                                    std::move(result), comp, proj);
    }

    template <typename Rng, typename O, typename Comp = equal_to<>,
              typename Proj = identity>
    constexpr auto
    operator()(Rng&& rng, O result, Comp comp = Comp{}, Proj proj = Proj{}) const
    -> std::enable_if_t<
            InputRange<Rng> &&
            WeaklyIncrementable<O> &&
            IndirectRelation<Comp, projected<iterator_t<Rng>, Proj>> &&
            IndirectlyCopyable<iterator_t<Rng>, O> &&
            decltype(constraint_helper<iterator_t<Rng>, O>(priority_tag<2>{}))::value,
       unique_copy_result<safe_iterator_t<Rng>, O>>
    {
        return unique_copy_fn::impl(nano::begin(rng), nano::end(rng),
                                    std::move(result), comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::unique_copy_fn, unique_copy)

NANO_END_NAMESPACE

#endif



// Algorithms which reuse the STL implementation
// nanorange/algorithm/stl/inplace_merge.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_INPLACE_MERGE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_INPLACE_MERGE_HPP_INCLUDED



#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct inplace_merge_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalIterator<I> &&
        Cpp98Iterator<I> &&
        Sortable<I, Comp>>
    operator()(I first, I middle, I last, Comp comp = Comp{}) const
    {
        std::inplace_merge(std::move(first), std::move(middle),
                           std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        BidirectionalRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, iterator_t<Rng> middle, Comp comp = Comp{}) const
    {
        std::inplace_merge(nano::begin(rng), std::move(middle),
                           nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::inplace_merge_fn, inplace_merge)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/stl/nth_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_NTH_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_NTH_ELEMENT_HPP_INCLUDED



#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct nth_element_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessIterator<I> &&
        detail::Cpp98Iterator<I> &&
        Sortable<I, Comp>>
    operator()(I first, I nth, I last, Comp comp = Comp{}) const
    {
        std::nth_element(std::move(first), std::move(nth),
                         std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
        RandomAccessRange<Rng> &&
        CommonRange<Rng> &&
        detail::Cpp98Iterator<iterator_t<Rng>> &&
        Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, iterator_t<Rng> nth, Comp comp = Comp{}) const
    {
        std::nth_element(nano::begin(rng), std::move(nth),
                         nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::nth_element_fn, nth_element)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/stl/stable_partition.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_STABLE_PARTITION_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_STABLE_PARTITION_HPP_INCLUDED



#include <algorithm>

// TODO: Reimplement

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_partition_fn {
    template <typename I, typename Pred>
    std::enable_if_t<
        BidirectionalIterator<I> &&
        Cpp98Iterator<I> &&
        IndirectUnaryPredicate<Pred, I>, I>
    operator()(I first, I last, Pred pred) const
    {
        return std::stable_partition(std::move(first), std::move(last),
                                     std::ref(pred));
    }

    template <typename Rng, typename Pred>
    std::enable_if_t<
        BidirectionalRange<Rng> &&
        CommonRange<Rng> &&
        Cpp98Iterator<iterator_t<Rng>> &&
         IndirectUnaryPredicate<Pred, iterator_t<Rng>>,
    safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred) const
    {
        return std::stable_partition(nano::begin(rng), nano::end(rng),
                                     std::ref(pred));
    }
};

}

NANO_INLINE_VAR(detail::stable_partition_fn, stable_partition)

NANO_END_NAMESPACE

#endif

// nanorange/algorithm/stl/stable_sort.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_STL_STABLE_SORT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_STL_STABLE_SORT_HPP_INCLUDED



#include <algorithm>

// TODO: Implement

NANO_BEGIN_NAMESPACE

namespace detail {

struct stable_sort_fn {
    template <typename I, typename Comp = less<>>
    std::enable_if_t<
            RandomAccessIterator<I> &&
            detail::Cpp98Iterator<I> &&
    Sortable<I, Comp>>
    operator()(I first, I last, Comp comp = Comp{}) const
    {
        std::stable_sort(std::move(first), std::move(last), std::ref(comp));
    }

    template <typename Rng, typename Comp = less<>>
    std::enable_if_t<
            RandomAccessRange<Rng> &&
            CommonRange<Rng> &&
    detail::Cpp98Iterator<iterator_t<Rng>> &&
    Sortable<iterator_t<Rng>, Comp>>
    operator()(Rng&& rng, Comp comp = Comp{}) const
    {
        std::stable_sort(nano::begin(rng), nano::end(rng), std::ref(comp));
    }
};

}

NANO_INLINE_VAR(detail::stable_sort_fn, stable_sort)

NANO_END_NAMESPACE

#endif


#endif



// nanorange/iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_HPP_INCLUDED








// nanorange/iterator/back_insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_BACK_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_BACK_INSERT_ITERATOR_HPP_INCLUDED



#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct back_insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr back_insert_iterator() = default;

    explicit back_insert_iterator(Container& x) : cont_(std::addressof(x)) {}

    back_insert_iterator& operator=(const iter_value_t<Container>& value)
    {
        cont_->push_back(value);
        return *this;
    }

    back_insert_iterator& operator=(iter_value_t<Container>&& value)
    {
        cont_->push_back(std::move(value));
        return *this;
    }

    back_insert_iterator& operator*() { return *this; }
    back_insert_iterator& operator++() { return *this; }
    back_insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
};

template <typename Container>
back_insert_iterator<Container> back_inserter(Container& x)
{
    return back_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Cont>
struct iterator_traits<::nano::back_insert_iterator<Cont>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = std::output_iterator_tag;
};

} // namespace std

#endif



// nanorange/iterator/counted_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED







NANO_BEGIN_NAMESPACE

namespace counted_iterator_ {

template <typename I>
class counted_iterator {
    static_assert(Iterator<I>, "");

    template <typename I2>
    friend class counted_iterator;

public:
    using iterator = I;
    using difference_type = iter_difference_t<I>;

    constexpr counted_iterator() = default;

    constexpr counted_iterator(I x, iter_difference_t<I> n)
        : current_(x), cnt_(n)
    {}

    template <typename I2, std::enable_if_t<ConvertibleTo<I2, I>, int> = 0>
    constexpr counted_iterator(const counted_iterator<I2>& i)
        : current_(i.current_), cnt_(i.cnt_)
    {}

    template <typename I2>
    constexpr auto operator=(const counted_iterator<I2>& i)
        -> std::enable_if_t<ConvertibleTo<I2, I>, counted_iterator&>
    {
        current_ = i.current_;
        cnt_ = i.cnt_;
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr iter_difference_t<I> count() const { return cnt_; }

    constexpr decltype(auto) operator*() { return *current_; }

    template <typename II = I,
              std::enable_if_t<detail::Dereferenceable<const II>, int> = 0>
    constexpr decltype(auto) operator*() const
    {
        return *current_;
    }

    constexpr counted_iterator& operator++()
    {
        ++current_;
        --cnt_;
        return *this;
    }

    template <typename II = I, std::enable_if_t<!ForwardIterator<II>, int> = 0>
    decltype(auto) operator++(int)
    {
        --cnt_;
        try {
            return current_++;
        } catch (...) {
            ++cnt_;
            throw;
        }
    }

    template <typename II = I>
    constexpr auto
    operator++(int) -> std::enable_if_t<ForwardIterator<II>, counted_iterator>
    {
        auto tmp = *this;
        ++*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator--()
        -> std::enable_if_t<BidirectionalIterator<II>, counted_iterator&>
    {
        --current_;
        ++cnt_;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator--(int)
        -> std::enable_if_t<BidirectionalIterator<II>, counted_iterator>
    {
        auto tmp = *this;
        --*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator+(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator>
    {
        return counted_iterator(current_ + n, cnt_ - n);
    }

    template <typename II = I>
    constexpr auto operator+=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator&>
    {
        current_ += n;
        cnt_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator-(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator>
    {
        return counted_iterator(current_ - n, cnt_ + n);
    }

    template <typename II = I>
    constexpr auto operator-=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator&>
    {
        current_ -= n;
        cnt_ += n;
        return *this;
    }

    template <typename II = I,
              std::enable_if_t<RandomAccessIterator<II>, int> = 0>
    constexpr decltype(auto) operator[](difference_type n) const
    {
        return current_[n];
    }

#ifndef _MSC_VER
    // FIXME MSVC: If this is a template, MSVC can't find it via ADL for some reason
    // Making it a non-template doesn't lose much other than the InputIterator guard
    template <typename II = I, std::enable_if_t<InputIterator<II>, int> = 0>
#endif
    friend constexpr iter_rvalue_reference_t<I>
    iter_move(const counted_iterator& i) noexcept(
        noexcept(ranges::iter_move(i.current_)))
    {
        return ranges::iter_move(i.current_);
    }

    template <typename I2>
    friend constexpr auto iter_swap(
        const counted_iterator<I>& x,
        const counted_iterator<I2>&
            y) noexcept(noexcept(ranges::iter_swap(x.current_, y.current_)))
        -> std::enable_if_t<IndirectlySwappable<I2, I>>
    {
        ranges::iter_swap(x.current_, y.current_);
    }

private:
    I current_{};
    iter_difference_t<I> cnt_{0};
};

template <typename I1, typename I2>
constexpr auto operator==(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return x.count() == y.count();
}

template <typename I>
constexpr bool operator==(const counted_iterator<I>& x, default_sentinel)
{
    return x.count() == 0;
}

template <typename I>
constexpr bool operator==(default_sentinel, const counted_iterator<I>& x)
{
    return x.count() == 0;
}

template <typename I1, typename I2>
constexpr auto operator!=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(x == y);
}

template <typename I>
constexpr bool operator!=(const counted_iterator<I>& x, default_sentinel y)
{
    return !(x == y);
}

template <typename I>
constexpr bool operator!=(default_sentinel x, const counted_iterator<I>& y)
{
    return !(x == y);
}

template <typename I1, typename I2>
constexpr auto operator<(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return y.count() < x.count();
}

template <typename I1, typename I2>
constexpr auto operator<=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(y < x);
}

template <typename I1, typename I2>
constexpr auto operator>(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return y < x;
}

template <typename I1, typename I2>
constexpr auto operator>=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(x < y);
}

template <typename I1, typename I2>
constexpr auto operator-(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, iter_difference_t<I2>>
{
    return y.count() - x.count();
}

template <typename I>
constexpr iter_difference_t<I> operator-(const counted_iterator<I>& x,
                                         default_sentinel)
{
    return -x.count();
}

template <typename I>
constexpr iter_difference_t<I> operator-(default_sentinel,
                                         const counted_iterator<I>& y)
{
    return y.count();
}

template <typename I>
constexpr auto operator+(iter_difference_t<I> n, const counted_iterator<I>& x)
    -> std::enable_if_t<RandomAccessIterator<I>, counted_iterator<I>>
{
    return x + n;
}

}

using counted_iterator_::counted_iterator;

namespace detail {

template <typename I, typename = void>
struct counted_iterator_readable_traits_helper {
};

template <typename I>
struct counted_iterator_readable_traits_helper<I, std::enable_if_t<Readable<I>>> {
    using value_type = iter_value_t<I>;
};

template <typename I, typename = void>
struct counted_iterator_category_helper {
};

template <typename I>
struct counted_iterator_category_helper<I, std::enable_if_t<InputIterator<I>>> {
    using type = iterator_category_t<I>;
};

} // namespace detail

template <typename I>
struct readable_traits<counted_iterator<I>>
        : detail::counted_iterator_readable_traits_helper<I> {
};

template <typename I>
struct iterator_category<counted_iterator<I>>
        : detail::counted_iterator_category_helper<I> {
};

template <typename I>
constexpr auto make_counted_iterator(I i, iter_difference_t<I> n)
    -> std::enable_if_t<Iterator<I>, counted_iterator<I>>
{
    return counted_iterator<I>(std::move(i), n);
}

NANO_END_NAMESPACE

#endif


// nanorange/iterator/front_insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_FRONT_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_FRONT_INSERT_ITERATOR_HPP_INCLUDED



#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct front_insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr front_insert_iterator() = default;

    explicit front_insert_iterator(Container& x) : cont_(std::addressof(x)) {}

    front_insert_iterator& operator=(const iter_value_t<Container>& value)
    {
        cont_->push_front(value);
        return *this;
    }

    front_insert_iterator& operator=(iter_value_t<Container>&& value)
    {
        cont_->front_back(std::move(value));
        return *this;
    }

    front_insert_iterator& operator*() { return *this; }
    front_insert_iterator& operator++() { return *this; }
    front_insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
};

template <typename Container>
front_insert_iterator<Container> front_inserter(Container& x)
{
    return front_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Cont>
struct iterator_traits<::nano::front_insert_iterator<Cont>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif

// nanorange/iterator/insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_INSERT_ITERATOR_HPP_INCLUDED



#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr insert_iterator() = default;

    explicit insert_iterator(Container& x, iterator_t<Container> i)
        : cont_(std::addressof(x)), it_(i)
    {}

    insert_iterator& operator=(const iter_value_t<Container>& value)
    {
        cont_->insert(it_, value);
        ++it_;
        return *this;
    }

    insert_iterator& operator=(iter_value_t<Container>&& value)
    {
        cont_->push_back(it_, std::move(value));
        ++it_;
        return *this;
    }

    insert_iterator& operator*() { return *this; }
    insert_iterator& operator++() { return *this; }
    insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
    iterator_t<container_type> it_{};
};

template <typename Container>
insert_iterator<Container> inserter(Container& x)
{
    return back_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Container>
struct iterator_traits<::nano::insert_iterator<Container>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif
// nanorange/iterator/istream_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_ISTREAM_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_ISTREAM_ITERATOR_HPP_INCLUDED




#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>,
          typename Distance = std::ptrdiff_t>
class istream_iterator {
public:
    using iterator_category = input_iterator_tag;
    using difference_type = Distance;
    using value_type = T;
    using reference = const T&;
    using pointer = const T*;
    using char_type = CharT;
    using traits_type = Traits;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istream_iterator() = default;

    constexpr istream_iterator(default_sentinel) {}

    istream_iterator(istream_type& s)
        : in_stream_(std::addressof(s))
    {
        s >> value_;
    }

    istream_iterator(const istream_iterator& x) = default;

    ~istream_iterator() = default;

    const T& operator*() const { return value_; }

    const T* operator->() const { return std::addressof(value_); }

    istream_iterator& operator++()
    {
        *in_stream_ >> value_;
        if (in_stream_->fail()) {
            in_stream_ = nullptr;
        }
        return *this;
    }

    istream_iterator operator++(int)
    {
        istream_iterator tmp = *this;
        this->operator++();
        return tmp;
    }

    friend bool operator==(const istream_iterator& x, const istream_iterator& y)
    {
        return x.in_stream_ == y.in_stream_;
    }

    friend bool operator==(default_sentinel, const istream_iterator y)
    {
        return nullptr == y.in_stream_;
    }

    friend bool operator==(const istream_iterator& x, default_sentinel)
    {
        return x.in_stream_ == nullptr;
    }

    friend bool operator!=(const istream_iterator& x, const istream_iterator& y)
    {
        return !(x == y);
    }

    friend bool operator!=(default_sentinel x, const istream_iterator y)
    {
        return !(x == y);
    }

    friend bool operator!=(const istream_iterator& x, default_sentinel y)
    {
        return !(x == y);
    }

private:
    istream_type* in_stream_ = nullptr;
    T value_{};
};

NANO_END_NAMESPACE

#endif

// nanorange/iterator/istreambuf_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_ISTREAMBUF_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_ISTREAMBUF_ITERATOR_HPP_INCLUDED




#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename CharT, typename Traits = std::char_traits<CharT>>
class istreambuf_iterator {
    class proxy {
        friend class istreambuf_iterator;
        CharT keep_;
        std::basic_streambuf<CharT, Traits>* sbuf_;

        proxy(CharT c, std::basic_streambuf<CharT, Traits>* sbuf)
            : keep_(c), sbuf_(sbuf)
        {}
    public:
        CharT operator*() const { return keep_; }
    };

public:
    using iterator_category = input_iterator_tag;
    using value_type = CharT;
    using difference_type = typename Traits::off_type;
    using reference = CharT;
    using pointer = CharT*;
    using char_type = CharT;
    using traits_type = Traits;
    using int_type = typename Traits::int_type;
    using streambuf_type = std::basic_streambuf<CharT, Traits>;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istreambuf_iterator() noexcept = default;

    constexpr istreambuf_iterator(default_sentinel) noexcept {}

    istreambuf_iterator(const istreambuf_iterator&) noexcept = default;

    ~istreambuf_iterator() = default;

    istreambuf_iterator(istream_type& s) noexcept
        : sbuf_(s.rdbuf())
    {}

    istreambuf_iterator(streambuf_type* s) noexcept
        : sbuf_(s)
    {}

    istreambuf_iterator(const proxy& p) noexcept
        : sbuf_(p.sbuf_)
    {}

    char_type operator*() const { return Traits::to_char_type(sbuf_->sgetc()); }

    istreambuf_iterator& operator++()
    {
        sbuf_->sbumpc();
        return *this;
    }

    proxy operator++(int)
    {
        return proxy(Traits::to_char_type(sbuf_->sbumpc()), sbuf_);
    }

    bool equal(const istreambuf_iterator& b) const
    {
        return  is_eof() == b.is_eof();
    }

private:
    bool is_eof() const
    {
        if (sbuf_ && sbuf_->sgetc() == Traits::eof()) {
            sbuf_ = nullptr;
            return true;
        }

        return sbuf_ == nullptr;
    }

    mutable streambuf_type* sbuf_ = nullptr;
};

template <typename CharT, typename Traits>
bool operator==(const istreambuf_iterator<CharT, Traits>& a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return a.equal(b);
}

template <typename CharT, typename Traits>
bool operator==(default_sentinel,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return istreambuf_iterator<CharT, Traits>{}.equal(b);
}

template <typename CharT, typename Traits>
bool operator==(const istreambuf_iterator<CharT, Traits>& a,
                default_sentinel)
{
    return a.equal(istreambuf_iterator<CharT, Traits>{});
}

template <typename CharT, typename Traits>
bool operator!=(const istreambuf_iterator<CharT, Traits>& a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return !(a == b);
}

template <typename CharT, typename Traits>
bool operator!=(default_sentinel a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return !(a == b);
}

template <typename CharT, typename Traits>
bool operator!=(const istreambuf_iterator<CharT, Traits>& a,
                default_sentinel b)
{
    return !(a == b);
}

NANO_END_NAMESPACE

#endif

// nanorange/iterator/move_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_MOVE_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_MOVE_ITERATOR_HPP_INCLUDED




NANO_BEGIN_NAMESPACE

namespace move_iterator_ {

template <typename I>
class move_iterator {

    static_assert(
        InputIterator<I>,
        "Template argument to move_iterator must model InputIterator");

    template <typename I2>
    friend class move_iterator;

public:
    using iterator_type = I;
    using difference_type = iter_difference_t<I>;
    using value_type = iter_value_t<I>;
    using iterator_category = input_iterator_tag;
    using reference = iter_rvalue_reference_t<I>;

    constexpr move_iterator() = default;

    explicit constexpr move_iterator(I i) : current_(std::move(i)) {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, I>, int> = 0>
    constexpr move_iterator(const move_iterator<U>& i) : current_(i.current_)
    {}

    template <typename U>
    constexpr std::enable_if_t<ConvertibleTo<U, I>, move_iterator&>
    operator=(const move_iterator<U>& i)
    {
        current_ = i.current_;
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr reference operator*() const { return iter_move(current_); }

    constexpr move_iterator& operator++()
    {
        ++current_;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator++(int) -> std::enable_if_t<!ForwardIterator<II>>
    {
        ++current_;
    }

    template <typename II = I>
    constexpr auto operator++(int)
        -> std::enable_if_t<ForwardIterator<II>, move_iterator>
    {
        move_iterator tmp = *this;
        ++current_;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator--()
        -> std::enable_if_t<BidirectionalIterator<II>, move_iterator&>
    {
        --current_;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator--(int)
        -> std::enable_if_t<BidirectionalIterator<II>, move_iterator>
    {
        move_iterator tmp = *this;
        --current_;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator+(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, move_iterator>
    {
        return move_iterator(current_ + n);
    }

    template <typename II = I>
    constexpr auto operator+=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, move_iterator&>
    {
        current_ += n;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator-(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, move_iterator>
    {
        return move_iterator(current_ - n);
    }

    template <typename II = I>
    constexpr auto operator-=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, move_iterator&>
    {
        current_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator[](difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, reference>
    //   -> decltype(auto)
    {
        return iter_move(current_ + n);
    }

    friend constexpr iter_rvalue_reference_t<I>
    iter_move(const move_iterator& i) noexcept(
        noexcept(ranges::iter_move(i.current_)))
    {
        return ranges::iter_move(i.current_);
    }

    template <typename I2>
    friend constexpr auto
    iter_swap(const move_iterator& x, const move_iterator<I2>& y) noexcept(
        noexcept(ranges::iter_swap(x.current_, y.current_)))
        -> std::enable_if_t<IndirectlySwappable<I2, I>>
    {
        ranges::iter_swap(x.current_, y.current_);
    }

private:
    I current_{};
};

template <typename I1, typename I2>
constexpr auto operator==(const move_iterator<I1>& x,
                          const move_iterator<I2>& y)
    -> std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
{
    return x.base() == y.base();
}

template <typename I1, typename I2>
constexpr auto operator!=(const move_iterator<I1>& x,
                          const move_iterator<I2>& y)
    -> std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
{
    return !(x == y);
}

template <typename I1, typename I2>
constexpr auto operator<(const move_iterator<I1>& x, const move_iterator<I2>& y)
    -> std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
{
    return x.base() < y.base();
}

template <typename I1, typename I2>
constexpr auto operator<=(const move_iterator<I1>& x,
                          const move_iterator<I2>& y)
    -> std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
{
    return !(y < x);
}

template <typename I1, typename I2>
constexpr auto operator>(const move_iterator<I1>& x, const move_iterator<I2>& y)
    -> std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
{
    return y < x;
}

template <typename I1, typename I2>
constexpr auto operator>=(const move_iterator<I1>& x,
                          const move_iterator<I2>& y)
    -> std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
{
    return !(x < y);
}

template <typename I1, typename I2>
constexpr auto operator-(const move_iterator<I1>& x, const move_iterator<I2>& y)
    -> std::enable_if_t<SizedSentinel<I1, I2>, iter_difference_t<I2>>
{
    return x.base() - y.base();
}

template <typename I>
constexpr auto operator+(iter_difference_t<I> n, const move_iterator<I>& x)
    -> std::enable_if_t<RandomAccessIterator<I>, move_iterator<I>>
{
    return x + n;
}

} // namespace move_iterator_

using move_iterator_::move_iterator;

template <typename I>
constexpr auto make_move_iterator(I i)
    -> std::enable_if_t<InputIterator<I>, move_iterator<I>>
{
    return move_iterator<I>(std::move(i));
}

template <typename S>
class move_sentinel {
    static_assert(Semiregular<S>,
                  "Template argument to move_sentinel must model Semiregular");

public:
    constexpr move_sentinel() = default;

    constexpr explicit move_sentinel(S s) : last_(std::move(s)) {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, S>, int> = 0>
    constexpr move_sentinel(const move_sentinel<U>& s) : last_(s.base())
    {}

    template <typename U>
    constexpr auto operator=(const move_sentinel<U>& s)
        -> std::enable_if_t<ConvertibleTo<U, S>, move_sentinel&>
    {
        last_ = s.base();
        return *this;
    }

    constexpr S base() const { return last_; }

private:
    S last_{};
};

template <typename I, typename S>
constexpr auto operator==(const move_iterator<I>& i, const move_sentinel<S>& s)
    -> std::enable_if_t<Sentinel<S, I>, bool>
{
    return i.base() == s.base();
}

template <typename I, typename S>
constexpr auto operator==(const move_sentinel<S>& s, const move_iterator<I>& i)
    -> std::enable_if_t<Sentinel<S, I>, bool>
{
    return i.base() == s.base();
}

template <typename I, typename S>
constexpr auto operator!=(const move_iterator<I>& i, const move_sentinel<S>& s)
    -> std::enable_if_t<Sentinel<S, I>, bool>
{
    return !(i == s);
}

template <typename I, typename S>
constexpr auto operator!=(const move_sentinel<S>& s, const move_iterator<I>& i)
    -> std::enable_if_t<Sentinel<S, I>, bool>
{
    return !(i == s);
}

template <typename I, typename S>
constexpr auto operator-(const move_sentinel<S>& s, const move_iterator<I>& i)
    -> std::enable_if_t<SizedSentinel<S, I>, iter_difference_t<I>>
{
    return s.base() - i.base();
}

template <typename I, typename S>
constexpr auto operator-(const move_iterator<I>& i, const move_sentinel<S>& s)
    -> std::enable_if_t<SizedSentinel<S, I>, iter_difference_t<I>>
{
    return i.base() - s.base();
}

template <typename S>
constexpr auto make_move_sentinel(S s)
    -> std::enable_if_t<Semiregular<S>, move_sentinel<S>>
{
    return move_sentinel<S>(std::move(s));
}

NANO_END_NAMESPACE

namespace std {

template <typename I>
struct iterator_traits<::nano::move_iterator_::move_iterator<I>> {
    using value_type =
        typename ::nano::move_iterator_::move_iterator<I>::value_type;
    using reference =
        typename ::nano::move_iterator_::move_iterator<I>::reference;
    using pointer = value_type*;
    using difference_type =
        typename ::nano::move_iterator_::move_iterator<I>::difference_type;
    using iterator_category =
        typename ::nano::move_iterator_::move_iterator<I>::iterator_category;
};

} // namespace std

#endif


// nanorange/iterator/ostream_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OSTREAM_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_OSTREAM_ITERATOR_HPP_INCLUDED



#include <string> // for char_traits

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>>
struct ostream_iterator
{
    using char_type = CharT;
    using traits_type = Traits;
    using ostream_type = std::basic_ostream<CharT, Traits>;
    using difference_type = std::ptrdiff_t;

    constexpr ostream_iterator() noexcept = default;

    ostream_iterator(ostream_type& os, const CharT* delim = nullptr) noexcept
            : os_(std::addressof(os)), delim_(delim)
    {}

    ostream_iterator& operator=(const T& value)
    {
        *os_ << value;
        if (delim_) {
            *os_ << delim_;
        }
        return *this;
    }

    ostream_iterator& operator*() { return *this; }
    ostream_iterator& operator++() { return *this; }
    ostream_iterator& operator++(int) { return *this; }

private:
    ostream_type* os_ = nullptr;
    const char_type* delim_ = nullptr;
};

NANO_END_NAMESPACE

namespace std {

template <typename T, typename C, typename Tr>
struct iterator_traits<::nano::ranges::ostream_iterator<T, C, Tr>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = std::output_iterator_tag;
};

}

#endif
// nanorange/iterator/ostreambuf_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OSTREAMBUF_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_OSTREAMBUF_ITERATOR_HPP_INCLUDED



#include <iosfwd> // for basic_streambuf
#include <string> // for char_traits

NANO_BEGIN_NAMESPACE

template <typename CharT, typename Traits = std::char_traits<CharT>>
struct ostreambuf_iterator {

    using char_type = CharT;
    using traits = Traits;
    using difference_type = std::ptrdiff_t;
    using streambuf_type = std::basic_streambuf<CharT, Traits>;
    using ostream_type = std::basic_ostream<CharT, Traits>;

    constexpr ostreambuf_iterator() = default;

    ostreambuf_iterator(ostream_type& s) noexcept : sbuf_(s.rdbuf()) {}

    ostreambuf_iterator(streambuf_type* s) noexcept : sbuf_(s) {}

    ostreambuf_iterator& operator=(char_type c)
    {
        if (!failed()) {
            failed_ = (sbuf_->sputc(c) == traits::eof());
        }
        return *this;
    }

    ostreambuf_iterator& operator*() { return *this; }
    ostreambuf_iterator& operator++() { return *this; }
    ostreambuf_iterator& operator++(int) { return *this; }

    bool failed() const noexcept { return failed_; }

private:
    streambuf_type* sbuf_ = nullptr;
    bool failed_ = false;
};

NANO_END_NAMESPACE

namespace std {

template <typename C, typename T>
struct iterator_traits<::nano::ranges::ostreambuf_iterator<C, T>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif


// nanorange/iterator/unreachable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_UNREACHABLE_HPP_INCLUDED
#define NANORANGE_ITERATOR_UNREACHABLE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

// [range.unreachable.sentinels]

class unreachable {
    template<typename I>
    friend constexpr std::enable_if_t<WeaklyIncrementable<I>, bool>
    operator==(const I &, unreachable) noexcept
    {
        return false;
    }

    template<typename I>
    friend constexpr std::enable_if_t<WeaklyIncrementable<I>, bool>
    operator==(unreachable, const I &) noexcept
    {
        return false;
    }

    template<typename I>
    friend constexpr std::enable_if_t<WeaklyIncrementable<I>, bool>
    operator!=(const I &, unreachable) noexcept
    {
        return true;
    }

    template<typename I>
    friend constexpr std::enable_if_t<WeaklyIncrementable<I>, bool>
    operator!=(unreachable, const I &) noexcept
    {
        return true;
    }
};

NANO_END_NAMESPACE

#endif


#endif

// nanorange/memory.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_HPP_INCLUDED

// nanorange/memory/destroy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_DESTROY_HPP_INCLUDED
#define NANORANGE_MEMORY_DESTROY_HPP_INCLUDED

// nanorange/detail/memory/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

template <typename>
auto NoThrowInputIterator_fn(long) -> std::false_type;

template <typename I>
auto NoThrowInputIterator_fn(int) -> std::enable_if_t<
    InputIterator<I> &&
    std::is_lvalue_reference<iter_reference_t<I>>::value &&
    Same<remove_cvref_t<iter_reference_t<I>>, iter_value_t<I>>,
    std::true_type>;

template <typename I>
NANO_CONCEPT NoThrowInputIterator =
    decltype(NoThrowInputIterator_fn<I>(0))::value;

template <typename S, typename I>
NANO_CONCEPT NoThrowSentinel = Sentinel<S, I>;

template <typename>
auto NoThrowInputRange_fn(long) -> std::false_type;

template <typename Rng>
auto NoThrowInputRange_fn(int) -> std::enable_if_t<
    Range<Rng> &&
    NoThrowInputIterator<iterator_t<Rng>> &&
    NoThrowSentinel<sentinel_t<Rng>, iterator_t<Rng>>,
    std::true_type>;

template <typename Rng>
NANO_CONCEPT NoThrowInputRange =
    decltype(NoThrowInputRange_fn<Rng>(0))::value;

template <typename I>
NANO_CONCEPT NoThrowForwardIterator =
    NoThrowInputIterator<I> &&
    ForwardIterator<I> &&
    NoThrowSentinel<I, I>;

template <typename>
auto NoThrowForwardRange_fn(long) -> std::false_type;

template <typename Rng>
auto NoThrowForwardRange_fn(int) -> std::enable_if_t<
    NoThrowInputRange<Rng> &&
    NoThrowForwardIterator<iterator_t<Rng>>,
    std::true_type>;

template <typename Rng>
NANO_CONCEPT NoThrowForwardRange =
    decltype(NoThrowForwardRange_fn<Rng>(0))::value;

}

NANO_END_NAMESPACE

#endif



NANO_BEGIN_NAMESPACE

template <typename T>
std::enable_if_t<Destructible<T>>
destroy_at(T* location) noexcept
{
    location->~T();
}

namespace detail {

struct destroy_fn {
private:
    template <typename I, typename S>
    static I impl(I first, S last) noexcept
    {
        for (; first != last; ++first) {
            nano::destroy_at(std::addressof(*first));
        }
        return first;
    }

public:
    template <typename I, typename S>
    std::enable_if_t<
        NoThrowInputIterator<I> &&
        NoThrowSentinel<S, I> &&
        Destructible<iter_value_t<I>>, I>
    operator()(I first, S last) const noexcept
    {
        return destroy_fn::impl(std::move(first), std::move(last));
    }

    template <typename Rng>
    std::enable_if_t<
        NoThrowInputRange<Rng> &&
        Destructible<iter_value_t<iterator_t<Rng>>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const noexcept
    {
        return destroy_fn::impl(nano::begin(rng), nano::end(rng));
    }
};

}

NANO_INLINE_VAR(detail::destroy_fn, destroy)

namespace detail {

struct destroy_n_fn {
    template <typename I>
    std::enable_if_t<
        NoThrowInputIterator<I> &&
        Destructible<iter_value_t<I>>, I>
    operator()(I first, iter_difference_t<I> n) const noexcept
    {
        return nano::destroy(make_counted_iterator(std::move(first), n),
                             default_sentinel{}).base();
    }


};

}

NANO_INLINE_VAR(detail::destroy_n_fn, destroy_n)

NANO_END_NAMESPACE

#endif

// nanorange/memory/uninitialized_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_COPY_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_COPY_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
struct uninitialized_copy_result {
    I in;
    O out;
};

namespace detail {

struct uninitialized_copy_fn {
private:
    friend struct uninitialized_copy_n_fn;

    template <typename I, typename S, typename O, typename S2>
    static uninitialized_copy_result<I, O>
    impl4(I ifirst, S ilast, O ofirst, S2 olast)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast && oit != olast; ++ifirst, (void) ++oit) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *oit))))
                        std::remove_reference_t<iter_reference_t<O>>(*ifirst);
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

    template <typename I, typename S, typename O>
    static uninitialized_copy_result<I, O>
    impl3(I ifirst, S ilast, O ofirst)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast; ++ifirst, (void) ++oit) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *oit))))
                        std::remove_reference_t<iter_reference_t<O>>(*ifirst);
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

public:
    // Four-legged
    template <typename I, typename S, typename O, typename S2>
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        NoThrowForwardIterator<O> &&
        NoThrowSentinel<S2, O> &&
        Constructible<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst, S2 olast) const
    {
        return uninitialized_copy_fn::impl4(
                std::move(ifirst), std::move(ilast),
                std::move(ofirst), std::move(olast));
    }

    // Two ranges
    template <typename IRng, typename ORng>
    std::enable_if_t<
        InputRange<IRng> &&
        NoThrowForwardRange<ORng> &&
        Constructible<iter_value_t<iterator_t<ORng>>, iter_reference_t<iterator_t<IRng>>>,
        uninitialized_copy_result<safe_iterator_t<IRng>, safe_iterator_t<ORng>>>
    operator()(IRng&& irng, ORng&& orng) const
    {
        return uninitialized_copy_fn::impl4(
                nano::begin(irng), nano::end(irng),
                nano::begin(orng), nano::end(orng));
    }

    // Three-legged
    template <typename I, typename S, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        NoThrowForwardIterator<O> &&
        Constructible<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst) const
    {
        return uninitialized_copy_fn::impl3(std::move(ifirst), std::move(ilast),
                                            std::move(ofirst));
    }

    // Range and a half
    template <typename IRng, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputRange<IRng> &&
        NoThrowForwardIterator<std::decay_t<O>> &&
        !NoThrowForwardRange<O> &&
        Constructible<iter_value_t<std::decay_t<O>>, iter_reference_t<iterator_t<IRng>>>,
        uninitialized_copy_result<safe_iterator_t<IRng>, std::decay_t<O>>>
    operator()(IRng&& irng, O&& ofirst) const
    {
        return uninitialized_copy_fn::impl3(
                nano::begin(irng), nano::end(irng), std::forward<O>(ofirst));
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_copy_fn, uninitialized_copy)

template <typename I, typename O>
using uninitialized_copy_n_result = uninitialized_copy_result<I, O>;

namespace detail {

struct uninitialized_copy_n_fn {
    template <typename I, typename O, typename S>
    std::enable_if_t<
        InputIterator<I> &&
        NoThrowForwardIterator<O> &&
        NoThrowSentinel<S, O> &&
        Constructible<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst, S olast) const
    {
        auto t = uninitialized_copy_fn::impl4(
                    make_counted_iterator(std::move(ifirst), n),
                    default_sentinel{}, std::move(ofirst), std::move(olast));
        return {std::move(t).in.base(), std::move(t).out};
    }

    template <typename I, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputIterator<I> &&
        NoThrowForwardIterator<O> &&
        Constructible<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst) const
    {
        auto t = uninitialized_copy_fn::impl3(
                make_counted_iterator(std::move(ifirst), n),
                default_sentinel{}, std::move(ofirst));
        return {std::move(t).in.base(), std::move(t).out};
    }

};

}

NANO_INLINE_VAR(detail::uninitialized_copy_n_fn, uninitialized_copy_n)

NANO_END_NAMESPACE

#endif

// nanorange/memory/uninitialized_default_construct.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_DEFAULT_CONSTRUCT_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_DEFAULT_CONSTRUCT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct uninitialized_default_construct_fn {
private:
    template <typename I, typename S>
    static I impl(I first, S last)
    {
        I it = first;
        try {
            for (; it != last; ++it) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *it))))
                        std::remove_reference_t<iter_reference_t<I>>;
            }
            return it;
        } catch (...) {
            nano::destroy(first, ++it);
            throw;
        }
    }

public:
    template <typename I, typename S>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        NoThrowSentinel<S, I> &&
        DefaultConstructible<iter_value_t<I>>, I>
    operator()(I first, S last) const
    {
        return uninitialized_default_construct_fn::impl(
                std::move(first), std::move(last));
    }

    template <typename Rng>
    std::enable_if_t<
        NoThrowForwardRange<Rng> &&
        DefaultConstructible<iter_value_t<iterator_t<Rng>>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const
    {
        return uninitialized_default_construct_fn::impl(
                nano::begin(rng), nano::end(rng));
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_default_construct_fn,
                uninitialized_default_construct)

namespace detail {

struct uninitialized_default_construct_n_fn {
    template <typename I>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        DefaultConstructible<iter_value_t<I>>, I>
    operator()(I first, iter_difference_t<I> n) const
    {
        return nano::uninitialized_default_construct(
                    make_counted_iterator(std::move(first), n),
                    default_sentinel{}).base();
    }

};

}

NANO_INLINE_VAR(detail::uninitialized_default_construct_n_fn,
                uninitialized_default_construct_n)

NANO_END_NAMESPACE

#endif

// nanorange/memory/uninitialized_fill.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_FILL_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_FILL_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct uninitialized_fill_fn {
private:
    friend struct uninitialized_fill_n_fn;

    template <typename I, typename S, typename T>
    static I impl(I first, S last, const T& x)
    {
        I it = first;
        try {
            for (; it != last; ++it) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *it))))
                        std::remove_reference_t<iter_reference_t<I>>(x);
            }
            return it;
        } catch (...) {
            nano::destroy(first, ++it);
            throw;
        }
    }

public:
    template <typename I, typename S, typename T>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        NoThrowSentinel<S, I> &&
        Constructible<iter_value_t<I>, const T&>,
        I>
    operator()(I first, S last, const T& x) const
    {
        return uninitialized_fill_fn::impl(std::move(first), std::move(last), x);
    }

    template <typename Rng, typename T>
    std::enable_if_t<
        NoThrowForwardRange<Rng> &&
        Constructible<iter_value_t<iterator_t<Rng>>, const T&>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& x) const
    {
        return uninitialized_fill_fn::impl(nano::begin(rng), nano::end(rng), x);
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_fill_fn, uninitialized_fill)

namespace detail {

struct uninitialized_fill_n_fn {
    template <typename I, typename T>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        Constructible<iter_value_t<I>, const T&>,
        I>
    operator()(I first, iter_difference_t<I> n, const T& x) const
    {
        return uninitialized_fill_fn::impl(
                    make_counted_iterator(std::move(first), n),
                    default_sentinel{}, x).base();
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_fill_n_fn, uninitialized_fill_n)

NANO_END_NAMESPACE

#endif

// nanorange/memory/uninitialized_move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_MOVE_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_MOVE_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using uninitialized_move_result = uninitialized_copy_result<I, O>;

namespace detail {

struct uninitialized_move_fn {
private:
    friend struct uninitialized_move_n_fn;

    template <typename I, typename S, typename O, typename S2>
    static uninitialized_move_result<I, O>
    impl4(I ifirst, S ilast, O ofirst, S2 olast)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast && oit != olast; ++ifirst, (void) ++oit) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *oit))))
                        std::remove_reference_t<iter_reference_t<O>>(nano::iter_move(ifirst));
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

    template <typename I, typename S, typename O>
    static uninitialized_move_result<I, O>
    impl3(I ifirst, S ilast, O ofirst)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast; ++ifirst, (void) ++oit) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *oit))))
                        std::remove_reference_t<iter_reference_t<O>>(nano::iter_move(ifirst));
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

public:
    // Four-legged
    template <typename I, typename S, typename O, typename S2>
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        NoThrowForwardIterator<O> &&
        NoThrowSentinel<S2, O> &&
        Constructible<iter_value_t<O>, iter_rvalue_reference_t<I>>,
        uninitialized_move_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst, S2 olast) const
    {
        return uninitialized_move_fn::impl4(
                std::move(ifirst), std::move(ilast),
                std::move(ofirst), std::move(olast));
    }

    // Two ranges
    template <typename IRng, typename ORng>
    std::enable_if_t<
        InputRange<IRng> &&
        NoThrowForwardRange<ORng> &&
        Constructible<iter_value_t<iterator_t<ORng>>, iter_rvalue_reference_t<iterator_t<IRng>>>,
        uninitialized_move_result<safe_iterator_t<IRng>, safe_iterator_t<ORng>>>
    operator()(IRng&& irng, ORng&& orng) const
    {
        return uninitialized_move_fn::impl4(
                nano::begin(irng), nano::end(irng),
                nano::begin(orng), nano::end(orng));
    }

    // Three-legged
    template <typename I, typename S, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        NoThrowForwardIterator<O> &&
        Constructible<iter_value_t<O>, iter_rvalue_reference_t<I>>,
        uninitialized_move_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst) const
    {
        return uninitialized_move_fn::impl3(std::move(ifirst), std::move(ilast),
                                            std::move(ofirst));
    }

    // Range and a half
    template <typename IRng, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputRange<IRng> &&
        NoThrowForwardIterator<std::decay_t<O>> &&
        !NoThrowForwardRange<O> &&
        Constructible<iter_value_t<std::decay_t<O>>, iter_rvalue_reference_t<iterator_t<IRng>>>,
        uninitialized_move_result<safe_iterator_t<IRng>, std::decay_t<O>>>
    operator()(IRng&& irng, O&& ofirst) const
    {
        return uninitialized_move_fn::impl3(
                nano::begin(irng), nano::end(irng), std::forward<O>(ofirst));
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_move_fn, uninitialized_move)

template <typename I, typename O>
using uninitialized_move_n_result = uninitialized_copy_result<I, O>;

namespace detail {

struct uninitialized_move_n_fn {
    template <typename I, typename O, typename S>
    std::enable_if_t<
        InputIterator<I> &&
        NoThrowForwardIterator<O> &&
        NoThrowSentinel<S, O> &&
        Constructible<iter_value_t<O>, iter_rvalue_reference_t<I>>,
        uninitialized_move_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst, S olast) const
    {
        auto t = uninitialized_move_fn::impl4(
                    make_counted_iterator(std::move(ifirst), n),
                    default_sentinel{}, std::move(ofirst), std::move(olast));
        return {std::move(t).in.base(), std::move(t).out};
    }

    template <typename I, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        InputIterator<I> &&
        NoThrowForwardIterator<O> &&
        Constructible<iter_value_t<O>, iter_rvalue_reference_t<I>>,
        uninitialized_move_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst) const
    {
        auto t = uninitialized_move_fn::impl3(
                make_counted_iterator(std::move(ifirst), n),
                default_sentinel{}, std::move(ofirst));
        return {std::move(t).in.base(), std::move(t).out};
    }

};

}

NANO_INLINE_VAR(detail::uninitialized_move_n_fn, uninitialized_move_n)

NANO_END_NAMESPACE

#endif

// nanorange/memory/uninitialized_value_construct.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_VALUE_CONSTRUCT_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_VALUE_CONSTRUCT_HPP_INCLUDED



NANO_BEGIN_NAMESPACE

namespace detail {

struct uninitialized_value_construct_fn {
private:
    template <typename I, typename S>
    static I impl(I first, S last)
    {
        I it = first;
        try {
            for (; it != last; ++it) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *it))))
                        std::remove_reference_t<iter_reference_t<I>>();
            }
            return it;
        } catch (...) {
            nano::destroy(first, ++it);
            throw;
        }
    }

public:
    template <typename I, typename S>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        NoThrowSentinel<S, I> &&
        DefaultConstructible<iter_value_t<I>>, I>
    operator()(I first, S last) const
    {
        return uninitialized_value_construct_fn::impl(
                std::move(first), std::move(last));
    }

    template <typename Rng>
    std::enable_if_t<
        NoThrowForwardRange<Rng> &&
        DefaultConstructible<iter_value_t<iterator_t<Rng>>>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng) const
    {
        return uninitialized_value_construct_fn::impl(
                nano::begin(rng), nano::end(rng));
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_value_construct_fn,
                uninitialized_value_construct)

namespace detail {

struct uninitialized_value_construct_n_fn {
    template <typename I>
    std::enable_if_t<
        NoThrowForwardIterator<I> &&
        DefaultConstructible<iter_value_t<I>>, I>
    operator()(I first, iter_difference_t<I> n) const
    {
        return nano::uninitialized_value_construct(
                    make_counted_iterator(std::move(first), n),
                    default_sentinel{}).base();
    }

};

}

NANO_INLINE_VAR(detail::uninitialized_value_construct_n_fn,
                uninitialized_value_construct_n)

NANO_END_NAMESPACE

#endif


#endif




// nanorange/utility.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_UTILITY_HPP_INCLUDED
#define NANORANGE_UTILITY_HPP_INCLUDED



#endif


#endif // NANORANGE_HPP_INCLUDED
