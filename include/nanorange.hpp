// nanorange/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// This file implements proposed standard library concepts from P0898r2
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0898r2.pdf

#ifndef NANORANGE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_CONCEPTS_HPP_INCLUDED

#include <algorithm>
#include <functional>
#include <iterator>
#include <type_traits>
#include <utility>

// These are needed for the view-predicate definition, and
// we're not allowed to forward-declare standard library classes
#include <initializer_list>
#include <set>
#include <unordered_set>

namespace nanorange {

#define NANO_CONCEPT constexpr bool

// FIXME: Work out how to use the "poison pills" on MSVC
#ifdef _MSC_VER
#define NANO_MSVC_NO_POISON_PILLS
#endif

namespace detail {

template <typename T>
using lref_t = std::add_lvalue_reference_t<T>;

template <typename T>
using clref_t = std::add_lvalue_reference_t<std::add_const_t<T>>;

template <typename T>
using rref_t = std::add_rvalue_reference_t<T>;

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template<typename...>
using void_t = void;

template<typename... T>
void valid_expr(T &&...);

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

template <typename Void, template <class...> class AliasT, typename... Args>
constexpr bool exists_helper_v = false;

template <template <class...> class AliasT, typename... Args>
constexpr bool exists_helper_v<void_t<AliasT<Args...>>, AliasT, Args...> = true;

template <template <class...> class AliasT, typename... Args>
constexpr bool exists_v = exists_helper_v<void, AliasT, Args...>;

template <typename R, typename... Args, typename = decltype(&R::template requires_<Args...>)>
auto test_requires(R&) -> void;

template <typename R, typename... Args>
using test_requires_t = decltype(test_requires<R, Args...>(std::declval<R&>()));

template<typename R, typename... Args>
constexpr bool requires_ = exists_v<test_requires_t, R, Args...>;

template<bool Expr>
using requires_expr = std::enable_if_t<Expr, int>;

template <std::size_t I> struct priority_tag : priority_tag<I - 1> {};
template <> struct priority_tag<0> {};

}

// Note: this is supposed to go in type_traits, but we're putting it in here to avoid adding another header

template <typename T, typename U, template <class> class TQual, template <class> class UQual>
struct basic_common_reference {};

// FIXME: Bite the bullet and do this properly
namespace detail {

inline namespace common_reference_stuff {

template<typename From, typename To>
struct copy_cv {
    using type = To;
};

template<typename From, typename To>
struct copy_cv<const From, To> {
    using type = const To;
};

template<typename From, typename To>
struct copy_cv<volatile From, To> {
    using type = volatile To;
};

template<typename From, typename To>
struct copy_cv<const volatile From, To> {
    using type = const volatile To;
};

template<typename From, typename To>
using COPY_CV = typename copy_cv<From, To>::type;

template<typename X, typename Y>
using COND_RES = decltype(std::declval<bool>() ? std::declval<X(&)()>()() : std::declval<Y(&)()>()());

template <typename X, typename Y>
constexpr bool has_cond_res_v = exists_v<COND_RES, X, Y>;

template<typename A, typename B, typename = void>
struct COMMON_REF_S {};

template<typename A, typename B>
using COMMON_REF = typename COMMON_REF_S<A, B>::type;

template <typename A, typename B, typename C = test_t<COND_RES, COPY_CV<A, B>&, COPY_CV<B, A>&>>
struct lvalue_COMMON_REF_S
    : std::enable_if<std::is_reference<C>::value, C> {};

template<typename A, typename B>
struct COMMON_REF_S<A&, B&>
    : lvalue_COMMON_REF_S<A, B> {};

template <typename A, typename B>
struct COMMON_REF_S<A&&, B&&>
    : std::enable_if<
        std::is_convertible<A&&, std::remove_reference_t<COMMON_REF<A&, B&>>&&>::value &&
        std::is_convertible<B&&, std::remove_reference_t<COMMON_REF<A&, B&>>&&>::value,
        std::remove_reference_t<COMMON_REF<A&, B&>>&&> {};

template <typename A, typename B>
struct COMMON_REF_S<A&&, B&, void_t<COMMON_REF<const A&, B&>>>
    : std::enable_if<
        std::is_convertible<A&&, COMMON_REF<const A&, B&>>::value,
        COMMON_REF<const A&, B&>> {};

template <typename A, typename B>
struct COMMON_REF_S<A&, B&&>
    : COMMON_REF_S<B&&, A&> {};

}

template <typename T, typename U>
constexpr bool has_simple_common_reference_v = exists_v<COMMON_REF, T, U>;

template <typename T, typename U, typename = void>
struct binary_common_reference
    : std::common_type<T, U> {};

template <typename T, typename U>
struct binary_common_reference<T, U, std::enable_if_t<has_simple_common_reference_v<T, U>>>
    : COMMON_REF_S<T, U> {};

template <typename T, typename U>
struct binary_common_reference<T, U, std::enable_if_t<
    !has_simple_common_reference_v<T, U> &&
    has_cond_res_v<T, U>>> {
    using type = COND_RES<T, U>;
};

}

template <typename...>
struct common_reference;

template <>
struct common_reference<> {};

template <typename T>
struct common_reference<T> {
    using type = T;
};

template <typename T, typename U>
struct common_reference<T, U> :
    detail::binary_common_reference<T, U> {};

namespace detail {

template<typename Void, typename, typename, typename...>
struct multiple_common_reference {
};

template<typename T, typename U, typename... Rest>
struct multiple_common_reference<void_t<typename binary_common_reference<T, U>::type>, T, U, Rest...>
    : common_reference<typename binary_common_reference<T, U>::type, Rest...>
{
};

}

template <typename T1, typename T2, typename... Rest>
struct common_reference<T1, T2, Rest...>
    : detail::multiple_common_reference<void, T1, T2, Rest...> {};

template <typename... T>
using common_reference_t = typename common_reference<T...>::type;

namespace detail {

template <typename... T>
using checked_common_ref_t = test_t<common_reference_t, T...>;

}

using std::common_type;
using std::common_type_t;

namespace detail {

template <typename... T>
using checked_common_type_t = test_t<common_type_t, T...>;

}

// [concepts.lib.corelang.same]
template <typename T, typename U>
NANO_CONCEPT Same = std::is_same<T, U>::value;

// [concepts.lib.corelang.derived]

// FIXME: Spec doesn't use remove_reference_t here, not sure if it should
template<typename Derived, typename Base>
NANO_CONCEPT DerivedFrom =
    std::is_base_of<Base, Derived>::value &&
    std::is_convertible<const volatile std::remove_reference_t<Derived>*,
                        const volatile std::remove_reference_t<Base>*>::value;

// [concepts.lib.corelang.convertibleto]
namespace detail {

struct ConvertibleTo_req {
    template <typename From, typename To>
    auto requires_(From (&f)()) -> decltype(
        static_cast<To>(f())
    );
};

}

// [concepts.lib.corelang.convertibleto]
template <typename From, typename To>
NANO_CONCEPT ConvertibleTo =
    std::is_convertible<From, To>::value &&
    detail::requires_<detail::ConvertibleTo_req, From, To>;

// [concepts.lib.corelang.commonref]
template <typename T, typename U>
NANO_CONCEPT CommonReference =
    Same<detail::checked_common_ref_t<T, U>, detail::checked_common_ref_t<U, T>> &&
    ConvertibleTo<T, detail::checked_common_ref_t<T, U>> &&
    ConvertibleTo<U, detail::checked_common_ref_t<T, U>>;

// [concepts.lib.corelang.common]
template <typename T, typename U>
NANO_CONCEPT Common =
    Same<detail::checked_common_type_t<T, U>, detail::checked_common_type_t<U, T>> &&
    ConvertibleTo<T, detail::checked_common_type_t<T, U>> &&
    ConvertibleTo<U, detail::checked_common_type_t <T, U>> &&
    CommonReference<
        std::add_lvalue_reference_t<const T>,
        std::add_lvalue_reference_t<const U>> &&
    CommonReference<
        std::add_lvalue_reference_t<detail::checked_common_type_t<T, U>>,
        detail::checked_common_ref_t<
            std::add_lvalue_reference_t<const T>,
            std::add_lvalue_reference_t<const U>>>;

// [concepts.lib.corelang.integral]
template <typename T>
NANO_CONCEPT Integral = std::is_integral<T>::value;

// [concepts.lib.corelang.signedintegral]
template <typename T>
NANO_CONCEPT SignedIntegral = Integral<T> && std::is_signed<T>::value;

// [concepts.lib.corelang.unsignedintegral]
template <typename T>
NANO_CONCEPT UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

// [concepts.lib.corelang.assignable]

namespace detail {

struct Assignable_req {
    template<typename LHS, typename RHS>
    auto requires_(LHS lhs, RHS&& rhs) -> decltype(valid_expr(
        lhs = std::forward<RHS>(rhs),
        requires_expr<Same<decltype(lhs = std::forward<RHS>(rhs)), LHS>>{}
    ));
};

}

template <typename LHS, typename RHS>
NANO_CONCEPT Assignable =
    std::is_lvalue_reference<LHS>::value &&
    CommonReference<detail::clref_t<std::remove_reference_t<LHS>>, detail::clref_t<std::remove_reference_t<RHS>>> &&
    detail::requires_<detail::Assignable_req, LHS, RHS>;

// [concepts.lib.corelang.swappable]
namespace detail {

// Hack: we can't predeclare a constexpr variable template, so use a
// constexpr function to delay the defintion until we've defined swap()
template <typename T>
constexpr bool swappable_f();

template <typename T, typename U>
constexpr bool swappable_with_f();

}

template <typename T>
NANO_CONCEPT Swappable = detail::swappable_f<T>();

template <typename T, typename U>
NANO_CONCEPT SwappableWith = detail::swappable_with_f<T, U>();

// [concepts.lib.corelang.destructible]
template <typename T>
NANO_CONCEPT Destructible = std::is_nothrow_destructible<T>::value;

// [concepts.lib.corelang.constructible]
template <typename T, typename... Args>
NANO_CONCEPT Constructible = Destructible<T> && std::is_constructible<T, Args...>::value;

// [concepts.lib.corelang.defaultconstructible]
template <typename T>
NANO_CONCEPT DefaultConstructible = Constructible<T>;

// [concepts.lib.corelang.moveconstructible]
template <typename T>
NANO_CONCEPT MoveConstructible = Constructible<T, T> && ConvertibleTo<T, T>;

// concepts.lib.corelang.copyconstructible]
template <typename T>
NANO_CONCEPT CopyConstructible = MoveConstructible<T> &&
    Constructible<T, detail::lref_t<T>> && ConvertibleTo<detail::lref_t<T>, T> &&
    Constructible<T, detail::lref_t<T>> && ConvertibleTo<detail::clref_t<T>, T> &&
    Constructible<T, const T> && ConvertibleTo<const T, T>;

// N.B This is out of order, but never mind
template <typename T>
NANO_CONCEPT Movable = std::is_object<T>::value && MoveConstructible<T> && Assignable<detail::lref_t<T>, T> && Swappable<T>;

// [concepts.lib.compare.boolean]

namespace detail {

struct Boolean_req {
    template <typename B>
    auto requires_(const std::remove_reference_t<B>& b1,
                   const std::remove_reference_t<B>& b2, const bool a) -> decltype(valid_expr(
        requires_expr<ConvertibleTo<const std::remove_reference_t<B&>, bool>>{},
        !b1, requires_expr<ConvertibleTo<decltype(!b1), bool>>{},
        b1 && a, requires_expr<Same<decltype(b1 && a), bool>>{},
        b1 || a, requires_expr<Same<decltype(b1 || a), bool>>{},
        b1 && b2, requires_expr<Same<decltype(b1 && b2), bool>>{},
        a && b2, requires_expr<Same<decltype(a && b2), bool>>{},
        b1 || b2, requires_expr<Same<decltype(b1 || b2), bool>>{},
        a || b2, requires_expr<Same<decltype(a || b2), bool>>{},
        b1 == b2, requires_expr<ConvertibleTo<decltype(b1 == b2), bool>>{},
        b1 == a, requires_expr<ConvertibleTo<decltype(b1 == a), bool>>{},
        a == b2, requires_expr<ConvertibleTo<decltype(a == b2), bool>>{},
        b1 != b2, requires_expr<ConvertibleTo<decltype(b1 != b2), bool>>{},
        b1 != a, requires_expr<ConvertibleTo<decltype(b1 != a), bool>>{},
        a != b2, requires_expr<ConvertibleTo<decltype(a != b2), bool>>{}
    ));
};

}

template <typename B>
NANO_CONCEPT Boolean = Movable<detail::remove_cvref_t<B>> &&
    detail::requires_<detail::Boolean_req, B>;

// [concepts.lib.compare.equalitycomparable]

namespace detail {

struct WeaklyEqualityComparableWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u) -> decltype(valid_expr(
        t == u, requires_expr<Boolean<decltype(t == u)>>{},
        t != u, requires_expr<Boolean<decltype(t != u)>>{},
        u == t, requires_expr<Boolean<decltype(u == t)>>{},
        u != t, requires_expr<Boolean<decltype(u != t)>>{}
    ));
};

template <typename T, typename U>
NANO_CONCEPT WeaklyEqualityComparableWith =
    requires_<WeaklyEqualityComparableWith_req, T, U>;

}

template <typename T>
NANO_CONCEPT EqualityComparable = detail::WeaklyEqualityComparableWith<T, T>;

template <typename T, typename U>
NANO_CONCEPT EqualityComparableWith =
    EqualityComparable<T> && EqualityComparable<U> &&
    CommonReference<detail::clref_t<std::remove_reference_t<T>>,
                    detail::clref_t<std::remove_reference_t<U>>> &&
    EqualityComparable<detail::checked_common_ref_t<
                       detail::clref_t<std::remove_reference_t<T>>,
                       detail::clref_t<std::remove_reference_t<U>>>> &&
    detail::WeaklyEqualityComparableWith<T, U>;

// [concepts.lib.compare.stricttotallyordered]

namespace detail {

struct StrictTotallyOrdered_req {
    template <typename T>
    auto requires_(const std::remove_reference_t<T>& a,
                   const std::remove_reference_t<T>& b) -> decltype(valid_expr(
        a < b, requires_expr<Boolean<decltype(a < b)>>{},
        a > b, requires_expr<Boolean<decltype(a > b)>>{},
        a <= b, requires_expr<Boolean<decltype(a <= b)>>{},
        a >= b, requires_expr<Boolean<decltype(a >= b)>>{}
    ));
};

}

template <typename T>
NANO_CONCEPT StrictTotallyOrdered = EqualityComparable<T> &&
    detail::requires_<detail::StrictTotallyOrdered_req, T>;

namespace detail {

struct StrictTotallyOrderedWith_req {
    template <typename T, typename U>
    auto requires_(const std::remove_reference_t<T>& t,
                   const std::remove_reference_t<U>& u) -> decltype(valid_expr(
        t < u, requires_expr<Boolean<decltype(t < u)>>{},
        t > u, requires_expr<Boolean<decltype(t > u)>>{},
        t <= u, requires_expr<Boolean<decltype(t <= u)>>{},
        t >= u, requires_expr<Boolean<decltype(t >= u)>>{},
        u < t, requires_expr<Boolean<decltype(u < t)>>{},
        u > t, requires_expr<Boolean<decltype(u > t)>>{},
        u <= t, requires_expr<Boolean<decltype(u <= t)>>{},
        u >= t, requires_expr<Boolean<decltype(u >= t)>>{}
    ));
};

}

template <typename T, typename U>
NANO_CONCEPT StrictTotallyOrderedWith = StrictTotallyOrdered<T> && StrictTotallyOrdered<U> &&
    CommonReference<detail::clref_t<std::remove_reference_t<T>>, detail::clref_t<std::remove_reference_t<U>>> &&
    StrictTotallyOrdered<detail::checked_common_ref_t<
                                        detail::clref_t<std::remove_reference_t<T>>,
                                        detail::clref_t<std::remove_reference_t<T>>>> &&
    EqualityComparableWith<T, U> &&
    detail::requires_<detail::StrictTotallyOrderedWith_req, T, U>;

// Movable defined above

// [concepts.lib.object.copyable]
template <typename T>
NANO_CONCEPT Copyable = CopyConstructible<T> && Movable<T> && Assignable<detail::lref_t<T>, detail::clref_t<T>>;

// [concepts.lib.object.semiregular]
template <typename T>
NANO_CONCEPT Semiregular = Copyable<T> && DefaultConstructible<T>;

// [concepts.lib.object.regular]
template <typename T>
NANO_CONCEPT Regular = Semiregular<T> && EqualityComparable<T>;

namespace detail {

#ifdef NANO_HAVE_CPP17
using std::invoke;
using std::invoke_result;
using std::invoke_result_t;
#else
// Reimplementation of std::invoke
inline namespace invoke_ {

template <typename>
constexpr bool is_reference_wrapper_v = false;

template <typename T>
constexpr bool is_reference_wrapper_v<std::reference_wrapper<T>> = true;

template <class Base, class T, class Derived, class... Args>
auto INVOKE(T Base::*pmf, Derived&& ref, Args&&... args)
-> typename std::enable_if<std::is_function<T>::value &&
                           std::is_base_of<Base, typename std::decay<Derived>::type>::value,
    decltype((std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...))>::type
{
    return (std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...);
}

template <class Base, class T, class RefWrap, class... Args>
auto INVOKE(T Base::*pmf, RefWrap&& ref, Args&&... args)
-> typename std::enable_if<std::is_function<T>::value &&
                           is_reference_wrapper_v<typename std::decay<RefWrap>::type>,
    decltype((ref.get().*pmf)(std::forward<Args>(args)...))>::type
{
    return (ref.get().*pmf)(std::forward<Args>(args)...);
}

template <class Base, class T, class Pointer, class... Args>
auto INVOKE(T Base::*pmf, Pointer&& ptr, Args&&... args)
-> typename std::enable_if<std::is_function<T>::value &&
                           !is_reference_wrapper_v<typename std::decay<Pointer>::type> &&
                           !std::is_base_of<Base, typename std::decay<Pointer>::type>::value,
    decltype(((*std::forward<Pointer>(ptr)).*pmf)(std::forward<Args>(args)...))>::type
{
    return ((*std::forward<Pointer>(ptr)).*pmf)(std::forward<Args>(args)...);
}

template <class Base, class T, class Derived>
auto INVOKE(T Base::*pmd, Derived&& ref)
-> typename std::enable_if<!std::is_function<T>::value &&
                           std::is_base_of<Base, typename std::decay<Derived>::type>::value,
    decltype(std::forward<Derived>(ref).*pmd)>::type
{
    return std::forward<Derived>(ref).*pmd;
}

template <class Base, class T, class RefWrap>
auto INVOKE(T Base::*pmd, RefWrap&& ref)
-> typename std::enable_if<!std::is_function<T>::value &&
                           is_reference_wrapper_v<typename std::decay<RefWrap>::type>,
    decltype(ref.get().*pmd)>::type
{
    return ref.get().*pmd;
}

template <class Base, class T, class Pointer>
auto INVOKE(T Base::*pmd, Pointer&& ptr)
-> typename std::enable_if<!std::is_function<T>::value &&
                           !is_reference_wrapper_v<typename std::decay<Pointer>::type> &&
                           !std::is_base_of<Base, typename std::decay<Pointer>::type>::value,
    decltype((*std::forward<Pointer>(ptr)).*pmd)>::type
{
    return (*std::forward<Pointer>(ptr)).*pmd;
}

template <class F, class... Args>
auto INVOKE(F&& f, Args&&... args)
-> typename std::enable_if<!std::is_member_pointer<typename std::decay<F>::type>::value,
    decltype(std::forward<F>(f)(std::forward<Args>(args)...))>::type
{
    return std::forward<F>(f)(std::forward<Args>(args)...);
}

}

template <typename F, typename... Args>
auto invoke(F&& f, Args&&... args) -> decltype(INVOKE(std::forward<F>(f), std::forward<Args>(args)...))
{
    return INVOKE(std::forward<F>(f), std::forward<Args>(args)...);
}

template <typename Void, typename, typename...>
struct invoke_result {};

template <typename F, typename... Args>
struct invoke_result<void_t<decltype(INVOKE(std::declval<F>(), std::declval<Args>()...))>, F, Args...>
{
    using type = decltype(INVOKE(std::declval<F>(), std::declval<Args>()...));
};

template <typename F, typename... Args>
using invoke_result_t = typename invoke_result<void, F, Args...>::type;

#endif

template <typename F, typename... Args>
using checked_invoke_result_t = test_t<invoke_result_t, F, Args...>;

struct Invocable_req {
    /*template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) -> decltype(
        detail::invoke(std::forward<F>(f), std::forward<Args>(args)...)
    );*/
    // FIXME: Clang really doesn't like the above, work out why
    template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) ->
        invoke_result_t<F, Args...>;
};

}

template <typename F, typename... Args>
NANO_CONCEPT Invocable = detail::requires_<detail::Invocable_req, F, Args...>;

template <typename F, typename... Args>
NANO_CONCEPT RegularInvocable = Invocable<F, Args...>;

template <typename F, typename... Args>
NANO_CONCEPT Predicate = RegularInvocable<F, Args...> &&
    Boolean<detail::checked_invoke_result_t<F, Args...>>;

template <typename R, typename T, typename U>
NANO_CONCEPT Relation = Predicate<R, T, T> && Predicate<R, U, U> &&
    CommonReference<detail::clref_t<std::remove_reference_t<T>>, detail::clref_t<std::remove_reference_t<U>>> &&
    Predicate<R,
        detail::checked_common_ref_t<detail::clref_t<std::remove_reference_t<T>>, detail::clref_t<std::remove_reference_t<U>>>,
        detail::checked_common_ref_t<detail::clref_t<std::remove_reference_t<T>>, detail::clref_t<std::remove_reference_t<U>>>> &&
    Predicate<R, T, U> && Predicate<R, U, T>;

template <typename R, typename T, typename U>
NANO_CONCEPT StrictWeakOrder = Relation<R, T, U>;


// Utilities

struct identity {
    template <typename T>
    constexpr T&& operator()(T&& t) const noexcept
    {
        return std::forward<T>(t);
    }

    using is_transparent = std::true_type;
};

//  [rand.req.urng]

namespace detail {

struct UniformRandomBitGenerator_req {
    template <typename G>
    auto requires_() -> decltype(valid_expr(
        G::min(), requires_expr<Same<decltype(G::min()), detail::invoke_result_t<G&>>>{},
        G::max(), requires_expr<Same<decltype(G::max()), detail::invoke_result_t<G&>>>{}
    ));
};

}

template <typename G>
NANO_CONCEPT UniformRandomBitGenerator =
    Invocable<G&> && UnsignedIntegral<detail::checked_invoke_result_t<G&>> &&
    detail::requires_<detail::UniformRandomBitGenerator_req, G>;

// [range.swap]

namespace detail {
namespace swap_ {

#ifndef MSVC_NO_POISON_PILLS
template <typename T>
void swap(T&, T&) = delete;

template <typename T, std::size_t N>
void swap(T (&)[N], T (&)[N]) = delete;
#endif

template <typename T, typename U>
using custom_swap_t = decltype(swap(std::declval<T>(), std::declval<U>()));

template <typename T, typename U>
constexpr bool has_custom_swap_v = exists_v<custom_swap_t, T, U>;

// I'll be honest, I copied this from STL2. Thanks Casey.
template <typename, typename, typename, typename = void>
constexpr bool is_cpo_swappable_v = false;

template <typename F, typename T, typename U>
constexpr bool is_cpo_swappable_v<F, T, U, void_t<std::result_of_t<F&(T&, U&)>>> = true;

struct swap_cpo {

    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(swap(std::forward<T>(t), std::forward<U>(u))))
        -> decltype(static_cast<void>(swap(std::forward<T>(t), std::forward<U>(u))))
    {
        (void) swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U, std::size_t N, typename F = swap_cpo>
    constexpr auto operator()(T (&t)[N], U (&u)[N]) const
    noexcept(noexcept(std::declval<F&>()(*t, *u)))
        -> std::enable_if_t<
                !has_custom_swap_v<T (&)[N], U (&)[N]> &&
                 is_cpo_swappable_v<F, T, U>>
    {
        for (std::size_t i = 0; i < N; ++i) {
                (*this)(t[i], u[i]);
        }
    }

    template <typename T>
    constexpr auto operator()(T& a, T& b) const
        noexcept(std::is_nothrow_move_constructible<T>::value &&
                 std::is_nothrow_assignable<T&, T>::value)
        -> std::enable_if_t<!has_custom_swap_v<T&, T&> &&
                            MoveConstructible<T> &&
                            Assignable<T&, T>>
    {
        T temp = std::move(a);
        a = std::move(b);
        b = std::move(temp);
    }

};

} // end namespace swap_

} // end namespace detail

constexpr auto swap = detail::swap_::swap_cpo{};

namespace detail {

// Implement the Swappable concepts now we have swap()

struct Swappable_req {
    template <typename T>
    auto requires_(T& a, T& b) -> decltype(
        nanorange::swap(a, b)
    );
};

template <typename T>
constexpr bool swappable_f()
{
    return requires_<Swappable_req, T>;
}

struct SwappableWith_req {
    template <typename T, typename U>
    auto requires_(T&& t, U&& u) -> decltype(
        nanorange::swap(std::forward<T>(t), std::forward<T>(t)),
        nanorange::swap(std::forward<U>(u), std::forward<U>(u)),
        nanorange::swap(std::forward<T>(t), std::forward<U>(u)),
        nanorange::swap(std::forward<U>(u), std::forward<T>(t))
    );
};

template <typename T, typename U>
constexpr bool swappable_with_f()
{
    return CommonReference<clref_t<std::remove_reference_t<T>>,
                           clref_t<std::remove_reference_t<U>>> &&
            requires_<SwappableWith_req, T, U>;
}

}


// [range.synopsis]

namespace detail {

template <typename T>
auto not_void(T&&) -> int;

struct Dereferenceable_req {
    template <typename T>
    auto requires_(T& t) -> decltype(valid_expr(
        not_void(*t)
    ));
};

template <typename T>
NANO_CONCEPT Dereferenceable = requires_<Dereferenceable_req, T>;

}


namespace detail {
namespace iter_move_ {

template <typename E>
using adl_iter_move_t = decltype(static_cast<decltype(iter_move(
        std::declval<E>()))>(iter_move(std::declval<E>())));

template <typename T>
constexpr bool has_adl_iter_move_v = exists_v<adl_iter_move_t, T>;

struct iter_move_cpo {
    template <typename T>
    constexpr auto operator()(T&& t) const
    noexcept(noexcept(static_cast<decltype(iter_move(t))>(iter_move(t))))
    -> decltype(static_cast<decltype(iter_move(t))>(iter_move(t)))
    {
        return static_cast<decltype(iter_move(t))>(iter_move(t));
    }

    template <typename T>
    constexpr auto operator()(T& t) const
    noexcept(noexcept(std::move(*std::declval<T&>())))
    -> std::enable_if_t<!has_adl_iter_move_v<T&>,
            decltype(std::move(*t))>
    {
        return std::move(*t);
    }

    template <typename T>
    constexpr auto operator()(T&& t) const
    noexcept(noexcept(static_cast<decltype(*t)>(*t)))
    -> std::enable_if_t<!has_adl_iter_move_v<T&&>,
            decltype(static_cast<decltype(*t)>(*t))>
    {
        return static_cast<decltype(*t)>(*t);
    }
};

}


}


namespace {

constexpr auto iter_move = detail::iter_move_::iter_move_cpo{};

}

template <typename>
struct difference_type;

namespace detail {

template<typename, typename = void>
struct difference_type_ {};

template<typename T>
struct difference_type_<T*>
    : std::enable_if<std::is_object<T>::value, std::ptrdiff_t> {
};

template<class I>
struct difference_type_<const I> : difference_type<std::decay_t<I>> {};

template <typename, typename = void>
constexpr bool has_member_difference_type_v = false;

template <typename T>
constexpr bool has_member_difference_type_v<T, void_t<typename T::difference_type>> = true;

template<typename T>
struct difference_type_<T, std::enable_if_t<has_member_difference_type_v<T>>> {
    using type = typename T::difference_type;
};

template<typename T>
struct difference_type_<T, std::enable_if_t<
    !std::is_pointer<T>::value &&
    !has_member_difference_type_v<T> &&
     Integral<decltype(std::declval<const T&>() - std::declval<const T&>())>>>
    : std::make_signed<decltype(std::declval<T>() - std::declval<T>())> {
};

}

template <typename T>
struct difference_type : detail::difference_type_<T> {};

template <typename T>
using difference_type_t = typename difference_type<T>::type;

// [range.iterator.assoc.types.value_type]

template <typename>
struct value_type;

namespace detail {

template<typename, typename = void>
struct value_type_helper {
};

template<typename T>
struct value_type_helper<T*>
    : std::enable_if<std::is_object<T>::value, std::remove_cv_t<T>> {
};

template<typename T>
struct value_type_helper<T, std::enable_if_t<std::is_array<T>::value>>
    : value_type<std::decay_t<T>> {
};

template<typename I>
struct value_type_helper<const I, std::enable_if_t<!std::is_array<I>::value>>
    : value_type<std::decay_t<I>> {
};

template<typename T, typename V = typename T::value_type>
struct member_value_type
    : std::enable_if<std::is_object<V>::value, V> {};

template<typename T, typename E = typename T::element_type>
struct member_element_type
    : std::enable_if<std::is_object<E>::value, E> {};

template <typename T>
using member_value_type_t = typename T::value_type;

template <typename T>
constexpr bool has_member_value_type_v = exists_v<member_value_type_t, T>;

template <typename T>
using member_element_type_t = typename T::element_type;

template <typename T>
constexpr bool has_member_element_type_v = exists_v<member_element_type_t, T>;

template<typename T>
struct value_type_helper<T, std::enable_if_t<has_member_value_type_v<T>>>
    : member_value_type<T> {};

template<typename T>
struct value_type_helper<T, std::enable_if_t<has_member_element_type_v<T>>>
    : member_element_type<T> {};

} // namespace detail

template <typename T>
struct value_type : detail::value_type_helper<T> {};

template <typename T>
using value_type_t = typename value_type<T>::type;

// [range.iterator.assoc.types.iterator_category]
// FIXME: Not to spec -- do we want to duplicate all the iterator tags, or just use the std:: ones?

using std::input_iterator_tag;
using std::output_iterator_tag;
using std::forward_iterator_tag;
using std::bidirectional_iterator_tag;
using std::random_access_iterator_tag;

template <typename T>
struct iterator_category;

namespace detail {

template <typename T, typename = void>
struct iterator_category_ {};

template <typename T>
struct iterator_category_<T*>
    : std::enable_if<std::is_object<T>::value, random_access_iterator_tag> {};

template <typename T>
struct iterator_category_<const T> : iterator_category<T> {};

template <typename T>
struct iterator_category_<T, void_t<typename T::iterator_category>> {
    using type = typename T::iterator_category;
};

} // namespace detail

template <typename T>
struct iterator_category : detail::iterator_category_<T> {};

template <typename T>
using iterator_category_t = typename iterator_category<T>::type;

namespace detail {

template <typename, typename = void>
struct reference_helper {};

template <typename T>
struct reference_helper<T, std::enable_if_t<Dereferenceable<T>>> {
    using type = decltype(*std::declval<T&>());
};

}

template <typename T>
using reference_t = typename detail::reference_helper<T>::type;

template <typename T>
using rvalue_reference_t = decltype(nanorange::iter_move(std::declval<T&>()));

namespace detail {

template <typename T>
using checked_difference_type_t = test_t<difference_type_t, T>;

template <typename T>
using checked_value_type_t = test_t<value_type_t, T>;

template <typename T>
using checked_iterator_category_t = test_t<iterator_category_t, T>;

template <typename T>
using checked_reference_t = test_t<reference_t, T>;

template <typename T>
using checked_rvalue_ref_t = test_t<rvalue_reference_t, T>;

}

// [range.iterators.readable]
namespace detail {

struct Readable_req {
    template <typename In>
    auto requires_() -> decltype(valid_expr(
        std::declval<value_type_t<In>>(),
        std::declval<reference_t<In>>(),
        std::declval<rvalue_reference_t<In>>()
    ));
};

} // namespace detail

template <typename In>
NANO_CONCEPT Readable =
    detail::requires_<detail::Readable_req, In> &&
    CommonReference<detail::checked_reference_t<In>&&,
                    detail::checked_value_type_t<In>&> &&
    CommonReference<detail::checked_reference_t<In>&&,
                    detail::checked_rvalue_ref_t<In>&> &&
    CommonReference<detail::checked_rvalue_ref_t<In>&&,
                    const detail::checked_value_type_t<In>&>;

// [range.iterators.writable]
namespace detail {

struct Writable_req {
    template <typename Out, typename T>
    auto requires_(Out&& o, T&& t) -> decltype(valid_expr(
        *o = std::forward<T>(t),
        *std::forward<Out>(o) = std::forward<T>(t),
        const_cast<const reference_t<Out>&&>(*o) = std::forward<T>(t),
        const_cast<const reference_t<Out>&&>(*std::forward<Out>(o)) = std::forward<T>(t)
    ));
};

}

template <typename Out, typename T>
NANO_CONCEPT Writable =
    detail::requires_<detail::Writable_req, Out, T>;


// [range.iterators.weaklyincrementable]

namespace detail {

template <typename T, typename Deduced>
auto same_lv(Deduced&) -> std::enable_if_t<Same<T, Deduced>, int>;

template <typename T, typename Deduced>
auto same_rv(Deduced&&) -> std::enable_if_t<Same<T, Deduced>, int>;

struct WeaklyIncrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(valid_expr(
        std::declval<difference_type_t<I>>(),
        requires_expr<SignedIntegral<difference_type_t<I>>>{},
        same_lv<I>(++i),
        i++
    ));
};

} // namespace detail

template <typename I>
NANO_CONCEPT WeaklyIncrementable =
    Semiregular<I> &&
    detail::requires_<detail::WeaklyIncrementable_req, I>;

// [range.iterators.incrementable]

namespace detail {

struct Incrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(valid_expr(
        same_rv<I>(i++)
    ));
};

}

template <typename I>
NANO_CONCEPT Incrementable =
    Regular<I> &&
    WeaklyIncrementable<I> &&
    detail::requires_<detail::Incrementable_req, I>;


// [range.iterators.iterator]

namespace detail {

struct Iterator_req {
    template <typename I>
    auto requires_(I i) -> decltype(valid_expr(
        not_void(*i)
    ));
};

}

template <typename I>
NANO_CONCEPT Iterator =
    detail::requires_<detail::Iterator_req, I> &&
    WeaklyIncrementable<I>;

// [range.iterators.sentinel]

template <typename S, typename I>
NANO_CONCEPT Sentinel =
    Semiregular<S> &&
    Iterator<I> &&
    detail::WeaklyEqualityComparableWith<S, I>;

// [range.iterators.sizedsentinel]

template <typename S, typename I>
constexpr bool disable_sized_sentinel = false;

namespace detail {

struct SizedSentinel_req {
    template <typename S, typename I>
    auto requires_(const S& s, const I& i) -> decltype(valid_expr(
        same_rv<difference_type_t<I>>(s - i),
        same_rv<difference_type_t<I>>(i - s)
    ));
};

}

template <typename S, typename I>
NANO_CONCEPT SizedSentinel =
    Sentinel<S, I> &&
    !disable_sized_sentinel<std::remove_cv_t<S>, std::remove_cv_t<I>> &&
    detail::requires_<detail::SizedSentinel_req, S, I>;

// [range.iterators.input]

template <typename I>
NANO_CONCEPT InputIterator =
    Iterator<I> &&
    Readable<I> &&
    detail::exists_v<iterator_category_t, I> &&
    DerivedFrom<detail::checked_iterator_category_t<I>, input_iterator_tag>;

// [ranges.iterator.output]

namespace detail {

struct OutputIterator_req {
    template <typename I, typename T>
    auto requires_(I i, T&& t) -> decltype(valid_expr(
        *i++ = std::forward<T>(t)
    ));
};

}

template <typename I, typename T>
NANO_CONCEPT OutputIterator =
    Iterator<I> &&
    Writable<I, T> &&
    detail::requires_<detail::OutputIterator_req, I, T>;

// [ranges.iterators.forward]

template <typename I>
NANO_CONCEPT ForwardIterator =
    InputIterator<I> &&
    DerivedFrom<detail::checked_iterator_category_t<I>, forward_iterator_tag> &&
    Incrementable<I> &&
    Sentinel<I, I>;

// [ranges.iterators.bidirectional]

namespace detail {

struct BidirectionalIterator_req {
    template <typename I>
    auto requires_(I i) -> decltype(valid_expr(
        same_lv<I>(--i),
        same_rv<I>(i--)
    ));
};

}

template <typename I>
NANO_CONCEPT BidirectionalIterator =
    ForwardIterator<I> &&
    DerivedFrom<detail::checked_iterator_category_t<I>, bidirectional_iterator_tag> &&
    detail::requires_<detail::BidirectionalIterator_req, I>;

// [ranges.iterators.random.access]

namespace detail {

struct RandomAccessIterator_req {
    template <typename I>
    auto requires_(I i, const I j, const difference_type_t<I> n) -> decltype(valid_expr(
        same_lv<I>(i += n),
        same_rv<I>(j + n),
        n + j, //same_rv<I>(n + j) -- FIXME: MSVC doesn't like this with I = int*, find out why
        same_lv<I>(i -= n),
        same_rv<I>(j - n),
        j[n],
        requires_expr<Same<decltype(j[n]), reference_t<I>>>{}
    ));
};

}

template <typename I>
NANO_CONCEPT RandomAccessIterator =
    BidirectionalIterator<I> &&
    DerivedFrom<detail::checked_iterator_category_t<I>, random_access_iterator_tag> &&
    StrictTotallyOrdered<I> &&
    SizedSentinel<I, I> &&
    detail::requires_<detail::RandomAccessIterator_req, I>;


// [range.indirectcallable.indirectinvocable]

namespace detail {

template <typename, typename = void>
struct iter_common_ref_helper {};

template <typename T>
struct iter_common_ref_helper<T, std::enable_if_t<Readable<T>>> {
    using type = common_reference_t<reference_t<T>, value_type_t<T>&>;
};

}

template <typename I>
using iter_common_reference_t = typename detail::iter_common_ref_helper<I>::type;

namespace detail {

template <typename I>
using checked_iter_common_ref_t = test_t<iter_common_reference_t, I>;

}

template <typename F, typename I>
NANO_CONCEPT IndirectUnaryInvocable =
    Readable<I> &&
    CopyConstructible<F> &&
    Invocable<detail::lref_t<F>, detail::checked_value_type_t<I>&> &&
    Invocable<detail::lref_t<F>, detail::checked_reference_t<I>&> &&
    Invocable<detail::lref_t<F>, detail::checked_iter_common_ref_t<I>> &&
    CommonReference<
        detail::checked_invoke_result_t<detail::lref_t<F>, detail::checked_value_type_t<I>&>,
        detail::checked_invoke_result_t<detail::lref_t<F>, detail::checked_reference_t<I>>>;

template <typename F, typename I>
NANO_CONCEPT IndirectRegularUnaryInvocable =
    Readable<I> &&
    CopyConstructible<F> &&
    RegularInvocable<detail::lref_t<F>, detail::checked_value_type_t<I>&> &&
    RegularInvocable<detail::lref_t<F>, detail::checked_reference_t<I>&> &&
    RegularInvocable<detail::lref_t<F>, detail::checked_iter_common_ref_t<I>> &&
    CommonReference<
        detail::checked_invoke_result_t<detail::lref_t<F>, detail::checked_value_type_t<I>&>,
        detail::checked_invoke_result_t<detail::lref_t<F>, detail::checked_reference_t<I>>>;


template <typename F, typename I>
NANO_CONCEPT IndirectUnaryPredicate =
    Readable<I> &&
    CopyConstructible<F> &&
    Predicate<detail::lref_t<F>, detail::checked_value_type_t<I>&> &&
    Predicate<detail::lref_t<F>, detail::checked_reference_t<I>> &&
    Predicate<detail::lref_t<F>, detail::checked_iter_common_ref_t<I>>;

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectRelation =
    Readable<I1> && Readable<I2> &&
    CopyConstructible<F> &&
    Relation<detail::lref_t<F>, detail::checked_value_type_t<I1>&, detail::checked_value_type_t<I2>&> &&
    Relation<detail::lref_t<F>, detail::checked_value_type_t<I1>&, detail::checked_reference_t<I2>> &&
    Relation<detail::lref_t<F>, detail::checked_reference_t<I1>, detail::checked_value_type_t<I2>&> &&
    Relation<detail::lref_t<F>, detail::checked_reference_t<I1>, detail::checked_reference_t<I2>> &&
    Relation<detail::lref_t<F>, detail::checked_iter_common_ref_t<I1>, detail::checked_iter_common_ref_t<I2>>;

template <typename F, typename I1, typename I2 = I1>
NANO_CONCEPT IndirectStrictWeakOrder =
    Readable<I1> && Readable<I2> &&
    CopyConstructible<F> &&
    StrictWeakOrder<detail::lref_t<F>, detail::checked_value_type_t<I1>&, detail::checked_value_type_t<I2>&> &&
    StrictWeakOrder<detail::lref_t<F>, detail::checked_value_type_t<I1>&, detail::checked_reference_t<I2>> &&
    StrictWeakOrder<detail::lref_t<F>, detail::checked_reference_t<I1>, detail::checked_value_type_t<I2>&> &&
    StrictWeakOrder<detail::lref_t<F>, detail::checked_reference_t<I1>, detail::checked_reference_t<I2>> &&
    StrictWeakOrder<detail::lref_t<F>, detail::checked_iter_common_ref_t<I1>, detail::checked_iter_common_ref_t<I2>>;


template <typename, typename...>
struct indirect_result;

namespace detail {

template <typename Void, typename, typename...>
struct indirect_result_helper {};

template <bool...>
struct all_readable_helper;

template <>
struct all_readable_helper<> : std::true_type {};

template <bool First, bool... Rest>
struct all_readable_helper<First, Rest...>
    : std::conditional_t<First, all_readable_helper<Rest...>, std::false_type> {};

template <typename... Is>
constexpr bool all_readable = all_readable_helper<Readable<Is>...>::value;

template <typename F, typename... Is>
struct indirect_result_helper<
    std::enable_if_t<all_readable<Is...> && Invocable<F, reference_t<Is>...>>, F, Is...>
    : invoke_result<F, reference_t<Is>...>
{
    using type = invoke_result_t<F, reference_t<Is>...>;
};

} // namespace detail

template <typename F, typename... Is>
struct indirect_result : detail::indirect_result_helper<void, F, Is...> {};

template <typename F, typename... Is>
using indirect_result_t = typename indirect_result<F, Is...>::type;

namespace detail {

template <typename F, typename... Is>
using checked_indirect_result_t = test_t<indirect_result_t, F, Is...>;

}

// [range.projected]

template <typename I, typename Proj>
struct projected;

namespace detail {

template <typename, typename, typename = void>
struct projected_helper {};

template <typename I, typename Proj>
struct projected_helper<I, Proj, std::enable_if_t<
    Readable<I> && IndirectRegularUnaryInvocable<Proj, I>>>
{
    using value_type = std::remove_cv_t<std::remove_reference_t<indirect_result_t<Proj&, I>>>;
    indirect_result_t<Proj&, I> operator*() const;
};

}

template <typename I, typename Proj>
struct projected : detail::projected_helper<I, Proj> {};

// range.commonalgoreq.indirectlymovable]

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovable =
    Readable<In> &&
    Writable<Out, detail::checked_rvalue_ref_t<In>>;

template <typename In, typename Out>
NANO_CONCEPT IndirectlyMovableStorable =
    IndirectlyMovable<In, Out> &&
    Writable<Out, detail::checked_value_type_t<In>> &&
    Movable<detail::checked_value_type_t<In>> &&
    Constructible<detail::checked_value_type_t<In>, detail::checked_rvalue_ref_t<In>> &&
    Assignable<detail::checked_value_type_t<In>&, detail::checked_rvalue_ref_t<In>>;

// range.commonalgoreq.indirectlycopyable

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyable =
    Readable<In> &&
    Writable<Out, detail::checked_reference_t<Out>>;

template <typename In, typename Out>
NANO_CONCEPT IndirectlyCopyableStorable =
    IndirectlyCopyable<In, Out> &&
    Writable<Out, const detail::checked_value_type_t<In>&> &&
    Copyable<detail::checked_value_type_t<In>> &&
    Constructible<detail::checked_value_type_t<In>, detail::checked_rvalue_ref_t<In>> &&
    Assignable<detail::checked_value_type_t<In>&, detail::checked_reference_t<In>>;

namespace detail {
namespace iter_swap_ {

#ifndef MSVC_NO_POISON_PILLS
template <typename I1, typename I2>
void iter_swap(I1, I2) = delete;
#endif

struct iter_swap_cpo {
private:
    template <typename T1, typename T2>
    static constexpr bool iter_exchange_move_noexcept =
            std::is_nothrow_constructible<value_type_t<T1>, rvalue_reference_t<T1>>::value &&
                    std::is_nothrow_assignable<value_type_t<T1>&, rvalue_reference_t<T1>>::value &&
                    std::is_nothrow_assignable<reference_t<T1>, rvalue_reference_t<T2>>::value &&
                    std::is_nothrow_assignable<reference_t<T1>, value_type_t<T2>>::value &&
                    std::is_nothrow_move_constructible<value_type_t<T1>>::value &&
                    noexcept(nanorange::iter_move(std::declval<T1&>()));

    template <typename X, typename Y>
    static constexpr value_type_t<std::remove_reference_t<X>>
    iter_exchange_move(X&& x, Y&& y)
    noexcept(iter_exchange_move_noexcept<std::remove_reference_t<X>, std::remove_reference_t<Y>> &&
             iter_exchange_move_noexcept<std::remove_reference_t<Y>, std::remove_reference_t<X>>)
    {
        value_type_t<std::remove_reference_t<X>> old_value(nanorange::iter_move(x));
        *x = nanorange::iter_move(y);
        return old_value;
    }


    template <typename T, typename U>
    static constexpr auto do_swap(T&& t, U&& u, priority_tag<2>)
        noexcept(noexcept((void)(iter_swap(std::forward<T>(t), std::forward<U>(u)))))
        -> decltype((void)(iter_swap(std::forward<T>(t), std::forward<U>(u))))
    {
        (void) iter_swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U>
    static constexpr auto do_swap(T&& t, U&& u, priority_tag<1>)
    -> std::enable_if_t<Readable<T> &&
                        Readable<U> &&
                        SwappableWith<reference_t<T>, reference_t<U>>>
    {
        nanorange::swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U>
    static constexpr auto do_swap(T&& t, U&& u, priority_tag<0>)
        -> std::enable_if_t<IndirectlyMovableStorable<T, U> &&
                            IndirectlyMovableStorable<U, T>>
    {
        return *t = iter_swap_cpo::iter_exchange_move(std::forward<U>(u),
                                                      std::forward<T>(t));
    }

public:
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{})))
        -> decltype(do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{}))
    {
        return do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{});
    }

};

}
}

namespace {

constexpr auto iter_swap = detail::iter_swap_::iter_swap_cpo{};

}


namespace detail {

struct IndirectlySwappable_req {
    template <typename I1, typename I2>
    auto requires_(I1&& i1, I2&& i2) -> decltype(valid_expr(
        nanorange::iter_swap(std::forward<I1>(i1), std::forward<I2>(i2)),
        nanorange::iter_swap(std::forward<I2>(i2), std::forward<I1>(i1)),
        nanorange::iter_swap(std::forward<I1>(i1), std::forward<I1>(i1)),
        nanorange::iter_swap(std::forward<I2>(i2), std::forward<I2>(i2))
    ));
};

}

template <typename I1, typename I2 = I1>
NANO_CONCEPT IndirectlySwappable =
    Readable<I1> && Readable<I2> &&
    detail::requires_<detail::IndirectlySwappable_req, I1, I2>;

// [range.commonalgoreq.indirectlycomparable]

template <typename I1, typename I2, typename R = std::equal_to<>,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT IndirectlyComparable =
    IndirectRelation<R, projected<I1, P2>, projected<I2, P2>>;

// [range.commonalgoreq.permutable]

template <typename I>
NANO_CONCEPT Permutable =
    ForwardIterator<I> &&
    IndirectlyMovableStorable<I, I> &&
    IndirectlySwappable<I, I>;

// [range.commonalgoreq.mergeable]

template <typename I1, typename I2, typename Out, typename R = std::less<>,
          typename P1 = identity, typename P2 = identity>
NANO_CONCEPT Mergeable =
    InputIterator<I1> &&
    InputIterator<I2> &&
    WeaklyIncrementable<Out> &&
    IndirectlyCopyable<I1, Out> &&
    IndirectlyCopyable<I2, Out> &&
    IndirectStrictWeakOrder<R, projected<I1, P1>, projected<I2, P2>>;

// [range.commonalgoreq.sortable]

template <typename I, typename R = std::less<>, typename P = identity>
NANO_CONCEPT Sortable =
    Permutable<I> &&
    IndirectStrictWeakOrder<R, projected<I, P>>;


//  [range.iterator.operations]


// [range.iterators.predef]

// [range.iterators.reverse]

// [range.iterators.common]

template <typename I, typename S>
class common_iterator {
    static_assert(Iterator<I>, "");
    static_assert(Sentinel<S, I>, "");
    static_assert(!Same<I, S>, "");

public:
    using difference_type = difference_type_t<I>;

    constexpr common_iterator()
        : is_sentinel_{false},
          iter_{} {}

    constexpr common_iterator(I i)
        : is_sentinel_{false},
          iter_(i) {}

    constexpr common_iterator(S s)
        : is_sentinel_{true},
          sentinel_{s}
    {}

    template <typename II, typename SS, std::enable_if_t<
              ConvertibleTo<II, I> && ConvertibleTo<SS, S>, int> = 0>
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
    };

    decltype(auto) operator*()
    {
        return *iter_;
    }

    template <typename II = I, std::enable_if_t<detail::Dereferenceable<const I>, int> = 0>
    decltype(auto) operator*() const
    {
        return *iter_;
    };

    decltype(auto) operator->() const;

    common_iterator& operator++()
    {
        ++iter_;
        return *this;
    }

    template <typename II = I, std::enable_if_t<!ForwardIterator<II>, int> = 0>
    decltype(auto) operator++(int)
    {
        return iter_++;
    }

    template <typename II = I, std::enable_if_t<ForwardIterator<II>, int> = 0>
    common_iterator operator++(int)
    {
        common_iterator tmp = *this;
        ++iter_;
        return tmp;
    }

    friend rvalue_reference_t<I> iter_move(const common_iterator& i)
    {
        return nanorange::iter_move(i.iter_);
    }

    template <typename I2, typename S2>
    friend std::enable_if_t<IndirectlySwappable<I2, I>>
    iter_swap(const common_iterator& x, const common_iterator<I2, S2>& y)
    {
        return nanorange::iter_swap(x.iter_, y.iter_);
    }

private:


    // TODO: Some sort of variant-like union
    bool is_sentinel_;
    I iter_;
    S sentinel_;
};

template <typename I, typename S>
struct value_type<common_iterator<I, S>> {
    using type = value_type_t<I>;
};

template <typename I, typename S>
struct iterator_category<common_iterator<I, S>>
    : std::conditional<
        ForwardIterator<I>,
        forward_iterator_tag,
        input_iterator_tag> {};

template <typename I1, typename I2, typename S1, typename S2,
          std::enable_if_t<!EqualityComparableWith<I1, I2>, int> = 0>
bool operator==(const common_iterator<I1, S1>& x,
                const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_ ?
           (y.is_sentinel_ || y.iter_ == x.sentinel_) :
           (!y.is_sentinel_ || x.iter_ == y.sentinel_);
}


template <typename I1, typename I2, typename S1, typename S2,
          std::enable_if_t<EqualityComparableWith<I1, I2>, int> = 0>
bool operator==(const common_iterator<I1, S1>& x,
                const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_ ?
           (y.is_sentinel_ || y.iter_ == x.sentinel_) :
           (y.is_sentinel_ ?
            x.iter_ == y.sentinel_ :
            x.iter_ == y.iter_);
}

template <typename I1, typename I2, typename S1, typename S2>
bool operator!=(const common_iterator<I1, S1>& x,
                const common_iterator<I2, S2>& y)
{
    return !(x == y);
}

template <typename I2, typename I1, typename S1, typename S2>
std::enable_if_t<
    SizedSentinel<I1, I2> &&
    SizedSentinel<S1, I2> &&
    SizedSentinel<S2, I2>,
    difference_type_t<I2>>
operator-(const common_iterator<I1, S1>& x, const common_iterator<I2, S2>& y)
{
    return x.is_sentinel_ ?
                  (y.is_sentinel_ ? 0 : x.sentinel_ - y.iter_)
                          :  (y.is_sentinel_ ?
                   x.iter_ - y.sentinel_ :
                   x.iter_ - y.iter_);

}


// [range.unreachable.sentinels]

class unreachable {};

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool>
operator==(const I&, unreachable) noexcept
{
    return false;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool>
operator==(unreachable, const I&) noexcept
{
    return false;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool>
operator!=(const I&, unreachable) noexcept
{
    return true;
}

template <typename I>
constexpr std::enable_if_t<Iterator<I>, bool>
operator!=(unreachable, const I&) noexcept
{
    return true;
}



// [range.access.begin]

namespace detail {

inline namespace begin_poison_pill {

#ifndef NANO_MSVC_NO_POISON_PILLS
template <typename T>
void begin(T&) = delete;
#endif

template <typename T>
using member_begin_t = decltype(std::declval<T&>().begin());

template <typename T>
constexpr bool has_member_begin_v = exists_v<member_begin_t, T>;

struct begin_cpo {
private:
    template <typename T>
    static constexpr auto do_begin(const T&& t, priority_tag<3>)
        noexcept(noexcept(do_begin(static_cast<const T&>(t), priority_tag<2>{})))
        -> decltype(do_begin(static_cast<const T&>(t), priority_tag<2>{}))
    {
        return do_begin(static_cast<const T&>(t), priority_tag<2>{});
    }

    template <typename T, std::size_t N>
    static constexpr auto do_begin(T (& t)[N], priority_tag<2>) noexcept
        -> decltype((t) + 0)
    {
        return (t) + 0;
    }

    template <typename T>
    static constexpr auto do_begin(T& t, priority_tag<1>)
        noexcept(noexcept(t.begin()))
        -> std::enable_if_t<Iterator<decltype(t.begin())>, decltype(t.begin())>
    {
        return t.begin();
    }

    template <typename T>
    static constexpr auto do_begin(T& t, priority_tag<0>)
        noexcept(noexcept(begin(t)))
        -> std::enable_if_t<Iterator<decltype(begin(t))>, decltype(begin(t))>
    {
        return begin(t);
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(do_begin(std::forward<T>(t), priority_tag<3>{})))
        -> decltype(do_begin(std::forward<T>(t), priority_tag<3>{}))
    {
        return do_begin(std::forward<T>(t), priority_tag<3>{});
    }

};

}
}

constexpr auto begin = detail::begin_cpo{};

namespace detail {
inline namespace end_poison_pill {

#ifndef NANO_MSVC_NO_POISON_PILLS
template <typename T>
void end(T&) = delete;
#endif

template <typename T>
using member_end_t = decltype(std::declval<T&>().end());

template <typename T>
constexpr bool has_member_end_v = exists_v<member_end_t, T>;

struct end_cpo {
private:
    template <typename T, std::size_t N>
    static constexpr auto do_end(T (&t)[N], priority_tag<2>) noexcept
        -> decltype(t + N)
    {
        return t + N;
    }

    template <typename T,
              typename S = decltype(std::declval<T&>().end()),
              typename I = decltype(nanorange::begin(std::declval<T&>()))>
    static constexpr auto do_end(T& t, priority_tag<1>)
        noexcept(noexcept(t.end()))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return t.end();
    }

    template <typename T,
              typename S = decltype(end(std::declval<T&>())),
              typename I = decltype(nanorange::begin(std::declval<T&>()))>
    static constexpr auto do_end(T& t, priority_tag<0>)
        noexcept(noexcept(end(t)))
         -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return end(t);
    }

    template <typename T>
    static constexpr auto do_end(const T&& t, priority_tag<3>)
    noexcept(noexcept(do_end(static_cast<const T&>(t), priority_tag<3>{})))
    -> decltype(do_end(static_cast<const T&>(t), priority_tag<3>{}))
    {
        return do_end(static_cast<const T&>(t), priority_tag<2>{});
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(do_end(std::forward<T>(t), priority_tag<3>{})))
        -> decltype(do_end(std::forward<T>(t), priority_tag<3>{}))
    {
        return do_end(std::forward<T>(t), priority_tag<3>{});
    }
};

}
}

constexpr auto end = detail::end_cpo{};

// [range.access.cbegin]

namespace detail {

struct cbegin_cpo {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(nanorange::begin(t)))
        -> decltype(nanorange::begin(t))
    {
        return nanorange::begin(t);
    }

};

}

constexpr auto cbegin = detail::begin_cpo{};

// [ranges.access.cend]

namespace detail {

struct cend_cpo {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(nanorange::end(t)))
        -> decltype(nanorange::end(t))
    {
        return nanorange::end(t);
    }

};


}

constexpr auto cend = detail::cend_cpo{};


// [range.primitives.size]

template <typename T>
constexpr bool disable_sized_range = false;

namespace detail {
inline namespace size_poison_pill {

#ifndef MSVC_NO_POISON_PILLS
template <typename T>
void size(const T&) = delete;
#endif

template <typename T>
using member_size_t = decltype(std::declval<const T&>().size());

template <typename T>
constexpr bool has_member_size_v = exists_v<member_size_t, T>;

struct size_cpo {

    template <typename T, std::size_t N>
    constexpr std::size_t operator()(const T(&)[N]) const noexcept
    {
        return N;
    }

    template <typename T, typename I = decltype(std::declval<const T&>().size())>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(t.size()))
        -> std::enable_if_t<Integral<I> &&
                            !disable_sized_range<T>, I>
    {
        return t.size();
    }

    template <typename T, typename I = decltype(size(std::declval<const T&>()))>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(size(t)))
        -> std::enable_if_t<!has_member_size_v<T> &&
                            Integral<I> &&
                            !disable_sized_range<T>, I>
    {
        return size(t);
    }

    template <typename T,
              typename I = decltype(nanorange::cbegin(std::declval<T>())),
              typename S = decltype(nanorange::cend(std::declval<T>())),
              typename D = decltype(std::declval<S>() - std::declval<I>())>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(nanorange::cend(t) - nanorange::cbegin(t)))
        -> std::enable_if_t<SizedSentinel<S, I> &&
                            ForwardIterator<I>, D>
    {
        return nanorange::cend(t) - nanorange::cbegin(t);
    }
};

}
}

constexpr auto size = detail::size_cpo{};

// [range.comparisons]

// TODO: Constrained versions of these
using std::equal_to;
using std::not_equal_to;
using std::less;
using std::greater;
using std::less_equal;
using std::greater_equal;

template <typename T>
using iterator_t = decltype(nanorange::begin(std::declval<T&>()));

template <typename T>
using sentinel_t = decltype(nanorange::end(std::declval<T&>()));

namespace detail {

template <typename T>
using checked_iterator_t = test_t<iterator_t, T>;

template <typename T>
using checked_sentinel_t = test_t<sentinel_t, T>;

}

template <typename T>
struct enable_view {};

struct view_base {};

// [range.range]

namespace detail {

struct Range_req {
    template <typename T>
    auto requires_(T&& t) -> decltype(valid_expr(
        nanorange::begin(t),
        nanorange::end(t)
    ));
};

}

template <typename T>
NANO_CONCEPT Range =
    detail::requires_<detail::Range_req, T>;


// [range.sized]

namespace detail {

template <typename T, typename Deduced>
auto convertible_to_helper(Deduced) -> std::enable_if_t<ConvertibleTo<Deduced, T>, int>;

struct SizedRange_req {
    template <typename T>
    auto requires_(T& t) -> decltype(valid_expr(
        convertible_to_helper<difference_type_t<iterator_t<T>>>(nanorange::size(t))
    ));
};

}

template <typename T>
NANO_CONCEPT SizedRange =
    Range<T> &&
    !disable_sized_range<std::remove_cv_t<std::remove_reference_t<T>>> &&
    detail::requires_<detail::SizedRange_req, T>;

// [range.view]

namespace detail {

template <typename, typename = void>
constexpr bool view_predicate = true;

template <typename T>
using enable_view_t = typename enable_view<T>::type;

template <typename T>
constexpr bool has_enable_view_v = exists_v<enable_view_t, T>;

template <typename T>
constexpr bool view_predicate<T, std::enable_if_t<
    has_enable_view_v<T>>>
    = enable_view<T>::type::value;

template <typename T>
constexpr bool view_predicate<T, std::enable_if_t<
    !has_enable_view_v<T> &&
    DerivedFrom<T, view_base>>>
    = true;

template <typename T>
constexpr bool view_predicate<std::initializer_list<T>> = false;

template <typename K, typename C, typename A>
constexpr bool view_predicate<std::set<K, C, A>> = false;

template <typename K, typename C, typename A>
constexpr bool view_predicate<std::multiset<K, C, A>> = false;

template <typename K, typename H, typename E, typename A>
constexpr bool view_predicate<std::unordered_set<K, H, E, A>> = false;

template <typename K, typename H, typename E, typename A>
constexpr bool view_predicate<std::unordered_multiset<K, H, E, A>> = false;

template <typename T>
constexpr bool view_predicate<T, std::enable_if_t<
    !has_enable_view_v<T> &&
    !DerivedFrom<T, view_base> &&
    Range<T> && Range<const T> &&
    !Same<reference_t<iterator_t<T>>, reference_t<iterator_t<const T>>>>>
    = false;

}

template <typename T>
NANO_CONCEPT View =
    Range<T> &&
    Semiregular<T> &&
    detail::view_predicate<T>;


// [range.common]

template <typename T>
NANO_CONCEPT CommonRange =
    Range<T> && Same<detail::checked_iterator_t<T>, detail::checked_sentinel_t<T>>;

// [range.input]

template <typename T>
NANO_CONCEPT InputRange =
    Range<T> && InputIterator<detail::checked_iterator_t<T>>;

template <typename R, typename T>
NANO_CONCEPT OutputRange =
    Range<R> && OutputIterator<detail::checked_iterator_t<R>, T>;

template <typename T>
NANO_CONCEPT ForwardRange =
    InputRange<T> && ForwardIterator<detail::checked_iterator_t<T>>;

template <typename T>
NANO_CONCEPT BidirectionalRange =
    ForwardRange<T> && BidirectionalIterator<detail::checked_iterator_t<T>>;

template <typename T>
NANO_CONCEPT RandomAccessRange =
    BidirectionalRange<T> && RandomAccessIterator<detail::checked_iterator_t<T>>;

// [range.dangling.wrap]

template <typename T>
struct dangling {
    static_assert(CopyConstructible<T>,
                  "Argument to dangling<T> must satisfy CopyConstructible<T>");

    template <typename U = T, std::enable_if_t<DefaultConstructible<U>>>
    dangling() : value_{} {}

    dangling(T t) : value_(t) {}

    T get_unsafe() { return value_; }

private:
    T value_;
};

template <typename Range>
using safe_iterator_t = std::conditional_t<
    std::is_lvalue_reference<Range>::value,
    iterator_t<Range>, dangling<iterator_t<Range>>>;


// [range.alg.all_of]

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
all_of(I first, S last,  Pred pred, Proj proj = Proj{})
{
    while (first != last) {
        if (!detail::invoke(pred, detail::invoke(proj, *first))) {
            return false;
        }
        ++first;
    }
    return true;
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
all_of(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return nanorange::all_of(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

// [ranges.alg.any_of]

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
any_of(I first, S last, Pred pred, Proj proj = Proj{})
{
    while (first != last) {
        if (detail::invoke(pred, detail::invoke(proj, *first)) == true) {
            return true;
        }
        ++first;
    }
    return false;
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
any_of(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return any_of(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

// [ranges.alg.none_of]

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
none_of(I first, S last, Pred pred, Proj proj = Proj{})
{
    return !any_of(first, last, std::ref(pred), std::ref(proj));
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
none_of(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return none_of(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

// [range.alg.foreach]

template <typename I, typename S, typename Proj = identity, typename Fun>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryInvocable<Fun, I/*projected<I, Proj>*/>,
    std::pair<I, Fun>> // FIXME: use tagged_pair
for_each(I first, S last, Fun fun, Proj proj = Proj{})
{
    while (first != last) {
        detail::invoke(fun, detail::invoke(proj, *first));
        ++first;
    }
    return {last, std::move(fun)};
}

template <typename Rng, typename Proj = identity, typename Fun>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryInvocable<Fun, iterator_t<Rng>/*projected<iterator_t<Rng>, Proj>*/>,
    std::pair<safe_iterator_t<Rng>, Fun>>
for_each(Rng&& rng, Fun fun, Proj proj = Proj{})
{
    return for_each(nanorange::begin(rng), nanorange::end(rng), std::ref(fun), std::ref(proj));
}

// [ranges.alg.find]

template <typename I, typename S, typename T, typename Proj = identity>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectRelation<equal_to<>, projected<I, Proj>, const T*>, I>
find(I first, S last, const T& value, Proj proj = Proj{})
{
    while (first != last) {
        if (detail::invoke(proj, *first) == value) {
            return first;
        }
        ++first;
    }
    return first;
}

template <typename Rng, typename T, typename Proj = identity>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>, const T*>,
    safe_iterator_t<Rng>>
find(Rng&& rng, const T& value, Proj proj = Proj{})
{
    return find(nanorange::begin(rng), nanorange::end(rng), value, std::ref(proj));
}

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
find_if(I first, S last, Pred pred, Proj proj = Proj{})
{
    while (first != last) {
        if (detail::invoke(pred, detail::invoke(proj, *first))) {
            return first;
        }
        ++first;
    }
    return first;
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
find_if(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return find_if(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
find_if_not(I first, S last, Pred pred, Proj proj = Proj{})
{
    while (first != last) {
        if (detail::invoke(pred, detail::invoke(proj, *first)) == false) {
            return first;
        }
        ++first;
    }
    return first;
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
find_if_not(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return find_if_not(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

// [ranges.alg.find.end]

template <typename I1, typename I2, typename Pred = equal_to<>>
std::enable_if_t<
    ForwardIterator<I1> &&
    ForwardIterator<I2> &&
    IndirectRelation<Pred, I2, I1>, I1>
find_end(I1 first1, I1 last1, I2 first2, I2 last2, Pred pred = Pred{})
{
    return std::find_end(std::move(first1), std::move(last1),
                         std::move(first2), std::move(last2),
                         std::ref(pred));
}

template <typename Rng1, typename Rng2, typename Pred = equal_to<>>
std::enable_if_t<
    ForwardRange<Rng1> &&
    ForwardRange<Rng2> &&
    CommonRange<Rng1> &&
    CommonRange<Rng2> &&
    IndirectRelation<Pred, iterator_t<Rng2>, iterator_t<Rng1>>,
    safe_iterator_t<Rng1>>
find_end(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{})
{
    return std::find_end(nanorange::begin(rng1), nanorange::end(rng1),
                         nanorange::begin(rng2), nanorange::end(rng2),
                         std::ref(pred));
}

// [range.alg.find.first.of]

template <typename I1, typename I2, typename Pred = equal_to<>>
std::enable_if_t<
    InputIterator<I1> &&
    ForwardIterator<I2> &&
    IndirectRelation<Pred, I1, I2>, I1>
find_first_of(I1 first1, I1 last1, I2 first2, I2 last2, Pred pred = Pred{})
{
    return std::find_first_of(std::move(first1), std::move(last1),
                              std::move(first2), std::move(last2),
                              std::ref(pred));
}

template <typename Rng1, typename Rng2, typename Pred = equal_to<>>
std::enable_if_t<
    InputRange<Rng1> &&
    ForwardRange<Rng2> &&
    CommonRange<Rng1> &&
    CommonRange<Rng2> &&
    IndirectRelation<Pred, iterator_t<Rng1>, iterator_t<Rng2>>,
    safe_iterator_t<Rng1>>
find_first_of(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{})
{
    return std::find_first_of(nanorange::begin(rng1), nanorange::end(rng1),
                              nanorange::begin(rng2), nanorange::end(rng2),
                              std::ref(pred));
}


// [range.alg.adjacent.find]

template <typename I, typename S, typename Proj = identity, typename Pred = equal_to<>>
std::enable_if_t<
    ForwardIterator<I> &&
    Sentinel<S, I> &&
    IndirectRelation<Pred, projected<I, Proj>>, I>
adjacent_find(I first, S last, Pred pred = Pred{}, Proj proj = Proj{})
{
    if (first == last) {
        return first;
    }

    I next = first;
    ++next;
    while (next != last) {
        if (detail::invoke(pred, detail::invoke(proj, *first), detail::invoke(proj, *next)) != false) {
            return first;
        }
        ++first; ++next;
    }

    return first;
}

template <typename Rng, typename Proj = identity, typename Pred = equal_to<>>
std::enable_if_t<
    ForwardRange<Rng> &&
    IndirectRelation<Pred, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
adjacent_find(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{})
{
    return adjacent_find(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

// [rng.alg.count]

template <typename I, typename S, typename T, typename Proj = identity>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
    difference_type_t<I>>
count(I first, S last, const T& value, Proj proj = Proj{})
{
    difference_type_t<I> counter = 0;

    while (first != last) {
        if (detail::invoke(proj, *first) == value) {
            ++counter;
        }
        ++first;
    }

    return counter;
}

template <typename Rng, typename T, typename Proj = identity>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>, const T*>,
    difference_type_t<iterator_t<Rng>>>
count(Rng&& rng, const T& value, Proj proj = Proj{})
{
    return count(nanorange::begin(rng), nanorange::end(rng), value, std::ref(proj));
}

template <typename I, typename S, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>,
    difference_type_t<I>>
count_if(I first, S last, Pred pred, Proj proj = Proj{})
{
    difference_type_t<I> counter = 0;

    while (first != last) {
        if (detail::invoke(pred, detail::invoke(proj, *first)) != false) {
            ++counter;
        }
        ++first;
    }

    return counter;
}

template <typename Rng, typename Proj = identity, typename Pred>
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
    difference_type_t<iterator_t<Rng>>>
count_if(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return count_if(nanorange::begin(rng), nanorange::end(rng), std::ref(pred), std::ref(proj));
}

}; // namespace nanorange

#endif // NANORANGE_CONCEPTS_HPP_INCLUDED
