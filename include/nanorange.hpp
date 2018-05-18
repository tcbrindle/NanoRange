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

namespace nano {
inline namespace ranges {

#if (__cplusplus >= 201703) || (defined(_MSVC_LANG) && _MSVC_LANG >= 201703L)
#define NANO_HAVE_CPP17
#endif

#if defined(NANO_HAVE_CPP17) || defined(__cpp_inline_variables)
#define NANO_HAVE_INLINE_VARS
#endif

// FIXME: Work out how to use the "poison pills" on MSVC
#ifdef _MSC_VER
#define NANO_MSVC_NO_POISON_PILLS
#endif


#define NANO_CONCEPT constexpr bool


#ifdef NANO_HAVE_INLINE_VARS
#define NANO_INLINE_VAR(type, name) \
    inline namespace function_objects { \
        inline constexpr type name{}; \
    }

#else
#define NANO_INLINE_VAR(type, name) \
    inline namespace function_objects { \
        inline namespace { \
            constexpr const auto& name = ::nano::detail::static_const_<type>::value; \
        } \
    }
#endif

namespace detail {

template <typename T>
struct static_const_ {
    static constexpr T value{};
};

template <typename T>
constexpr T static_const_<T>::value;

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

template <typename T>
constexpr std::decay_t<T> decay_copy(T&& t)
    noexcept(noexcept(static_cast<std::decay_t<T>>(std::forward<T>(t))))
{
    return std::forward<T>(t);
}

// #define decay_copy(x) (static_cast<std::decay_t<decltype((x))>>(x))

}

namespace detail {

template <typename T, typename U>
struct copy_cv { using type = U; };

template <typename T, typename U>
struct copy_cv<const T, U> { using type = std::add_const_t<U>; };

template <typename T, typename U>
struct copy_cv<volatile T, U> { using type = std::add_volatile_t<U>; };

template <typename T, typename U>
struct copy_cv<const volatile T, U> { using type = std::add_cv_t<U>; };

template <typename T, typename U>
using copy_cv_t = typename copy_cv<T, U>::type;

template <typename T>
using cref_t = std::add_lvalue_reference_t<const std::remove_reference_t<T>>;

template <typename T>
using uncvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template <typename T>
struct rref_res { using type = T; };

template <typename T>
struct rref_res<T&> { using type = std::remove_reference_t<T>&&; };

template <typename T>
using rref_res_t = typename rref_res<T>::type;

template <typename T, typename U>
using cond_res_t = decltype(std::declval<bool>() ? std::declval<T(&)()>()() : std::declval<U(&)()>()());

// For some value of "simple"
template <typename T, typename U>
struct simple_common_reference {};

template <typename T, typename U,
    typename C = test_t<cond_res_t, copy_cv_t<T, U>&, copy_cv_t<U, T>&>>
struct lvalue_simple_common_reference
    : std::enable_if<std::is_reference<C>::value, C> {};

template <typename T, typename U>
using lvalue_scr_t = typename lvalue_simple_common_reference<T, U>::type;

template <typename T, typename U>
struct simple_common_reference<T&, U&>
    : lvalue_simple_common_reference<T, U> {};

template <typename T, typename U,
    typename LCR = test_t<lvalue_scr_t, T, U>,
    typename C = rref_res_t<LCR>>
struct rvalue_simple_common_reference
    : std::enable_if<
        std::is_convertible<T&&, C>::value &&
        std::is_convertible<U&&, C>::value, C> {};

template <typename T, typename U>
struct simple_common_reference<T&&, U&&>
    : rvalue_simple_common_reference<T, U> {};

template <typename A, typename B,
    typename C = test_t<lvalue_scr_t, A, const B>>
struct mixed_simple_common_reference
    : std::enable_if<std::is_convertible<B&&, C>::value, C> {};

template <typename A, typename B>
struct simple_common_reference<A&, B&&>
    : mixed_simple_common_reference<A, B> {};

template <typename A, typename B>
struct simple_common_reference<A&&, B&>
    : simple_common_reference<B&, A&&> {};

template <typename T, typename U>
using simple_common_reference_t = typename simple_common_reference<T, U>::type;

}

template <class T, class U, template <class> class TQual, template <class> class UQual>
struct basic_common_reference {};

template <typename...>
struct common_reference;

template <typename... Ts>
using common_reference_t = typename common_reference<Ts...>::type;

template <>
struct common_reference<> {};

template <typename T0>
struct common_reference<T0> {
    using type = T0;
};

namespace detail {

template <typename T, typename U>
constexpr bool has_simple_common_ref_v = exists_v<simple_common_reference_t, T, U>;

template <typename T>
T common_ref_test_func();

template <typename T, typename U,
    typename C = decltype(false ? common_ref_test_func<T>() : common_ref_test_func<U>())>
struct function_common_ref {
    using type = C;
};

template <typename T, typename U>
using function_common_ref_t = typename function_common_ref<T, U>::type;

template <typename T, typename U>
constexpr bool has_function_common_ref_v = exists_v<function_common_ref_t, T, U>;

template <typename T, typename U, typename = void>
struct binary_common_ref
    : std::common_type<T, U> {};

template <typename T, typename U>
struct binary_common_ref<T, U, std::enable_if_t<
    has_simple_common_ref_v<T, U>>>
    : simple_common_reference<T, U> {};

template <typename T, typename U>
struct binary_common_ref<T, U, std::enable_if_t<
    has_function_common_ref_v<T, U> &&
    !has_simple_common_ref_v<T, U>>>
    : function_common_ref<T, U> {};

}

// FIXME: Handle basic_common_reference
template <typename T1, typename T2>
struct common_reference<T1, T2>
    : detail::binary_common_ref<T1, T2> {};

namespace detail {

template <typename Void, typename T1, typename T2, typename... Rest>
struct multiple_common_reference {};

template <typename T1, typename T2, typename... Rest>
struct multiple_common_reference<
    void_t<common_reference_t<T1, T2>>, T1, T2, Rest...>
    : common_reference<common_reference_t<T1, T2>, Rest...> {};

}

template <typename T1, typename T2, typename... Rest>
struct common_reference<T1, T2, Rest...>
    : detail::multiple_common_reference<void, T1, T2, Rest...> {};

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

// [concepts.lib.corelang.copyconstructible]
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

// Reimplementation of std::invoke
inline namespace invoke_ {

template<typename>
constexpr bool is_reference_wrapper_v = false;

template<typename T>
constexpr bool is_reference_wrapper_v<std::reference_wrapper<T>> = true;

struct fn {
private:
    template <class Base, class T, class Derived, class... Args>
    static constexpr auto impl(T Base::*pmf, Derived&& ref, Args&& ... args)
    noexcept(noexcept((std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...)))
    -> std::enable_if_t<std::is_function<T>::value &&
                        std::is_base_of<Base, std::decay_t<Derived>>::value,
                        decltype((std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...))>
    {
        return (std::forward<Derived>(ref).*pmf)(std::forward<Args>(args)...);
    }

    template <class Base, class T, class RefWrap, class... Args>
    static constexpr auto impl(T Base::*pmf, RefWrap&& ref, Args&& ... args)
        noexcept(noexcept((ref.get().*pmf)(std::forward<Args>(args)...)))
        -> std::enable_if_t<std::is_function<T>::value &&
            is_reference_wrapper_v<std::decay_t<RefWrap>>,
            decltype((ref.get().*pmf)(std::forward<Args>(args)...))>
    {
        return (ref.get().*pmf)(std::forward<Args>(args)...);
    }

    template <class Base, class T, class Pointer, class... Args>
    static constexpr auto impl(T Base::*pmf, Pointer&& ptr, Args&& ... args)
        noexcept(noexcept(((*std::forward<Pointer>(ptr)).*pmf)(std::forward<Args>(args)...)))
        -> std::enable_if_t<std::is_function<T>::value &&
            !is_reference_wrapper_v<std::decay_t<Pointer>> &&
            !std::is_base_of<Base, std::decay_t<Pointer>>::value,
            decltype(((*std::forward<Pointer>(ptr)).*pmf)(
                    std::forward<Args>(args)...))>
    {
        return ((*std::forward<Pointer>(ptr)).*pmf)(
                std::forward<Args>(args)...);
    }

    template <class Base, class T, class Derived>
    static constexpr auto impl(T Base::*pmd, Derived&& ref)
    noexcept(noexcept(std::forward<Derived>(ref).*pmd))
    -> std::enable_if_t<!std::is_function<T>::value &&
            std::is_base_of<Base, std::decay_t<Derived>>::value,
            decltype(std::forward<Derived>(ref).*pmd)>
    {
        return std::forward<Derived>(ref).*pmd;
    }

    template <class Base, class T, class RefWrap>
    static constexpr auto impl(T Base::*pmd, RefWrap&& ref)
    noexcept(noexcept(ref.get().*pmd))
    -> std::enable_if_t<!std::is_function<T>::value &&
            is_reference_wrapper_v<std::decay_t<RefWrap>>,
            decltype(ref.get().*pmd)>
    {
        return ref.get().*pmd;
    }

    template <class Base, class T, class Pointer>
    static constexpr auto impl(T Base::*pmd, Pointer&& ptr)
    noexcept(noexcept((*std::forward<Pointer>(ptr)).*pmd))
    -> std::enable_if_t<!std::is_function<T>::value &&
            !is_reference_wrapper_v<std::decay_t<Pointer>> &&
            !std::is_base_of<Base, std::decay_t<Pointer>>::value,
            decltype((*std::forward<Pointer>(ptr)).*pmd)>
    {
        return (*std::forward<Pointer>(ptr)).*pmd;
    }

    template <class F, class... Args>
    static constexpr auto impl(F&& f, Args&& ... args)
    noexcept(noexcept(std::forward<F>(f)(std::forward<Args>(args)...)))
    -> std::enable_if_t<!std::is_member_pointer<std::decay_t<F>>::value,
            decltype(std::forward<F>(f)(std::forward<Args>(args)...))>
    {
        return std::forward<F>(f)(std::forward<Args>(args)...);
    }

public:

    template <typename F, typename... Args>
    constexpr auto operator()(F&& f, Args&&... args) const
        noexcept(noexcept(fn::impl(std::forward<F>(f), std::forward<Args>(args)...)))
        -> decltype(fn::impl(std::forward<F>(f), std::forward<Args>(args)...))
    {
        return fn::impl(std::forward<F>(f), std::forward<Args>(args)...);
    }
};

}
}

NANO_INLINE_VAR(nano::detail::invoke_::fn, invoke)

namespace detail {

template<typename Void, typename, typename...>
struct invoke_result_helper {
};

template<typename F, typename... Args>
struct invoke_result_helper<void_t<decltype(nano::invoke(std::declval<F>(), std::declval<Args>()...))>, F, Args...> {
    using type = decltype(nano::invoke(std::declval<F>(), std::declval<Args>()...));
};

}

template <typename F, typename... Args>
struct invoke_result : detail::invoke_result_helper<void, F, Args...> {};

template <typename F, typename... Args>
using invoke_result_t = typename invoke_result<F, Args...>::type;

namespace detail {

template <typename F, typename... Args>
using checked_invoke_result_t = test_t<invoke_result_t, F, Args...>;

struct Invocable_req {
    /*template <typename F, typename... Args>
    auto requires_(F&& f, Args&&... args) -> decltype(
        nano::invoke(std::forward<F>(f), std::forward<Args>(args)...)
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
        G::min(), requires_expr<Same<decltype(G::min()), invoke_result_t<G&>>>{},
        G::max(), requires_expr<Same<decltype(G::max()), invoke_result_t<G&>>>{}
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
constexpr bool is_cpo_swappable_v<F, T, U, void_t<invoke_result_t<F&, T&, U&>>> = true;

struct fn {
private:
    template <typename T, typename U>
    static constexpr auto do_swap(T&& t, U&& u, priority_tag<2>)
        noexcept(noexcept(swap(std::forward<T>(t), std::forward<U>(u))))
        -> decltype(static_cast<void>(swap(std::forward<T>(t), std::forward<U>(u))))
    {
        (void) swap(std::forward<T>(t), std::forward<U>(u));
    }

    template <typename T, typename U, std::size_t N, typename F = fn>
    static constexpr auto do_swap(T (&t)[N], U (&u)[N], priority_tag<1>)
    noexcept(noexcept(std::declval<F&>()(*t, *u)))
        -> decltype(std::declval<F&>()(*t, *u))
    {
        for (std::size_t i = 0; i < N; ++i) {
             fn::do_swap(t[i], u[i], priority_tag<2>{});
        }
    }

    template <typename T>
    static constexpr auto do_swap(T& a, T& b, priority_tag<0>)
        noexcept(std::is_nothrow_move_constructible<T>::value &&
                 std::is_nothrow_assignable<T&, T>::value)
        -> std::enable_if_t<MoveConstructible<T> &&
                            Assignable<T&, T>>
    {
        T temp = std::move(a);
        a = std::move(b);
        b = std::move(temp);
    }

public:
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{})))
        -> decltype(fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{}))
    {
        return fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{});
    }

};

} // end namespace swap_

} // end namespace detail

NANO_INLINE_VAR(detail::swap_::fn, swap)

namespace detail {

// Implement the Swappable concepts now we have swap()

struct Swappable_req {
    template <typename T>
    auto requires_(T& a, T& b) -> decltype(
        ranges::swap(a, b)
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
        ranges::swap(std::forward<T>(t), std::forward<T>(t)),
        ranges::swap(std::forward<U>(u), std::forward<U>(u)),
        ranges::swap(std::forward<T>(t), std::forward<U>(u)),
        ranges::swap(std::forward<U>(u), std::forward<T>(t))
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

// [range.comparisons]

// TODO: Constrained versions of the rest of these
template <typename = void, typename = void>
struct equal_to;

template <typename T>
struct equal_to<T, std::enable_if_t<EqualityComparable<T>>>
        : std::equal_to<T> {};

template <>
struct equal_to<void>
{
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u)
    -> std::enable_if_t<EqualityComparableWith<T, U>, bool>
    {
        return std::equal_to<>{}(std::forward<T>(t), std::forward<U>(u));
    }

    using is_transparent = std::true_type;
};

using std::not_equal_to;
using std::less;
using std::greater;
using std::less_equal;
using std::greater_equal;


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

struct fn {
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

NANO_INLINE_VAR(detail::iter_move_::fn, iter_move)

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
using rvalue_reference_t = decltype(ranges::iter_move(std::declval<T&>()));

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

#ifndef NANO_MSVC_NO_POISON_PILLS
template <typename I1, typename I2>
void iter_swap(I1, I2) = delete;
#endif

struct fn {
private:
    template <typename T1, typename T2>
    static constexpr bool iter_exchange_move_noexcept =
            std::is_nothrow_constructible<value_type_t<T1>, rvalue_reference_t<T1>>::value &&
                    std::is_nothrow_assignable<value_type_t<T1>&, rvalue_reference_t<T1>>::value &&
                    std::is_nothrow_assignable<reference_t<T1>, rvalue_reference_t<T2>>::value &&
                    std::is_nothrow_assignable<reference_t<T1>, value_type_t<T2>>::value &&
                    std::is_nothrow_move_constructible<value_type_t<T1>>::value &&
                    noexcept(ranges::iter_move(std::declval<T1&>()));

    template <typename X, typename Y>
    static constexpr value_type_t<std::remove_reference_t<X>>
    iter_exchange_move(X&& x, Y&& y)
    noexcept(iter_exchange_move_noexcept<std::remove_reference_t<X>, std::remove_reference_t<Y>> &&
             iter_exchange_move_noexcept<std::remove_reference_t<Y>, std::remove_reference_t<X>>)
    {
        value_type_t<std::remove_reference_t<X>> old_value(ranges::iter_move(x));
        *x = ranges::iter_move(y);
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
        noexcept(noexcept(ranges::swap(*std::forward<T>(t), *std::forward<U>(u))))
    -> std::enable_if_t<Readable<T> &&
                        Readable<U> &&
                        SwappableWith<reference_t<T>, reference_t<U>>>
    {
        ranges::swap(*std::forward<T>(t), *std::forward<U>(u));
    }

    template <typename T, typename U>
    static constexpr auto do_swap(T&& t, U&& u, priority_tag<0>)
        noexcept(noexcept(*t = fn::iter_exchange_move(std::forward<U>(u),
                                                      std::forward<T>(t))))
        -> std::enable_if_t<IndirectlyMovableStorable<T&&, U&&> &&
                            IndirectlyMovableStorable<U&&, T&&>>
    {
        return *t = fn::iter_exchange_move(std::forward<U>(u),
                                           std::forward<T>(t));
    }

public:
    template <typename T, typename U>
    constexpr auto operator()(T&& t, U&& u) const
        noexcept(noexcept(fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{})))
        -> decltype(fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{}))
    {
        return fn::do_swap(std::forward<T>(t), std::forward<U>(u), priority_tag<2>{});
    }

};

}
}

NANO_INLINE_VAR(detail::iter_swap_::fn, iter_swap)


namespace detail {

struct IndirectlySwappable_req {
    template <typename I1, typename I2>
    auto requires_(I1&& i1, I2&& i2) -> decltype(valid_expr(
        ranges::iter_swap(std::forward<I1>(i1), std::forward<I2>(i2)),
        ranges::iter_swap(std::forward<I2>(i2), std::forward<I1>(i1)),
        ranges::iter_swap(std::forward<I1>(i1), std::forward<I1>(i1)),
        ranges::iter_swap(std::forward<I2>(i2), std::forward<I2>(i2))
    ));
};

}

template <typename I1, typename I2 = I1>
NANO_CONCEPT IndirectlySwappable =
    Readable<I1> && Readable<I2> &&
    detail::requires_<detail::IndirectlySwappable_req, I1, I2>;

// [range.commonalgoreq.indirectlycomparable]

template <typename I1, typename I2, typename R = equal_to<>,
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

template <typename I1, typename I2, typename Out, typename R = less<>,
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

namespace common_iterator_ {

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
        return ranges::iter_move(i.iter_);
    }

    template <typename I2, typename S2>
    friend std::enable_if_t<IndirectlySwappable<I2, I>>
    iter_swap(const common_iterator& x, const common_iterator<I2, S2>& y)
    {
        return ranges::iter_swap(x.iter_, y.iter_);
    }

private:
    // TODO: Some sort of variant-like union
    bool is_sentinel_;
    I iter_;
    S sentinel_;
};



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

} // namespace common_iterator

using common_iterator_::common_iterator;

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
namespace begin_ {

// MSVC doesn't mind this for some reason
template <typename T>
void begin(T&&) = delete;

template <typename T>
void begin(std::initializer_list<T>&&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype((t) + 0)
    {
        return (t) + 0;
    }

    template <typename T>
    static constexpr auto impl(T& t, priority_tag<1>)
        noexcept(noexcept(decay_copy(t.begin())))
        -> std::enable_if_t<Iterator<decltype(decay_copy(t.begin()))>,
                           decltype(decay_copy(t.begin()))>
    {
        return decay_copy(t.begin());
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(decay_copy(begin(std::forward<T>(t)))))
        -> std::enable_if_t<Iterator<decltype(decay_copy(begin(std::forward<T>(t))))>,
                            decltype(decay_copy(begin(std::forward<T>(t))))>
    {
        return decay_copy(begin(t));
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

}
}

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
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype(t + N)
    {
        return t + N;
    }

    template <typename T,
              typename S = decltype(decay_copy(std::declval<T&>().end())),
              typename I = decltype(ranges::begin(std::declval<T&>()))>
    static constexpr auto impl(T& t, priority_tag<1>)
        noexcept(noexcept(decay_copy(t.end())))
        -> std::enable_if_t<Sentinel<S, I>,
                            decltype(decay_copy(t.end()))>
    {
        return decay_copy(t.end());
    }

    template <typename T,
              typename S = decltype(decay_copy(end(std::declval<T>()))),
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(decay_copy(end(std::forward<T>(t)))))
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

}
}

NANO_INLINE_VAR(detail::end_::fn, end)

// [range.access.cbegin]

namespace detail {
namespace cbegin_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
    noexcept(noexcept(ranges::begin(t)))
        -> decltype(ranges::begin(t))
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

}
}

NANO_INLINE_VAR(detail::cbegin_::fn, cbegin);

// [ranges.access.cend]

namespace detail {
namespace cend_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::end(t)))
        -> decltype(ranges::end(t))
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

}
}

NANO_INLINE_VAR(detail::cend_::fn, cend)

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
    static constexpr std::size_t impl(const T(&)[N], priority_tag<3>) noexcept
    {
        return N;
    }

    template <typename T, typename I = decltype(decay_copy(std::declval<T>().size()))>
    static constexpr auto impl(T&& t, priority_tag<2>)
        noexcept(noexcept(decay_copy(std::forward<T>(t).size())))
        -> std::enable_if_t<Integral<I> &&
                            !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(std::forward<T>(t).size());
    }

    template <typename T, typename I = decltype(decay_copy(size(std::declval<T>())))>
    static constexpr auto impl(T&& t, priority_tag<1>)
        noexcept(noexcept(decay_copy(size(std::forward<T>(t)))))
        -> std::enable_if_t<Integral<I> &&
                            !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(size(std::forward<T>(t)));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>())),
              typename S = decltype(ranges::end(std::declval<T>())),
              typename D = decltype(decay_copy(std::declval<S>() - std::declval<I>()))>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(decay_copy(ranges::end(t) - ranges::begin(t))))
    -> std::enable_if_t<!std::is_array<remove_cvref_t<T>>::value && // MSVC sillyness?
                        SizedSentinel<S, I> &&
                        ForwardIterator<I>, D>
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

}
}

NANO_INLINE_VAR(detail::size_::fn, size)

// [range.primitives.empty]

namespace detail {
namespace empty_ {

struct fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<2>)
        noexcept(noexcept((bool(std::forward<T>(t).empty()))))
        -> decltype((bool(std::forward<T>(t).empty())))
    {
        return bool((std::forward<T>(t).empty()));
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>)
        noexcept(noexcept(ranges::size(std::forward<T>(t)) == 0))
        -> decltype(ranges::size(std::forward<T>(t)) == 0)
    {
        return ranges::size(std::forward<T>(t)) == 0;
    }

    template <typename T, typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>)
        noexcept(noexcept(ranges::begin(t) == ranges::end(t)))
        -> std::enable_if_t<ForwardIterator<I>,
                            decltype(ranges::begin(t) == ranges::end(t))>
    {
        return ranges::begin(t) == ranges::end(t);
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t)
            noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<2>{})))
        -> decltype(fn::impl(std::forward<T>(t), priority_tag<2>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<2>{});
    }
};

}
}

NANO_INLINE_VAR(detail::empty_::fn, empty)

template <typename T>
using iterator_t = decltype(ranges::begin(std::declval<T&>()));

template <typename T>
using sentinel_t = decltype(ranges::end(std::declval<T&>()));

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
        ranges::begin(t),
        ranges::end(t)
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
        convertible_to_helper<difference_type_t<iterator_t<T>>>(ranges::size(t))
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

// [ranges.viewable]

template <typename T>
NANO_CONCEPT ViewableRange =
        Range<T> && (std::is_lvalue_reference<T>::value || View<std::decay_t<T>>);

// [range.dangling.wrap]

template <typename T>
struct dangling {
    static_assert(CopyConstructible<T>,
                  "Argument to dangling<T> must satisfy CopyConstructible<T>");

    template <typename U = T, std::enable_if_t<DefaultConstructible<U>>>
    dangling() : value_{} {}

    dangling(T t) : value_(std::move(t)) {}

    T get_unsafe() { return value_; }

private:
    T value_;
};

namespace detail {

template <typename R, typename = void_t<iterator_t<R>>>
struct safe_iterator_helper {
    using type = dangling<iterator_t<R>>;
};

template <typename R>
struct safe_iterator_helper<R,
    void_t<decltype(ranges::begin(std::declval<R>()))>>
{
    using type = iterator_t<R>;
};

}

template <typename Range>
using safe_iterator_t = typename detail::safe_iterator_helper<Range>::type;

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

// [ranges.view_interface]

namespace detail {

template <typename, typename = void>
struct range_common_iterator_impl;

template <typename R>
struct range_common_iterator_impl<R,
        std::enable_if_t<Range<R> && !CommonRange<R>>>
{
    using type = common_iterator<iterator_t<R>, sentinel_t<R>>;
};

template <typename R>
struct range_common_iterator_impl<R, std::enable_if_t<CommonRange<R>>>
{
    using type = iterator_t<R>;
};

template <typename R>
using range_common_iterator_t = typename range_common_iterator_impl<R>::type;

}

template <typename D>
class view_interface {

    static_assert(std::is_class<D>::value, "");

private:

    constexpr D& derived() noexcept
    {
        return static_cast<D&>(*this);
    }

    constexpr const D& derived() const noexcept
    {
        return static_cast<const D&>(*this);
    }

public:
    template <typename R = D>
    constexpr auto empty()
    -> std::enable_if_t<ForwardRange<R>, bool>
    {
        return ranges::begin(derived()) == ranges::end(derived());
    }

    template <typename = decltype(ranges::empty(std::declval<const D&>()))>
    constexpr explicit operator bool() const
    {
        return ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto data() const
        -> std::enable_if_t<RandomAccessRange<R> &&
                            std::is_pointer<iterator_t<R>>::value,
                            decltype(ranges::begin(derived()))>
    {
        return ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto size() const
        -> std::enable_if_t<ForwardRange<const R> &&
                            SizedSentinel<sentinel_t<const R>, iterator_t<const R>>,
                            decltype(ranges::end(derived()) - ranges::begin(derived()))>
    {
        return ranges::end(derived()) - ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto front()
        -> std::enable_if_t<ForwardRange<R>, decltype(*ranges::begin(derived()))>
    {
        return *ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto front() const
    -> std::enable_if_t<ForwardRange<const R>, decltype(*ranges::begin(derived()))>
    {
        return *ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto back()
        -> std::enable_if_t<BidirectionalRange<R> && CommonRange<R>,
                            decltype(*prev(ranges::end(derived())))>
    {
        return *prev(ranges::end(derived()));
    }

    template <typename R = D>
    constexpr auto back()
    -> std::enable_if_t<BidirectionalRange<const R> && CommonRange<const R>,
            decltype(*prev(ranges::end(derived())))>
    {
        return *prev(ranges::end(derived()));
    }

    template <typename R = D>
    constexpr auto operator[](difference_type_t<iterator_t<R>> n)
        -> std::enable_if_t<RandomAccessRange<R>,
                            decltype(ranges::begin(derived())[n])>
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = const D>
    constexpr auto operator[](difference_type_t<iterator_t<R>> n) const
    -> std::enable_if_t<RandomAccessRange<R>,
            decltype(ranges::begin(derived())[n])>
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = D>
    constexpr auto at(difference_type_t<iterator_t<R>> n)
    -> std::enable_if_t<RandomAccessRange<R> && SizedRange<R>,
                        decltype(derived()[n])>
    {
        if (n < 0 || n >= ranges::size(derived())) {
            throw std::out_of_range{""};
        };

        return derived()[n];
    }

    template <typename R = const D>
    constexpr auto at(difference_type_t<iterator_t<R>> n) const
    -> std::enable_if_t<RandomAccessRange<R> && SizedRange<R>,
            decltype(derived()[n])>
    {
        if (n < 0 || n >= ranges::size(derived())) {
            throw std::out_of_range{""};
        };

        return derived()[n];
    }

    template <typename C, typename = std::enable_if_t<
            ForwardRange<C> &&
            !View<C> &&
            ConvertibleTo<reference_t<iterator_t<const D>>, value_type_t<iterator_t<C>>> &&
            Constructible<C, detail::range_common_iterator_t<const D>, detail::range_common_iterator_t<const D>>>>
    operator C() const
    {
        using I = detail::range_common_iterator_t<D>;
        return C(I{ranges::begin(derived()), ranges::end(derived())});
    }
};

// [range.alg.all_of]

namespace detail {

struct all_of_fn {
private:
    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr bool impl(I first, S last,  Pred pred, Proj proj)
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
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
    operator()(I first, S last,  Pred pred, Proj proj = Proj{}) const
    {
        return all_of_fn::impl(std::move(first), std::move(last),
                               std::move(pred), std::move(proj));
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return all_of_fn::impl(nano::begin(rng), nano::end(rng),
                               std::move(pred), std::move(proj));
    }
};

}

NANO_INLINE_VAR(detail::all_of_fn, all_of)

// [ranges.alg.any_of]

namespace detail {

struct any_of_fn {
private:
    // Allow none_of to use this implementation
    friend struct none_of_fn;

    template <typename I, typename S, typename Proj, typename Pred>
    static constexpr bool impl(I first, S last, Pred pred, Proj proj)
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
    constexpr
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(std::move(first), std::move(last),
                               std::move(pred), std::move(proj));
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr
    std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return any_of_fn::impl(nano::begin(rng), nano::end(rng),
                               std::move(pred), std::move(proj));
    }

};

}

NANO_INLINE_VAR(detail::any_of_fn, any_of)

// [ranges.alg.none_of]

namespace detail {

struct none_of_fn {

    template <typename I, typename S, typename Proj = identity, typename Pred>
    constexpr
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, bool>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(first, last, std::move(pred), std::move(proj));
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr
    std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>, bool>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return !any_of_fn::impl(nano::begin(rng), nano::end(rng),
                                std::move(pred), std::move(proj));
    }

};

}

NANO_INLINE_VAR(detail::none_of_fn, none_of)

// [range.alg.foreach]

namespace detail {

struct for_each_fn {
private:
    template <typename I, typename S, typename Proj, typename Fun>
    static constexpr std::pair<I, Fun> // FIXME: Used tagged pair
    impl(I first, S last, Fun fun, Proj proj)
    {
        while (first != last) {
            nano::invoke(fun, nano::invoke(proj, *first));
            ++first;
        }
        return {last, std::move(fun)};
    }

public:
    template <typename I, typename S, typename Proj = identity, typename Fun>
    constexpr
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryInvocable<Fun, projected<I, Proj>>,
        std::pair<I, Fun>> // FIXME: use tagged_pair
    operator()(I first, S last, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(std::move(first), std::move(last),
                                 std::move(fun), std::move(proj));
    }

    template <typename Rng, typename Proj = identity, typename Fun>
    constexpr
    std::enable_if_t<
        InputRange<Rng> &&
        IndirectUnaryInvocable<Fun, projected<iterator_t<Rng>, Proj>>,
        std::pair<safe_iterator_t<Rng>, Fun>>
    operator()(Rng&& rng, Fun fun, Proj proj = Proj{}) const
    {
        return for_each_fn::impl(nano::begin(rng), nano::end(rng),
                                 std::move(fun), std::move(proj));
    }

};
}

NANO_INLINE_VAR(detail::for_each_fn, for_each)

// [ranges.alg.find]

namespace detail {

struct find_if_fn {
private:
    friend struct find_fn;
    friend struct find_if_not_fn;

    template <typename I, typename S, typename Pred, typename Proj>
    static constexpr I impl(I first, S last, Pred pred, Proj proj)
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
    constexpr
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
    operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(std::move(first), std::move(last),
                                std::move(pred), std::move(proj));
    }

    template <typename Rng, typename Proj = identity, typename Pred>
    constexpr
    std::enable_if_t<
            InputRange<Rng> &&
                    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
            safe_iterator_t<Rng>>
    operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(nano::begin(rng), nano::end(rng),
                                std::move(pred), std::move(proj));
    }
};
}

NANO_INLINE_VAR(detail::find_if_fn, find_if)

namespace detail {

struct find_fn {

    template <typename I, typename S, typename T, typename Proj = identity>
    constexpr
    std::enable_if_t<
        InputIterator<I> &&
        Sentinel<S, I> &&
        IndirectRelation<equal_to<>, projected<I, Proj>, const T*>, I>
    operator()(I first, S last, const T& value, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(std::move(first), std::move(last),
                                [&value] (const auto& v){ return v == value; },
                                std::move(proj));
    }

    template <typename Rng, typename T, typename Proj = identity>
    constexpr
    std::enable_if_t<
        InputRange<Rng> &&
        IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>, const T*>,
        safe_iterator_t<Rng>>
    operator()(Rng&& rng, const T& value, Proj proj = Proj{}) const
    {
        return find_if_fn::impl(nano::begin(rng), nano::end(rng),
                                [&value] (const auto& v) { return v == value; },
                                std::move(proj));
    }
};
}

NANO_INLINE_VAR(detail::find_fn, find)

namespace detail {

struct find_if_not_fn {

template <typename I, typename S, typename Proj = identity, typename Pred>
constexpr
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>, I>
operator()(I first, S last, Pred pred, Proj proj = Proj{}) const
{
    return find_if_fn::impl(std::move(first), std::move(last),
                            [&pred] (auto&& v) {
                                return !nano::invoke(pred, static_cast<decltype(v)>(v)); },
                            std::move(proj));
}

template <typename Rng, typename Proj = identity, typename Pred>
constexpr
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
operator()(Rng&& rng, Pred pred, Proj proj = Proj{}) const
{
    return find_if_fn::impl(nano::begin(rng), nano::end(rng),
                           [&pred] (auto&& v) {
                               return !nano::invoke(pred, static_cast<decltype(v)>(v)); },
                           std::move(proj));
}

};
}

NANO_INLINE_VAR(detail::find_if_not_fn, find_if_not)

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
                         std::move(pred));
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
    return std::find_end(nano::begin(rng1), nano::end(rng1),
                         nano::begin(rng2), nano::end(rng2),
                         std::move(pred));
}

// [range.alg.find.first.of]

namespace detail {

struct find_first_of_fn {
private:

    template <typename I1, typename S1, typename I2, typename S2,
              typename Pred, typename Proj1, typename Proj2>
    static constexpr I1 impl(I1 first1, S1 last1, I2 first2, S2 last2,
                             Pred pred, Proj1 proj1, Proj2 proj2)
    {
        for (; first1 != last1; ++first1) {
            for(I2 it = first2; it != last2; ++it) {
                if (nano::invoke(pred, nano::invoke(proj1, *first1), nano::invoke(proj2, *it))) {
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
        InputIterator<I1> &&
        Sentinel<S1, I1> &&
        ForwardIterator<I2> &&
        Sentinel<S2, I2> &&
        IndirectRelation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
        I1>
    operator()(I1 first1, S1 last1, I2 first2, S2 last2,
               Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_first_of_fn::impl(std::move(first1), std::move(last1),
                                      std::move(first2), std::move(last2),
                                      std::move(pred), std::move(proj1),
                                      std::move(proj2));
    }

    template <typename Rng1, typename Rng2, typename Proj1 = identity,
              typename Proj2 = identity, typename Pred = equal_to<>>
    constexpr std::enable_if_t<
        InputRange<Rng1> &&
        ForwardRange<Rng2> &&
        IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>, projected<iterator_t<Rng2>, Proj2>>,
        safe_iterator_t<Rng1>>
    operator()(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{},
               Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{}) const
    {
        return find_first_of_fn::impl(nano::begin(rng1), nano::end(rng1),
                                      nano::begin(rng2), nano::end(rng2),
                                      std::move(pred), std::move(proj1),
                                      std::move(proj2));
    }

};
}

NANO_INLINE_VAR(detail::find_first_of_fn, find_first_of)

// [range.alg.adjacent.find]

template <typename I, typename S, typename Proj = identity, typename Pred = equal_to<>>
constexpr
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
        if (nano::invoke(pred, nano::invoke(proj, *first), nano::invoke(proj, *next)) != false) {
            return first;
        }
        ++first; ++next;
    }

    return first;
}

template <typename Rng, typename Proj = identity, typename Pred = equal_to<>>
constexpr
std::enable_if_t<
    ForwardRange<Rng> &&
    IndirectRelation<Pred, projected<iterator_t<Rng>, Proj>>,
    safe_iterator_t<Rng>>
adjacent_find(Rng&& rng, Pred pred = Pred{}, Proj proj = Proj{})
{
    return adjacent_find(nano::begin(rng), nano::end(rng), std::move(pred), std::move(proj));
}

// [rng.alg.count]

template <typename I, typename S, typename T, typename Proj = identity>
constexpr
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectRelation<equal_to<>, projected<I, Proj>, const T*>,
    difference_type_t<I>>
count(I first, S last, const T& value, Proj proj = Proj{})
{
    difference_type_t<I> counter = 0;

    while (first != last) {
        if (nano::invoke(proj, *first) == value) {
            ++counter;
        }
        ++first;
    }

    return counter;
}

template <typename Rng, typename T, typename Proj = identity>
constexpr
std::enable_if_t<
    InputRange<Rng> &&
    IndirectRelation<equal_to<>, projected<iterator_t<Rng>, Proj>, const T*>,
    difference_type_t<iterator_t<Rng>>>
count(Rng&& rng, const T& value, Proj proj = Proj{})
{
    return count(nano::begin(rng), nano::end(rng), value, std::move(proj));
}

template <typename I, typename S, typename Proj = identity, typename Pred>
constexpr
std::enable_if_t<
    InputIterator<I> &&
    Sentinel<S, I> &&
    IndirectUnaryPredicate<Pred, projected<I, Proj>>,
    difference_type_t<I>>
count_if(I first, S last, Pred pred, Proj proj = Proj{})
{
    difference_type_t<I> counter = 0;

    while (first != last) {
        if (nano::invoke(pred, nano::invoke(proj, *first)) != false) {
            ++counter;
        }
        ++first;
    }

    return counter;
}

template <typename Rng, typename Proj = identity, typename Pred>
constexpr
std::enable_if_t<
    InputRange<Rng> &&
    IndirectUnaryPredicate<Pred, projected<iterator_t<Rng>, Proj>>,
    difference_type_t<iterator_t<Rng>>>
count_if(Rng&& rng, Pred pred, Proj proj = Proj{})
{
    return count_if(nano::begin(rng), nano::end(rng), std::move(pred), std::move(proj));
}

// [range.mismatch]

// FIXME: Use tagged pair

// N.B Three-legged forms are deprecated
template <typename I1, typename S1, typename I2,
          typename Proj1 = identity, typename Proj2 = identity,
          typename Pred = equal_to<>>
constexpr
std::enable_if_t<
    InputIterator<I1> &&
    Sentinel<S1, I1> &&
    InputIterator<I2> &&
    IndirectRelation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
    std::pair<I1, I2>>
mismatch(I1 first1, S1 last1, I2 first2, Pred pred = Pred{},
         Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{})
{
    while (first1 != last1 &&
           nano::invoke(pred, nano::invoke(proj1, *first1), nano::invoke(proj2, *first2)))
    {
        ++first1; ++first2;
    }

    return {first1, first2};
}

template <typename Rng1, typename I2, typename Proj1 = identity,
          typename Proj2 = identity, typename Pred = equal_to<>>
constexpr
std::enable_if_t<
    InputRange<Rng1> &&
    InputIterator<I2> &&
    IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>, projected<I2, Proj2>>,
    std::pair<safe_iterator_t<Rng1>, I2>>
mismatch(Rng1&& rng1, I2 first2, Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{})
{
    return nano::mismatch(
            nano::begin(rng1), nano::end(rng1), std::move(first2),
            std::move(pred), std::move(proj1), std::move(proj2));
}

template <typename I1, typename S1, typename I2, typename S2,
          typename Proj1 = identity, typename Proj2 = identity,
          typename Pred = equal_to<>>
constexpr
std::enable_if_t<
    InputIterator<I1> &&
    Sentinel<S1, I1> &&
    InputIterator<I2> &&
    Sentinel<S2, I2> &&
    IndirectRelation<Pred, projected<I1, Proj1>, projected<I2, Proj2>>,
    std::pair<I1, I2>>
mismatch(I1 first1, S1 last1, I2 first2, S2 last2,
         Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{})
{
    while (first1 != last1 && first2 != last2 &&
           nano::invoke(pred, nano::invoke(proj1, *first1), nano::invoke(proj2, *first2)))
    {
        ++first1; ++first2;
    }

    return {first1, first2};
}

template <typename Rng1, typename Rng2, typename Proj1 = identity,
          typename Proj2 = identity, typename Pred = equal_to<>>
constexpr
std::enable_if_t<
    InputRange<Rng1> &&
    InputRange<Rng2> &&
    IndirectRelation<Pred, projected<iterator_t<Rng1>, Proj1>, projected<iterator_t<Rng2>, Proj2>>,
    std::pair<safe_iterator_t<Rng1>, safe_iterator_t<Rng2>>>
mismatch(Rng1&& rng1, Rng2&& rng2, Pred pred = Pred{}, Proj1 proj1 = Proj1{}, Proj2 proj2 = Proj2{})
{
    return nano::mismatch(
            nano::begin(rng1), nano::end(rng1),
            nano::begin(rng2), nano::end(rng2),
            std::move(pred), std::move(proj1), std::move(proj2));
}

} // inline namespace ranges
} // namespace nano

#endif // NANORANGE_CONCEPTS_HPP_INCLUDED
