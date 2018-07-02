// clang-format off
#ifndef NANORANGE_DETAIL_CONCEPTS_CONCEPT_DEF_HPP_INCLUDED
#define NANORANGE_DETAIL_CONCEPTS_CONCEPT_DEF_HPP_INCLUDED

// Taken from pushmi library by Facebook Research
// https://github.com/facebookresearch/pushmi
//
// Copyright (c) 2018-present, Facebook, Inc.
//
// This source code is licensed under the MIT license found in the
// LICENSE-MIT.txt file in the root directory of this source tree.

#include <type_traits>

// disable buggy compatibility warning about "requires" and "concept" being
// C++20 keywords.
#if defined(__clang__)
#define NANO_PP_IGNORE_CXX2A_COMPAT_BEGIN \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wunknown-pragmas\"") \
    _Pragma("GCC diagnostic ignored \"-Wpragmas\"") \
    _Pragma("GCC diagnostic ignored \"-Wc++2a-compat\"") \
    _Pragma("GCC diagnostic ignored \"-Wfloat-equal\"") \
    /**/
#define NANO_PP_IGNORE_CXX2A_COMPAT_END \
    _Pragma("GCC diagnostic pop")
#else
#define NANO_PP_IGNORE_CXX2A_COMPAT_BEGIN
#define NANO_PP_IGNORE_CXX2A_COMPAT_END
// #pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wunknown-pragmas"
// #pragma GCC diagnostic ignored "-Wpragmas"
// #pragma GCC diagnostic ignored "-Wc++2a-compat"
#endif

NANO_PP_IGNORE_CXX2A_COMPAT_BEGIN

#if __cpp_inline_variables >= 201606
#define NANO_INLINE_VAR inline
#else
#define NANO_INLINE_VAR
#endif

#ifdef __clang__
#define NANO_PP_IS_SAME(...) __is_same(__VA_ARGS__)
#elif defined(__GNUC__) && __GNUC__ >= 6
#define NANO_PP_IS_SAME(...) __is_same_as(__VA_ARGS__)
#else
#define NANO_PP_IS_SAME(...) std::is_same<__VA_ARGS__>::value
#endif

#if __COUNTER__ != __COUNTER__
#define NANO_COUNTER __COUNTER__
#else
#define NANO_COUNTER __LINE__
#endif

#define NANO_PP_CHECK(...) NANO_PP_CHECK_N(__VA_ARGS__, 0,)
#define NANO_PP_CHECK_N(x, n, ...) n
#define NANO_PP_PROBE(x) x, 1,

// NANO_CXX_VA_OPT
#ifndef NANO_CXX_VA_OPT
#if __cplusplus > 201703L
#define NANO_CXX_VA_OPT_(...) NANO_PP_CHECK(__VA_OPT__(,) 1)
#define NANO_CXX_VA_OPT NANO_CXX_VA_OPT_(~)
#else
#define NANO_CXX_VA_OPT 0
#endif
#endif // NANO_CXX_VA_OPT

#define NANO_PP_CAT_(X, ...)  X ## __VA_ARGS__
#define NANO_PP_CAT(X, ...)   NANO_PP_CAT_(X, __VA_ARGS__)
#define NANO_PP_CAT2_(X, ...) X ## __VA_ARGS__
#define NANO_PP_CAT2(X, ...)  NANO_PP_CAT2_(X, __VA_ARGS__)

#define NANO_PP_EVAL(X, ...) X(__VA_ARGS__)
#define NANO_PP_EVAL2(X, ...) X(__VA_ARGS__)
#define NANO_PP_EVAL3(X, ...) X(__VA_ARGS__)
#define NANO_PP_EVAL4(X, ...) X(__VA_ARGS__)

#define NANO_PP_EXPAND(...) __VA_ARGS__
#define NANO_PP_EAT(...)

#define NANO_PP_IS_PAREN(x) NANO_PP_CHECK(NANO_PP_IS_PAREN_PROBE x)
#define NANO_PP_IS_PAREN_PROBE(...) NANO_PP_PROBE(~)

#define NANO_PP_COUNT(...)                                                   \
    NANO_PP_COUNT_(__VA_ARGS__,                                              \
        50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,           \
        30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,           \
        10,9,8,7,6,5,4,3,2,1,)                                                 \
        /**/
#define NANO_PP_COUNT_(                                                      \
    _1, _2, _3, _4, _5, _6, _7, _8, _9, _10,                                   \
    _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,                          \
    _21, _22, _23, _24, _25, _26, _27, _28, _29, _30,                          \
    _31, _32, _33, _34, _35, _36, _37, _38, _39, _40,                          \
    _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, N, ...)                  \
    N                                                                          \
    /**/

#define NANO_PP_IS_EQUAL(X,Y) \
    NANO_PP_CHECK(NANO_PP_CAT(NANO_PP_CAT(NANO_PP_IS_EQUAL_, X), Y))
#define NANO_PP_IS_EQUAL_00 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_11 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_22 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_33 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_44 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_55 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_66 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_77 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_88 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_99 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1010 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1111 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1212 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1313 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1414 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1515 NANO_PP_PROBE(~)
#define NANO_PP_IS_EQUAL_1616 NANO_PP_PROBE(~)

#define NANO_PP_NOT(X) NANO_PP_CAT(NANO_PP_NOT_, X)
#define NANO_PP_NOT_0 1
#define NANO_PP_NOT_1 0

#define NANO_PP_IIF(BIT) NANO_PP_CAT_(NANO_PP_IIF_, BIT)
#define NANO_PP_IIF_0(TRUE, ...) __VA_ARGS__
#define NANO_PP_IIF_1(TRUE, ...) TRUE
#define NANO_PP_FIRST(X, ...) X
#define NANO_PP_SECOND(X, ...) __VA_ARGS__

#define NANO_PP_OR(X,Y) \
    NANO_PP_NOT(NANO_PP_CHECK(NANO_PP_CAT(                               \
        NANO_PP_CAT(NANO_PP_NOR_, X), Y)))
#define NANO_PP_NOR_00 NANO_PP_PROBE(~)

#if NANO_CXX_VA_OPT

#define NANO_PP_IS_EMPTY_NON_FUNCTION(...) \
    NANO_PP_CHECK(__VA_OPT__(, 0), 1)

#else // RANGES_VA_OPT

#define NANO_PP_SPLIT(i, ...)                                                \
    NANO_PP_CAT_(NANO_PP_SPLIT_, i)(__VA_ARGS__)                           \
    /**/
#define NANO_PP_SPLIT_0(a, ...) a
#define NANO_PP_SPLIT_1(a, ...) __VA_ARGS__

#define NANO_PP_IS_VARIADIC(...)                                             \
    NANO_PP_SPLIT(                                                           \
        0,                                                                     \
        NANO_PP_CAT(                                                         \
            NANO_PP_IS_VARIADIC_R_,                                          \
            NANO_PP_IS_VARIADIC_C __VA_ARGS__))                              \
    /**/
#define NANO_PP_IS_VARIADIC_C(...) 1
#define NANO_PP_IS_VARIADIC_R_1 1,
#define NANO_PP_IS_VARIADIC_R_NANO_PP_IS_VARIADIC_C 0,

// emptiness detection macro...

#define NANO_PP_IS_EMPTY_NON_FUNCTION(...)                                   \
    NANO_PP_IIF(NANO_PP_IS_VARIADIC(__VA_ARGS__))                          \
    (0, NANO_PP_IS_VARIADIC(NANO_PP_IS_EMPTY_NON_FUNCTION_C __VA_ARGS__()))\
    /**/
#define NANO_PP_IS_EMPTY_NON_FUNCTION_C() ()

#endif // NANO_PP_VA_OPT

#define NANO_PP_EMPTY()
#define NANO_PP_COMMA() ,
#define NANO_PP_COMMA_IIF(X)                                                 \
    NANO_PP_IIF(X)(NANO_PP_EMPTY, NANO_PP_COMMA)()

#define NANO_CONCEPT_ASSERT(...)                                             \
    static_assert((bool) (__VA_ARGS__),                                        \
        "Concept assertion failed : " #__VA_ARGS__)

////////////////////////////////////////////////////////////////////////////////
// NANO_CONCEPT_DEF
//   For defining concepts with a syntax symilar to C++20. For example:
//
//     NANO_CONCEPT_DEF(
//         // The Assignable concept from the C++20
//         template(class T, class U)
//         concept Assignable,
//             requires (T t, U &&u) (
//                 t = (U &&) u,
//                 ::nano::concepts::requires_<Same<decltype(t = (U &&) u), T>>
//             ) &&
//             std::is_lvalue_reference_v<T>
//     );
#define NANO_CONCEPT_DEF(DECL, ...)                                          \
    NANO_PP_EVAL(                                                            \
        NANO_PP_DECL_DEF,                                                    \
        NANO_PP_CAT(NANO_PP_DEF_DECL_, DECL),                              \
        __VA_ARGS__)                                                           \
    /**/
#define NANO_PP_DECL_DEF_NAME(...)                                           \
    __VA_ARGS__,                                                               \
    /**/
#define NANO_PP_DECL_DEF(TPARAM, NAME, ...)                                  \
    NANO_PP_CAT(NANO_PP_DECL_DEF_, NANO_PP_IS_PAREN(NAME))(              \
        TPARAM,                                                                \
        NAME,                                                                  \
        __VA_ARGS__)                                                           \
    /**/
// The defn is of the form:
//   template(class A, class B = void, class... Rest)
//   (concept Name)(A, B, Rest...),
//      // requirements...
#define NANO_PP_DECL_DEF_1(TPARAM, NAME, ...)                                \
    NANO_PP_EVAL4(                                                           \
        NANO_PP_DECL_DEF_IMPL,                                               \
        TPARAM,                                                                \
        NANO_PP_DECL_DEF_NAME NAME,                                          \
        __VA_ARGS__)                                                           \
    /**/
// The defn is of the form:
//   template(class A, class B)
//   concept Name,
//      // requirements...
// Compute the template arguments (A, B) from the template introducer.
#define NANO_PP_DECL_DEF_0(TPARAM, NAME, ...)                                \
    NANO_PP_DECL_DEF_IMPL(                                                   \
        TPARAM,                                                                \
        NAME,                                                                  \
        (NANO_PP_CAT(NANO_PP_AUX_, TPARAM)),                               \
        __VA_ARGS__)                                                           \
    /**/
// Expand the template definition into a struct and template alias like:
//    struct NameConcept {
//      template<class A, class B>
//      static auto _concept_requires_(/* args (optional)*/) ->
//          decltype(/*requirements...*/);
//      template<class A, class B>
//      static constexpr auto is_satisfied_by(int) ->
//          decltype(bool(&_concept_requires_<A,B>)) { return true; }
//      template<class A, class B>
//      static constexpr bool is_satisfied_by(long) { return false; }
//    };
//    template<class A, class B>
//    inline constexpr bool Name = NameConcept::is_satisfied_by<A, B>(0);
#if __cpp_concepts
// No requires expression
#define NANO_PP_DEF_IMPL_0(...)                                              \
    __VA_ARGS__                                                                \
    /**/
// Requires expression
#define NANO_PP_DEF_IMPL_1(...)                                              \
    NANO_PP_CAT(NANO_PP_DEF_IMPL_1_, __VA_ARGS__)                          \
    /**/
#define NANO_PP_DEF_IMPL_1_requires                                          \
    requires NANO_PP_DEF_IMPL_1_REQUIRES                                     \
    /**/
#define NANO_PP_DEF_IMPL_1_REQUIRES(...)                                     \
    (__VA_ARGS__) NANO_PP_DEF_IMPL_1_REQUIRES_BODY                           \
    /**/
#define NANO_PP_DEF_IMPL_1_REQUIRES_BODY(...)                                \
    { __VA_ARGS__; }                                                           \
    /**/
#define NANO_PP_DECL_DEF_IMPL(TPARAM, NAME, ARGS, ...)                       \
    inline namespace nano_concept_eager {                                    \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        concept bool NANO_PP_CAT(NANO_PP_DEF_, NAME) = NANO_PP_EVAL2(    \
            NANO_PP_DEF_IMPL(__VA_ARGS__),                                   \
            __VA_ARGS__);                                                      \
    }                                                                          \
    namespace defer = nano_concept_eager;                                    \
    namespace lazy {                                                           \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        struct NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept) {   \
            using Concept =                                                    \
                NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept);   \
            explicit constexpr operator bool() const noexcept {                \
                return (bool) defer::NANO_PP_CAT(NANO_PP_DEF_, NAME)<      \
                    NANO_PP_EXPAND ARGS>;                                    \
            }                                                                  \
            constexpr auto operator!() const noexcept {                        \
                return ::nano::ranges::concepts::detail::Not<Concept>{};             \
            }                                                                  \
            template <class That>                                              \
            constexpr auto operator&&(That) const noexcept {                   \
                return ::nano::ranges::concepts::detail::And<Concept, That>{};       \
            }                                                                  \
        };                                                                     \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        NANO_INLINE_VAR constexpr auto NANO_PP_CAT(NANO_PP_DEF_, NAME) = \
            NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept)        \
                <NANO_PP_EXPAND ARGS>{};                                     \
    }                                                                          \
    /**/
#else
// No requires expression:
#define NANO_PP_DEF_IMPL_0(...)                                              \
    () -> std::enable_if_t<bool(__VA_ARGS__), int>                             \
    /**/
// Requires expression:
#define NANO_PP_DEF_IMPL_1(...)                                              \
    NANO_PP_CAT(NANO_PP_DEF_IMPL_1_, __VA_ARGS__) ), int>                  \
    /**/
#define NANO_PP_DEF_IMPL_1_requires                                          \
    NANO_PP_DEF_IMPL_1_REQUIRES                                              \
    /**/
#define NANO_PP_DEF_IMPL_1_REQUIRES(...)                                     \
    (__VA_ARGS__) -> std::enable_if_t<bool(                                    \
        ::nano::ranges::concepts::detail::requires_  NANO_PP_DEF_REQUIRES_BODY     \
    /**/
 #define NANO_PP_DEF_REQUIRES_BODY(...)                                      \
    <decltype(__VA_ARGS__, void())>()                                          \
    /**/
#define NANO_PP_DECL_DEF_IMPL(TPARAM, NAME, ARGS, ...)                       \
    struct NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept) {       \
        using Concept =                                                        \
            NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept);       \
        NANO_PP_IGNORE_CXX2A_COMPAT_BEGIN                                    \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        static auto _concept_requires_ NANO_PP_EVAL2(                        \
            NANO_PP_DEF_IMPL(__VA_ARGS__),                                   \
            __VA_ARGS__);                                                      \
        NANO_PP_IGNORE_CXX2A_COMPAT_END                                      \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        struct _is_satisfied_by_ {                                             \
            template <class C_ = Concept>                                      \
            static constexpr decltype(                                         \
                !&C_::template _concept_requires_<NANO_PP_EXPAND ARGS>)      \
            impl(int) noexcept { return true; }                                \
            static constexpr bool impl(long) noexcept { return false; }        \
            explicit constexpr operator bool() const noexcept {                \
                return _is_satisfied_by_::impl(0);                             \
            }                                                                  \
            constexpr auto operator!() const noexcept {                        \
                return ::nano::ranges::concepts::detail::Not<_is_satisfied_by_>{};   \
            }                                                                  \
            template <class That>                                              \
            constexpr auto operator&&(That) const noexcept {                   \
                return ::nano::ranges::concepts::detail::And<                        \
                    _is_satisfied_by_, That>{};                                \
            }                                                                  \
        };                                                                     \
    };                                                                         \
    NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                      \
    NANO_INLINE_VAR constexpr bool NANO_PP_CAT(NANO_PP_DEF_, NAME) =     \
        (bool)NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept)      \
            ::_is_satisfied_by_<NANO_PP_EXPAND ARGS>{};                      \
    namespace lazy {                                                           \
        NANO_PP_CAT(NANO_PP_DEF_, TPARAM)                                  \
        NANO_INLINE_VAR constexpr auto NANO_PP_CAT(NANO_PP_DEF_, NAME) = \
            NANO_PP_CAT(NANO_PP_CAT(NANO_PP_DEF_, NAME), Concept)        \
                ::_is_satisfied_by_<NANO_PP_EXPAND ARGS>{};                  \
    }                                                                          \
    namespace defer = lazy;                                                    \
    /**/
#endif

#define NANO_PP_REQUIRES_PROBE_requires                                      \
    NANO_PP_PROBE(~)                                                         \
    /**/
#define NANO_PP_DEF_IMPL(REQUIRES, ...)                                      \
    NANO_PP_CAT(                                                             \
        NANO_PP_DEF_IMPL_,                                                   \
        NANO_PP_CHECK(NANO_PP_CAT(NANO_PP_REQUIRES_PROBE_, REQUIRES)))   \
    /**/
#define NANO_PP_DEF_DECL_template(...)                                       \
    template(__VA_ARGS__),                                                     \
    /**/
#define NANO_PP_DEF_template(...)                                            \
    template<__VA_ARGS__>                                                      \
    /**/
#define NANO_PP_DEF_concept
#define NANO_PP_DEF_class
#define NANO_PP_DEF_typename
#define NANO_PP_DEF_int
#define NANO_PP_DEF_bool
#define NANO_PP_DEF_size_t
#define NANO_PP_DEF_unsigned
#define NANO_PP_AUX_template(...)                                            \
    NANO_PP_CAT2(                                                            \
        NANO_PP_TPARAM_,                                                     \
        NANO_PP_COUNT(__VA_ARGS__))(__VA_ARGS__)                             \
    /**/
#define NANO_PP_TPARAM_1(_1)                                                 \
    NANO_PP_CAT2(NANO_PP_DEF_, _1)
#define NANO_PP_TPARAM_2(_1, ...)                                            \
    NANO_PP_CAT2(NANO_PP_DEF_, _1), NANO_PP_TPARAM_1(__VA_ARGS__)
#define NANO_PP_TPARAM_3(_1, ...)                                            \
    NANO_PP_CAT2(NANO_PP_DEF_, _1), NANO_PP_TPARAM_2(__VA_ARGS__)
#define NANO_PP_TPARAM_4(_1, ...)                                            \
    NANO_PP_CAT2(NANO_PP_DEF_, _1), NANO_PP_TPARAM_3(__VA_ARGS__)
#define NANO_PP_TPARAM_5(_1, ...)                                            \
    NANO_PP_CAT2(NANO_PP_DEF_, _1), NANO_PP_TPARAM_4(__VA_ARGS__)

////////////////////////////////////////////////////////////////////////////////
// NANO_TEMPLATE
// Usage:
//   NANO_TEMPLATE (class A, class B)
//     (requires Concept1<A> && Concept2<B>)
//   void foo(A a, B b)
//   {}
// or
//   NANO_TEMPLATE (class A, class B)
//     (requires requires (expr1, expr2, expr3) && Concept1<A> && Concept2<B>)
//   void foo(A a, B b)
//   {}
#if __cpp_concepts
#define NANO_TEMPLATE(...)                                                   \
    template<__VA_ARGS__> NANO_TEMPLATE_AUX_                                 \
    /**/
#define NANO_TEMPLATE_AUX_(...)                                              \
    NANO_TEMPLATE_AUX_4(NANO_PP_CAT(NANO_TEMPLATE_AUX_3_, __VA_ARGS__))  \
    /**/
#define NANO_TEMPLATE_AUX_3_requires
#define NANO_TEMPLATE_AUX_4(...)                                             \
    NANO_TEMPLATE_AUX_5(__VA_ARGS__)(__VA_ARGS__)                            \
    /**/
#define NANO_TEMPLATE_AUX_5(REQUIRES, ...)                                   \
    NANO_PP_CAT(                                                             \
        NANO_TEMPLATE_AUX_5_,                                                \
        NANO_PP_CHECK(NANO_PP_CAT(NANO_PP_REQUIRES_PROBE_, REQUIRES)))   \
    /**/
// No requires expression:
#define NANO_TEMPLATE_AUX_5_0(...)                                           \
    requires __VA_ARGS__                                                       \
    /**/
// Requires expression
#define NANO_TEMPLATE_AUX_5_1(...)                                           \
    NANO_PP_CAT(NANO_TEMPLATE_AUX_6_, __VA_ARGS__)                         \
    /**/
#define NANO_TEMPLATE_AUX_6_requires(...)\
    requires requires { __VA_ARGS__; }
#else
#define NANO_TEMPLATE(...)                                                   \
    template<__VA_ARGS__ NANO_TEMPLATE_AUX_
#define NANO_TEMPLATE_AUX_(...) ,                                            \
    int (*NANO_PP_CAT(_nano_concept_unique_, __LINE__))[                   \
        NANO_COUNTER] = nullptr,                                             \
    std::enable_if_t<NANO_PP_CAT(_nano_concept_unique_, __LINE__) ||       \
        bool(NANO_TEMPLATE_AUX_4(NANO_PP_CAT(                              \
            NANO_TEMPLATE_AUX_3_, __VA_ARGS__))), int> = 0>                  \
    /**/
#define NANO_TEMPLATE_AUX_3_requires
#define NANO_TEMPLATE_AUX_4(...)                                             \
    NANO_TEMPLATE_AUX_5(__VA_ARGS__)(__VA_ARGS__)                            \
    /**/
#define NANO_TEMPLATE_AUX_5(REQUIRES, ...)                                   \
    NANO_PP_CAT(                                                             \
        NANO_TEMPLATE_AUX_5_,                                                \
        NANO_PP_CHECK(NANO_PP_CAT(NANO_PP_REQUIRES_PROBE_, REQUIRES)))   \
    /**/
// No requires expression:
#define NANO_TEMPLATE_AUX_5_0(...)                                           \
    __VA_ARGS__                                                                \
    /**/
#define NANO_TEMPLATE_AUX_5_1(...)                                           \
    NANO_PP_CAT(NANO_TEMPLATE_AUX_6_, __VA_ARGS__)                         \
    /**/
#define NANO_TEMPLATE_AUX_6_requires(...)                                    \
    ::nano::ranges::concepts::detail::requires_<decltype(__VA_ARGS__)>()             \
    /**/
#endif


#if __cpp_concepts
#define NANO_BROKEN_SUBSUMPTION(...)
#define NANO_TYPE_CONSTRAINT(...) __VA_ARGS__
#else
#define NANO_BROKEN_SUBSUMPTION(...) __VA_ARGS__
#define NANO_TYPE_CONSTRAINT(...) class
#endif


#if __cpp_concepts
#define NANO_PP_CONSTRAINED_USING(REQUIRES, NAME, ...)                       \
    requires REQUIRES                                                          \
  using NAME __VA_ARGS__;                                                      \
  /**/
#else
#define NANO_PP_CONSTRAINED_USING(REQUIRES, NAME, ...)                       \
  using NAME std::enable_if_t<bool(REQUIRES), __VA_ARGS__>;                    \
  /**/
#endif

namespace nano {
inline namespace ranges {

template <bool B>
using bool_ = std::integral_constant<bool, B>;

namespace concepts {
namespace detail {
template <class>
inline constexpr bool requires_()
{
    return true;
}

template <class T, class U>
struct And;

template <class T>
struct Not {
    explicit constexpr operator bool() const noexcept
    {
        return !(bool) T{};
    }

    constexpr auto operator!() const noexcept
    {
        return T{};
    }

    template <class That>
    constexpr auto operator&&(That) const noexcept
    {
        return And<Not, That>{};
    }
};

template <class T, class U>
struct And {
    static constexpr bool impl(std::false_type) noexcept { return false; }

    static constexpr bool impl(std::true_type) noexcept { return (bool) U{}; }

    explicit constexpr operator bool() const noexcept
    {
        return And::impl(bool_<(bool) T{}>{});
    }

    constexpr auto operator!() const noexcept
    {
        return Not<And>{};
    }

    template <class That>
    constexpr auto operator&&(That) const noexcept
    {
        return detail::And<And, That>{};
    }
};
} // namespace detail
} // namespace concepts

template <class T>
constexpr bool implicitly_convertible_to(T)
{
    return true;
}

template <bool B>
NANO_INLINE_VAR constexpr std::enable_if_t<B, bool> requires_ = true;

}} // namespace nano::ranges

NANO_PP_IGNORE_CXX2A_COMPAT_END

#endif

