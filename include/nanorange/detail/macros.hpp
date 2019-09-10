// nanorange/detail/macros.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MACROS_HPP_INCLUDED
#define NANORANGE_DETAIL_MACROS_HPP_INCLUDED

#include <ciso646>

#ifdef NANORANGE_NO_DEPRECATION_WARNINGS
#define NANO_DEPRECATED
#define NANO_DEPRECATED_FOR(x)
#else
#define NANO_DEPRECATED [[deprecated]]
#define NANO_DEPRECATED_FOR(x) [[deprecated(x)]]
#endif

#ifdef __has_cpp_attribute
#if __has_cpp_attribute(no_unique_address) >= 201803L
#define NANO_NO_UNIQUE_ADDRESS [[no_unique_address]]
#else
#define NANO_NO_UNIQUE_ADDRESS
#endif // __has_cpp_attribute(no_unique_address)
#else
#define NANO_NO_UNIQUE_ADDRESS
#endif // defined(__has_cpp_attribute)

#define NANO_CONCEPT inline constexpr bool

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

#define NANO_INLINE_VAR(type, name)                                            \
    inline namespace function_objects {                                        \
    inline constexpr type name{};                                              \
    }

#if defined(_LIBCPP_VERSION)
#define NANO_BEGIN_NAMESPACE_STD _LIBCPP_BEGIN_NAMESPACE_STD
#define NANO_END_NAMESPACE_STD _LIBCPP_END_NAMESPACE_STD
#elif defined(_MSVC_STL_VERSION)
#define NANO_BEGIN_NAMESPACE_STD _STD_BEGIN
#define NANO_END_NAMESPACE_STD _STD_END
#elif defined(_GLIBCXX_DEBUG)
#ifndef NANORANGE_NO_STD_FORWARD_DECLARATIONS
#define NANORANGE_NO_STD_FORWARD_DECLARATIONS
#endif
#else
#define NANO_BEGIN_NAMESPACE_STD namespace std {
#define NANO_END_NAMESPACE_STD }
#endif

#if defined(_MSC_VER)
#define NANO_MSVC_LAMBDA_PIPE_WORKAROUND 1
#endif

#endif
