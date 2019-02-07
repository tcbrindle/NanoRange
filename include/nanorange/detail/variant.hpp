// nanorange/detail/variant.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_VARIANT_HPP_INCLUDED
#define NANORANGE_DETAIL_VARIANT_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <cassert>

#ifdef NANO_HAVE_CPP17
// MSVC is fine (for all versions we support, at least)
#if defined(_MSC_VER)
#define NANO_USE_STD_VARIANT
#endif

// libstdc++ < 8 is insufficiently constexpr
#if defined(_GLIBCXX_RELEASE) && (_GLIBCXX_RELEASE >= 8)
#define NANO_USE_STD_VARIANT
#endif

// AppleClang's libc++ lies about C++17 support
#if defined(_LIBCXX_VERSION) && !defined(__apple_build_version__)
#define NANO_USE_STD_VARIANT
#endif

#endif

#ifdef NANO_USE_STD_VARIANT
#include <variant>
#else
#include <nanorange/detail/thirdparty/mpark_variant.hpp>
#endif

NANO_BEGIN_NAMESPACE

namespace detail {

#ifdef NANO_USE_STD_VARIANT
using std::variant;
using std::in_place_type;
using std::in_place_index;
using std::get_if;
using std::visit;
using std::holds_alternative;
#else
using mpark::variant;
using mpark::in_place_type;
using mpark::in_place_index;
using mpark::get_if;
using mpark::visit;
using mpark::holds_alternative;
#endif

template <typename T, typename... Ts>
constexpr decltype(auto) unsafe_get(variant<Ts...>& v) noexcept
{
    assert(holds_alternative<T>(v));
    return *detail::get_if<T>(&v);
}

template <typename T, typename... Ts>
constexpr decltype(auto) unsafe_get(const variant<Ts...>& v) noexcept
{
    assert(holds_alternative<T>(v));
    return *detail::get_if<T>(&v);
}


}

NANO_END_NAMESPACE

#endif
