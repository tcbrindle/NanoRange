// nanorange/detail/variant.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_VARIANT_HPP_INCLUDED
#define NANORANGE_DETAIL_VARIANT_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#ifdef NANO_HAVE_CPP17
#include <variant>
#else
#include <nanorange/detail/thirdparty/mpark_variant.hpp>
#endif

#include <cassert>

NANO_BEGIN_NAMESPACE

namespace detail {

#ifdef NANO_HAVE_CPP17
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
