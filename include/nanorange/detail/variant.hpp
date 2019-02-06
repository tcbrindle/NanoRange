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

NANO_BEGIN_NAMESPACE

namespace detail {

#ifdef NANO_HAVE_CPP17
using std::variant;
#else
using mpark::variant;
#endif

}

NANO_END_NAMESPACE

#endif
