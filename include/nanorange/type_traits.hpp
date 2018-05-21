// nanorange/type_traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_TYPE_TRAITS_HPP_INCLUDED
#define NANORANGE_TYPE_TRAITS_HPP_INCLUDED

#include <nanorange/detail/common_reference.hpp>

NANO_BEGIN_NAMESPACE

using std::common_type;
using std::common_type_t;

namespace detail {

template <typename... T>
using checked_common_type_t = test_t<common_type_t, T...>;
}

NANO_END_NAMESPACE

#endif
