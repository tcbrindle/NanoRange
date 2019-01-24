// nanorange/detail/functional/identity.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_FUNCTIONAL_IDENTITY_HPP_INCLUDED
#define NANORANGE_DETAIL_FUNCTIONAL_IDENTITY_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <type_traits>
#include <utility>

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
