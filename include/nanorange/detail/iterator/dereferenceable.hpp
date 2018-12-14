// nanorange/detail/iterator/dereferenceable.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_DEREFERENCABLE_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_DEREFERENCABLE_HPP_INCLUDED

#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
using with_reference = T&;

struct CanReference_req {
    template <typename T>
    auto requires_() -> with_reference<T>;
};

template <typename T>
NANO_CONCEPT CanReference = requires_<CanReference_req, T>;

struct Dereferenceable_req {
    template <typename T>
    auto requires_(T& t)
        -> decltype(*t, requires_expr<CanReference<decltype(*t)>>{});
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
