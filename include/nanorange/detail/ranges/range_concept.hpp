// nanorange/detail/ranges/range_concept.hpp
//
// Copyright (c) 2020 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_RANGE_CONCEPT_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_RANGE_CONCEPT_HPP_INCLUDED

#include <nanorange/detail/ranges/begin_end.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct range_concept {
    template <typename T>
    auto requires_(T& t) -> decltype(ranges::begin(t), ranges::end(t));
};

} // namespace detail

template <typename T>
NANO_CONCEPT range = detail::requires_<detail::range_concept, T>;

NANO_END_NAMESPACE

#endif
