// nanorange/algorithm/clamp.hpp
//
// Copyright (c) 2020 Boris Staletic (boris dot staletic at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_CLAMP_HPP_INCLUDED
#define NANORANGE_ALGORITHM_CLAMP_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct clamp_fn {
    template <typename T, typename Proj = identity, typename Comp = nano::less>
    [[nodiscard]] constexpr std::enable_if_t<indirect_strict_weak_order<Comp, projected<const T*, Proj>>, const T&>
    operator()(const T& value, const T& low, const T& high, Comp comp = {}, Proj proj = Proj{}) const
    {
        auto&& projected_value = nano::invoke(proj, value);
        if (nano::invoke(comp, projected_value, nano::invoke(proj, low))) {
            return low;
        } else if (nano::invoke(comp, nano::invoke(proj, high), projected_value)) {
            return high;
        } else {
            return value;
        }
    }
};
} // namespace detail

NANO_INLINE_VAR(detail::clamp_fn, clamp)

NANO_END_NAMESPACE

#endif
