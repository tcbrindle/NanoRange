// nanorange/random.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANDOM_HPP_INCLUDED
#define NANORANGE_RANDOM_HPP_INCLUDED

#include <nanorange/concepts.hpp>

NANO_BEGIN_NAMESPACE

//  [rand.req.urng]

namespace detail {

struct UniformRandomBitGenerator_req {
    template <typename G>
    auto requires_() -> decltype(valid_expr(
        G::min(),
        requires_expr<Same<decltype(G::min()), invoke_result_t<G&>>>{},
        G::max(),
        requires_expr<Same<decltype(G::max()), invoke_result_t<G&>>>{}));
};

} // namespace detail

template <typename G>
NANO_CONCEPT UniformRandomBitGenerator =
    Invocable<G&>&& UnsignedIntegral<detail::checked_invoke_result_t<G&>>&&
        detail::requires_<detail::UniformRandomBitGenerator_req, G>;

NANO_END_NAMESPACE

#endif
