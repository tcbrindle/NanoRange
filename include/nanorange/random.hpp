// nanorange/random.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANDOM_HPP_INCLUDED
#define NANORANGE_RANDOM_HPP_INCLUDED

#include <nanorange/concepts.hpp>

#include <random>

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

template <typename>
auto UniformRandomBitGenerator_fn(long) -> std::false_type;

template <typename G>
auto UniformRandomBitGenerator_fn(int) -> std::enable_if_t<
        Invocable<G&> &&
        UnsignedIntegral<invoke_result_t<G&>> &&
        requires_v<UniformRandomBitGenerator_req, G>,
    std::true_type>;

} // namespace detail

template <typename G>
NANO_CONCEPT UniformRandomBitGenerator =
    decltype(detail::UniformRandomBitGenerator_fn<G>(0))::value;

NANO_END_NAMESPACE

#endif
