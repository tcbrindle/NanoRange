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

struct uniform_random_bit_generator_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename G>
    static auto test(int) -> std::enable_if_t<
        invocable<G&> &&
        unsigned_integral<invoke_result_t<G&>> &&
        detail::requires_<uniform_random_bit_generator_concept, G>,
        std::true_type>;


    template <typename G>
    auto requires_() -> decltype(
        requires_expr<same_as<decltype(G::min()), invoke_result_t<G&>>>{},
        requires_expr<same_as<decltype(G::max()), invoke_result_t<G&>>>{}
    );
};

} // namespace detail

template <typename G>
NANO_CONCEPT uniform_random_bit_generator =
    decltype(detail::uniform_random_bit_generator_concept::test<G>(0))::value;

NANO_END_NAMESPACE

#endif
