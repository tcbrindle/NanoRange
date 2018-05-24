
#pragma once

#include "catch.hpp"

template <typename Rng, typename Rng2>
void check_equal_(Rng && actual, Rng2&& expected)
{
    auto begin0 = nano::begin(actual);
    auto end0 = nano::end(actual);
    auto begin1 = nano::begin(expected);
    auto end1 = nano::end(expected);
    for(; begin0 != end0 && begin1 != end1; ++begin0, ++begin1)
        CHECK(*begin0 == *begin1);
    CHECK(begin0 == end0);
    CHECK(begin1 == end1);
}

template <typename Val, typename Rng>
void check_equal(Rng && actual, std::initializer_list<Val> expected)
{
    check_equal_(actual, expected);
}

template <typename Rng, typename Rng2>
void check_equal(Rng && actual, Rng2&& expected)
{
    check_equal_(actual, expected);
}