
#pragma once

#include "catch.hpp"

template <typename T>
T& as_lvalue(T&& t)
{
    return t;
}

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

namespace nano {
inline namespace ranges {
inline namespace ext {

template <typename I, typename S>
struct iterator_range {
    I first;
    S last;

    I begin() const { return first; }
    S end() const { return last; }
};

template <typename I, typename S>
iterator_range<I, S> make_range(I i, S s)
{
    return {std::move(i), std::move(s)};
}

}
}
}
