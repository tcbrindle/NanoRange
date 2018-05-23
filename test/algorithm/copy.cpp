// Range v3 library
//
//  Copyright Eric Niebler 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <cstring>
#include <utility>
#include <algorithm>
#include <nanorange/algorithm/copy.hpp>

#include "../catch.hpp"

namespace ranges = nano;

TEST_CASE("alg.copy")
{
    using ranges::begin;
    using ranges::end;
    using ranges::size;

    std::pair<int, int> const a[] = {{0, 0}, {0, 1}, {1, 2}, {1, 3}, {3, 4}, {3, 5}};
    static_assert(size(a) == 6, "");
    std::pair<int, int> out[size(a)] = {};

    auto res = ranges::copy(begin(a), end(a), out);
    REQUIRE(res.first == end(a));
    REQUIRE(res.second == out + size(out));
#if HAVE_TAGGED_PAIR
    REQUIRE(&res.first == &res.in());
    REQUIRE(&res.second == &res.out());
#endif
    REQUIRE(std::equal(a, a + size(a), out));

    std::fill_n(out, size(out), std::make_pair(0, 0));
    REQUIRE(!std::equal(a, a + size(a), out));

    res = ranges::copy(a, out);
    REQUIRE(res.first == a + size(a));
    REQUIRE(res.second == out + size(out));
    REQUIRE(std::equal(a, a + size(a), out));

    std::fill_n(out, size(out), std::make_pair(0, 0));

#if 0
    using ranges::view::delimit;
    {
        char const *sz = "hello world";
        char buf[50];
        auto str = delimit(sz, '\0');
        auto res3 = ranges::copy(str, buf);
        *res3.second = '\0';
        REQUIRE(res3.first == std::next(begin(str), static_cast<std::ptrdiff_t>(std::strlen(sz))));
        REQUIRE(res3.second == buf + std::strlen(sz));
        REQUIRE(std::strcmp(sz, buf) == 0);
    }

    {
        char const *sz = "hello world";
        char buf[50];
        auto str = delimit(sz, '\0');
        auto res3 = ranges::copy(std::move(str), buf);
        *res3.second = '\0';
        REQUIRE(res3.first.get_unsafe() == std::next(begin(str), static_cast<std::ptrdiff_t>(std::strlen(sz))));
        REQUIRE(res3 == buf + std::strlen(sz));
        REQUIRE(std::strcmp(sz, buf) == 0);
    }
#endif
}
