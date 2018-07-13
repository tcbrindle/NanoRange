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
//

#define IS_HEAP_UNTIL_2
#include "./is_heap_until.hpp"

TEST_CASE("alg.is_heap_until2")
{
    test();
    test_pred();

    // Test projections:
    S i185[] = {S{1}, S{0}, S{0}, S{0}, S{0}, S{0}, S{1}};
    auto is_heap_until = make_testable_1([](auto&&... args) {
        return stl2::is_heap_until(std::forward<decltype(args)>(args)...);
    });
    is_heap_until(i185, i185+7, std::greater<int>(), &S::i)
            .check([&](S *r){ CHECK(r == i185+1); });

    // Test rvalue range
#ifdef HAVE_RVALUE_RANGES
    auto res = stl2::is_heap_until(std::move(i185), std::greater<int>(), &S::i);
    CHECK(res.get_unsafe() == i185+1);
#endif
}