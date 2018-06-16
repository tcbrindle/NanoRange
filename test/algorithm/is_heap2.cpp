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

#define IS_HEAP_2
#include "./is_heap.hpp"

TEST_CASE("alg.is_heap2")
{
    test();
    test_comp();

    // Test projections:
    S i183[] = {S{0}, S{1}, S{1}, S{1}, S{1}, S{1}, S{1}};
    CHECK(stl2::is_heap(i183, i183+7, std::greater<int>(), &S::i));
}