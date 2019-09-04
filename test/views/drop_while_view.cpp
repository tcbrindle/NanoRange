// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter
//  Copyright Eric Niebler
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/views/drop_while.hpp>
#include <nanorange/views/iota.hpp>
#include <nanorange/views/take.hpp>
#include <list>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;
namespace views = ranges::views;

TEST_CASE("views.drop_while")
{
    {
        auto rng0 = views::iota(10) | views::drop_while([](int i) { return i < 25; });
        static_assert(ranges::random_access_range<decltype(rng0)>);
        static_assert(!ranges::common_range<decltype(rng0)>);
        auto b = rng0.begin();
        CHECK(*b == 25);
        CHECK(*(b+1) == 26);
        ::check_equal(rng0 | views::take(10), {25, 26, 27, 28, 29, 30, 31, 32, 33, 34});
    }

    std::list<int> vi{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto rng1 = vi | views::drop_while([](int i) { return i != 50; });
    static_assert(ranges::bidirectional_range<decltype(rng1)>);
    static_assert(ranges::common_range<decltype(rng1)>);
    CHECK(rng1.begin() == rng1.end());

#if 0 // Mutable predicates don't seem to be supported?
    // Check with a mutable predicate
    {
        auto rng0 = views::iota(10) | views::drop_while([b = true](int i) mutable { b = !b; return i < 25; });
        //  static_assert(range_cardinality<decltype(rng0)>::value == unknown);
        static_assert(ranges::random_access_range<decltype(rng0)>);
        static_assert(!ranges::common_range<decltype(rng0)>);
        auto b = rng0.begin();
        CHECK(*b == 25);
        CHECK(*(b+1) == 26);
        ::check_equal(rng0 | views::take(10), {25, 26, 27, 28, 29, 30, 31, 32, 33, 34});
    }
#endif
}