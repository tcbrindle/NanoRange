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

#include <nanorange/algorithm/adjacent_find.hpp>
#include "../catch.hpp"

namespace ranges = nano::ranges;

TEST_CASE("alg.adjacent_find")
{
	int v1[] = { 0, 2, 2, 4, 6 };
	REQUIRE(ranges::adjacent_find(ranges::begin(v1), ranges::end(v1)) == &v1[1]);
	REQUIRE(ranges::adjacent_find(v1) == &v1[1]);

	std::pair<int, int> v2[] = {{0, 0}, {0, 2}, {0, 2}, {0, 4}, {0, 6}};
	REQUIRE(ranges::adjacent_find(ranges::begin(v2), ranges::end(v2),
			ranges::equal_to<>{}, &std::pair<int, int>::second) == &v2[1]);
	REQUIRE(ranges::adjacent_find(v2, ranges::equal_to<>{}, &std::pair<int, int>::second) == &v2[1]);
	static_assert(std::is_same<std::pair<int,int>*,
							   decltype(ranges::adjacent_find(v2, ranges::equal_to<>{},
									&std::pair<int, int>::second))>::value, "");

	auto l = {0, 2, 2, 4, 6};
	CHECK(ranges::adjacent_find(std::move(l)).get_unsafe()[2] == 4);
}
