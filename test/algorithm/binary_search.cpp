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
//  Copyright 2005 - 2007 Adobe Systems Incorporated
//  Distributed under the MIT License(see accompanying file LICENSE_1_0_0.txt
//  or a copy at http://stlab.adobe.com/licenses.html)

#include <nanorange.hpp>
#include <utility>
#include "../catch.hpp"

namespace ranges = nanorange;

TEST_CASE("alg.binary_search")
{
	using ranges::begin;
	using ranges::end;
	using ranges::less;

	std::pair<int, int> a[] = {{0, 0}, {0, 1}, {1, 2}, {1, 3}, {3, 4}, {3, 5}};
	const std::pair<int, int> c[] = {{0, 0}, {0, 1}, {1, 2}, {1, 3}, {3, 4}, {3, 5}};

	REQUIRE(ranges::binary_search(begin(a), end(a), a[0]));
	REQUIRE(ranges::binary_search(begin(a), end(a), a[1], less<>()));
	//REQUIRE(ranges::binary_search(begin(a), end(a), 1, less<>(), &std::pair<int, int>::first));

	REQUIRE(ranges::binary_search(a, a[2]));
	REQUIRE(ranges::binary_search(c, c[3]));

	REQUIRE(ranges::binary_search(a, a[4], less<>()));
	REQUIRE(ranges::binary_search(c, c[5], less<>()));

#if HAVE_PROJECTIONS
	REQUIRE(ranges::binary_search(a, 1, less<>(), &std::pair<int, int>::first));
	REQUIRE(ranges::binary_search(c, 1, less<>(), &std::pair<int, int>::first));

	REQUIRE(ranges::binary_search(a, 0, less<>(), &std::pair<int, int>::first));
	REQUIRE(ranges::binary_search(c, 0, less<>(), &std::pair<int, int>::first));

	REQUIRE(!ranges::binary_search(a, -1, less<>(), &std::pair<int, int>::first));
	REQUIRE(!ranges::binary_search(c, -1, less<>(), &std::pair<int, int>::first));

	REQUIRE(!ranges::binary_search(a, 4, less<>(), &std::pair<int, int>::first));
	REQUIRE(!ranges::binary_search(c, 4, less<>(), &std::pair<int, int>::first));
#endif

	//REQUIRE(ranges::binary_search(ranges::ext::iota_view<int>{0}, 42));

	{
		auto il = {0, 3, 5, 7, 9, 11, 13};
		REQUIRE(ranges::binary_search(std::move(il), 11));
	}
	{
		auto il = {0, 3, 5, 7, 9, 11, 13};
		REQUIRE(!ranges::binary_search(std::move(il), 8));
	}
}
