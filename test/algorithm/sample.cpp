// Range v3 lbrary
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2016
//
//  Use, modification and distrbution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3
//
//  Copyright 2005 - 2007 Adobe Systems Incorporated
//  Distrbuted under the MIT License(see accompanying file LICENSE_1_0_0.txt
//  or a copy at http://stlab.adobe.com/licenses.html)

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <nanorange/algorithm/sample.hpp>

#include <array>
#include <numeric>
#include <nanorange/algorithm/equal.hpp>
#include <nanorange/iterator/move_iterator.hpp>
#include "../test_utils.hpp"
#include "../test_iterators.hpp"

namespace ranges = nano::ranges;

TEST_CASE("alg.sample")
{
	constexpr unsigned N = 100;
	constexpr unsigned K = 10;
	{
		std::array<int, N> i;
		std::iota(std::begin(i), std::end(i), 0);
		std::array<int, K> a{}, b{}, c{};
		std::minstd_rand g1, g2 = g1;

		{
			auto result = ranges::sample(random_access_iterator<int*>(i.data()),
				sentinel<int*>(i.data()+N), a.begin(), K, g1);
			CHECK(result == a.begin() + K);
			CHECK(!ranges::equal(a, c));
		}

		{
			auto result = ranges::sample(i.begin(), i.end(), b.begin(), K, g1);
			CHECK(result == b.begin() + K);
			CHECK(!ranges::equal(a, b));
			CHECK(!ranges::equal(b, c));
		}

		{
			auto result = ranges::sample(i.begin(), i.end(), c.begin(), K, g2);
			CHECK(result == c.begin() + K);
			CHECK(ranges::equal(a, c));
		}
	}

	{
		std::array<int, N> i;
		std::iota(std::begin(i), std::end(i), 0);
		std::array<int, K> a{}, b{}, c{};
		std::minstd_rand g1, g2 = g1;
		auto rng = ranges::subrange(random_access_iterator<int*>(i.data()), sentinel<int*>(i.data() + N));

		{
			auto result = ranges::sample(rng, a.begin(), K, g1);
			CHECK(result == a.end());
			CHECK(!ranges::equal(a, b));
		}

		{
			auto result = ranges::sample(i, b.begin(), K, g2);
			CHECK(result == b.end());
			CHECK(ranges::equal(a, b));
		}

		{
			auto result = ranges::sample(i, b.begin(), K, g1);
			CHECK(result == b.end());
			CHECK(!ranges::equal(a, b));
			CHECK(!ranges::equal(b, c));
		}

		{
			a.fill(0);
			auto result = ranges::sample(std::move(rng), a.begin(), K, g1);
			CHECK(result == a.end());
			CHECK(!ranges::equal(a, c));
		}
	}
}
