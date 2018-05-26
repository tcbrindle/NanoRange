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

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <nanorange/algorithm/shuffle.hpp>
#include <nanorange/algorithm/equal.hpp>
#include <numeric>
#include <random>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace stl2 = nano;

TEST_CASE("alg.shuffle")
{
	{
		int ia[100];
		constexpr unsigned s = sizeof(ia)/sizeof(ia[0]);
		int ib[s];
		int orig[s];
		std::iota(ia, ia + s, 0);
		std::iota(ib, ib + s, 0);
		std::iota(orig, orig + s, 0);
		std::minstd_rand g;
		stl2::shuffle(random_access_iterator<int*>(ia), random_access_iterator<int*>(ia+s), g);
		CHECK(!stl2::equal(ia, orig));
		CHECK(stl2::shuffle(ib, ib+s, g) == ib+s);
		CHECK(!stl2::equal(ia, ib));
	}

	{
		int ia[100];
		constexpr unsigned s = sizeof(ia)/sizeof(ia[0]);
		int ib[100];
		int orig[100];
		std::iota(ia, ia + s, 0);
		std::iota(ib, ib + s, 0);
		std::iota(orig, orig + s, 0);
		std::minstd_rand g;
		auto rng = stl2::ext::make_range(random_access_iterator<int*>(ia),  random_access_iterator<int*>(ia+s));
		stl2::shuffle(rng, g);
		CHECK(!stl2::equal(ia, orig));
		CHECK(stl2::shuffle(ib, g) == stl2::end(ib));
		CHECK(!stl2::equal(ia, ib));

		std::iota(ia, ia + s, 0);
		CHECK(stl2::shuffle(std::move(rng), g).get_unsafe().base() == stl2::end(ia));
		CHECK(!stl2::equal(ia, orig));
	}
}
