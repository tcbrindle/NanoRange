// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2017
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/view/reverse.hpp>
#include <nanorange/view/counted.hpp>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;

TEST_CASE("view.reverse") {
	using namespace ranges;
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		// FIXME FIXME
//		auto x = rg | view::reverse;
		auto x = view::reverse(rg);
		::check_equal(x, {9,8,7,6,5,4,3,2,1,0});
		static_assert(View<decltype(x)>, "");
		static_assert(Range<const decltype(x)>, "");
		static_assert(SizedRange<decltype(x)>, "");
		static_assert(CommonRange<decltype(x)>, "");
		static_assert(RandomAccessRange<decltype(x)>, "");
	}
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		// FIXME FIXME
		//auto x = view::counted(bidirectional_iterator(rg), 5) | view::reverse;
		auto x = view::reverse(view::counted(bidirectional_iterator<int*>(rg), 5));
		::check_equal(x, {4,3,2,1,0});
		static_assert(View<decltype(x)>, "");
		static_assert(!Range<const decltype(x)>, "");
		static_assert(SizedRange<decltype(x)>, "");
		static_assert(CommonRange<decltype(x)>, "");
		static_assert(BidirectionalRange<decltype(x)>, "");
		static_assert(!RandomAccessRange<decltype(x)>, "");
	}
}
