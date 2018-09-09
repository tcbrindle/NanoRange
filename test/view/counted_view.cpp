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
#include <stl2/view/counted.hpp>
#include "../simple_test.hpp"
#include "../test_iterators.hpp"

namespace ranges = __stl2;

int main() {
	using namespace ranges;
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9};
		auto x = view::counted(rg, 10);
		CHECK_EQUAL(x, {0,1,2,3,4,5,6,7,8,9});
		static_assert(View<decltype(x)>);
		static_assert(SizedRange<decltype(x)>);
		static_assert(CommonRange<decltype(x)>);
		static_assert(RandomAccessRange<decltype(x)>);
	}
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9};
		auto x = view::counted(forward_iterator(rg), 10);
		CHECK_EQUAL(x, {0,1,2,3,4,5,6,7,8,9});
		static_assert(View<decltype(x)>);
		static_assert(SizedRange<decltype(x)>);
		static_assert(!CommonRange<decltype(x)>);
		static_assert(ForwardRange<decltype(x)>);
		static_assert(!BidirectionalRange<decltype(x)>);
	}
 	return test_result();
}
