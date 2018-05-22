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
#include <stl2/view/reverse.hpp>
#include <stl2/view/counted.hpp>
#include "../simple_test.hpp"
#include "../test_iterators.hpp"

namespace ranges = __stl2;

int main() {
	using namespace ranges;
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		auto x = rg | view::reverse;
		check_equal(x, {9,8,7,6,5,4,3,2,1,0});
		static_assert(models::View<decltype(x)>);
		static_assert(models::Range<const decltype(x)>);
		static_assert(models::SizedRange<decltype(x)>);
		static_assert(models::BoundedRange<decltype(x)>);
		static_assert(models::RandomAccessRange<decltype(x)>);
	}
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		auto x = view::counted(bidirectional_iterator(rg), 5) | view::reverse;
		check_equal(x, {4,3,2,1,0});
		static_assert(models::View<decltype(x)>);
		static_assert(!models::Range<const decltype(x)>);
		static_assert(models::SizedRange<decltype(x)>);
		static_assert(models::BoundedRange<decltype(x)>);
		static_assert(models::BidirectionalRange<decltype(x)>);
		static_assert(!models::RandomAccessRange<decltype(x)>);
	}
 	return test_result();
}
