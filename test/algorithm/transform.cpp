// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/detail/algorithm/transform.hpp>

#include "../simple_test.hpp"

namespace ranges = __stl2;

int main() {
	int rgi[]{1,2,3,4,5};
	ranges::transform(rgi, rgi+5, rgi, [](int i){return i*2;});
	::check_equal(rgi, {2,4,6,8,10});

	ranges::transform(rgi, rgi, [](int i){return i/2;});
	::check_equal(rgi, {1,2,3,4,5});

	{
		auto sum = [](int x, int y) { return x + y; };
		int const source1[] = {0,1,2,3};
		int const source2[] = {4,5,6,7};
		int const control[] = {4,6,8,10};

		{
			int target[4]{};
			auto result = ranges::transform(source1, ranges::begin(source2), target, sum);
			CHECK(result.in1() == ranges::end(source1));
			CHECK(result.in2() == ranges::end(source2));
			CHECK(result.out() == ranges::end(target));
			::check_equal(target, control);
		}

		{
			int target[4]{};
			auto result = ranges::transform(ranges::begin(source1), ranges::end(source1),
				ranges::begin(source2), target, sum);
			CHECK(result.in1() == ranges::end(source1));
			CHECK(result.in2() == ranges::end(source2));
			CHECK(result.out() == ranges::end(target));
			::check_equal(target, control);
		}
	}

	return ::test_result();
}
