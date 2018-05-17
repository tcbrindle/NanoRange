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

#include "set_intersection.hpp"
#include <stl2/detail/algorithm/lexicographical_compare.hpp>

int main()
{
	// Test projections
	{
		S ia[] = {S{1}, S{2}, S{2}, S{3}, S{3}, S{3}, S{4}, S{4}, S{4}, S{4}};
		T ib[] = {T{2}, T{4}, T{4}, T{6}};
		U ic[20];
		int ir[] = {2, 4, 4};
		const int sr = sizeof(ir)/sizeof(ir[0]);

		U* res = stl2::set_intersection(ia, ib, ic, std::less<int>(), &S::i, &T::j);
		CHECK((res - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, res, ir, ir+sr, std::less<int>(), &U::k) == 0);
	}

	return ::test_result();
}
