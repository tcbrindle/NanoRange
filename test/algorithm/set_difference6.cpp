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

#include "set_difference.hpp"
#include <stl2/detail/algorithm/lexicographical_compare.hpp>

int main()
{
	// Test projections
	{
		S ia[] = {S{1}, S{2}, S{2}, S{3}, S{3}, S{3}, S{4}, S{4}, S{4}, S{4}};
		const int sa = sizeof(ia)/sizeof(ia[0]);
		T ib[] = {T{2}, T{4}, T{4}, T{6}};
		const int sb = sizeof(ib)/sizeof(ib[0]);
		U ic[20];
		int ir[] = {1, 2, 3, 3, 3, 4, 4};
		const int sr = sizeof(ir)/sizeof(ir[0]);

		std::pair<S *, U *> res = stl2::set_difference(ia, ib, ic, std::less<int>(), &S::i, &T::j);
		CHECK((res.first - ia) == sa);
		CHECK((res.second - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, res.second, ir, ir+sr, std::less<int>(), &U::k) == 0);
		stl2::fill(ic, U{0});

		int irr[] = {6};
		const int srr = sizeof(irr)/sizeof(irr[0]);
		std::pair<T *, U *> res2 = stl2::set_difference(ib, ia, ic, std::less<int>(), &T::j, &S::i);
		CHECK((res2.first - ib) == sb);
		CHECK((res2.second - ic) == srr);
		CHECK(stl2::lexicographical_compare(ic, res2.second, ir, irr+srr, std::less<int>(), &U::k) == 0);
	}

	// Test rvalue ranges
	{
		S ia[] = {S{1}, S{2}, S{2}, S{3}, S{3}, S{3}, S{4}, S{4}, S{4}, S{4}};
		const int sa = sizeof(ia)/sizeof(ia[0]);
		T ib[] = {T{2}, T{4}, T{4}, T{6}};
		const int sb = sizeof(ib)/sizeof(ib[0]);
		U ic[20];
		int ir[] = {1, 2, 3, 3, 3, 4, 4};
		const int sr = sizeof(ir)/sizeof(ir[0]);

		auto res = stl2::set_difference(std::move(ia), std::move(ib), ic, std::less<int>(), &S::i, &T::j);
		CHECK((res.first.get_unsafe() - ia) == sa);
		CHECK((res.second - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, res.second, ir, ir+sr, std::less<int>(), &U::k) == 0);
		stl2::fill(ic, U{0});

		int irr[] = {6};
		const int srr = sizeof(irr)/sizeof(irr[0]);
		auto res2 = stl2::set_difference(std::move(ib), std::move(ia), ic, std::less<int>(), &T::j, &S::i);
		CHECK((res2.first.get_unsafe() - ib) == sb);
		CHECK((res2.second - ic) == srr);
		CHECK(stl2::lexicographical_compare(ic, res2.second, ir, irr+srr, std::less<int>(), &U::k) == 0);
	}

	return ::test_result();
}
