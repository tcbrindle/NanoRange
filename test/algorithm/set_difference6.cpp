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
#include <nanorange/algorithm/lexicographical_compare.hpp>

TEST_CASE("alg.set_difference6")
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

		stl2::set_difference_result<S *, U *> res = stl2::set_difference(ia, ib, ic, std::less<int>(), &S::i, &T::j);
		CHECK((res.in - ia) == sa);
		CHECK((res.out - ic) == sr);
		CHECK_FALSE(stl2::lexicographical_compare(ic, res.out, ir, ir+sr, std::less<int>(), &U::k));
		stl2::fill(ic, U{0});

		int irr[] = {6};
		const int srr = sizeof(irr)/sizeof(irr[0]);
		stl2::set_difference_result<T *, U *> res2 = stl2::set_difference(ib, ia, ic, std::less<int>(), &T::j, &S::i);
		CHECK((res2.in - ib) == sb);
		CHECK((res2.out - ic) == srr);
		CHECK_FALSE(stl2::lexicographical_compare(ic, res2.out, ir, irr+srr, std::less<int>(), &U::k));
	}

#ifdef HAVE_RVALUE_RANGES
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
		// FIXME: Usual MSVC rvalue range weirdness
#ifndef _MSC_VER
		CHECK((res.in.get_unsafe() - ia) == sa);
#else
		CHECK((res.in - ia) == sa);
#endif
		CHECK((res.out - ic) == sr);
		CHECK_FALSE(stl2::lexicographical_compare(ic, res.out, ir, ir+sr, std::less<int>(), &U::k));
		stl2::fill(ic, U{0});

		int irr[] = {6};
		const int srr = sizeof(irr)/sizeof(irr[0]);
		auto res2 = stl2::set_difference(std::move(ib), std::move(ia), ic, std::less<int>(), &T::j, &S::i);
		// FIXME: Usual MSVC rvalue range weirdness
#ifndef _MSC_VER
		CHECK((res2.in.get_unsafe() - ib) == sb);
#else
		CHECK((res2.in - ib) == sb);
#endif
		CHECK((res2.out - ic) == srr);
		CHECK_FALSE(stl2::lexicographical_compare(ic, res2.out, ir, irr+srr, std::less<int>(), &U::k));
	}
#endif
}
