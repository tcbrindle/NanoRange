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
//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <stl2/detail/algorithm/set_intersection.hpp>
#include <stl2/detail/algorithm/fill.hpp>
#include <algorithm>
#include <functional>
#include "../simple_test.hpp"
#include "../test_utils.hpp"
#include "../test_iterators.hpp"

namespace stl2 = __stl2;

template <class Iter1, class Iter2, class OutIter>
void
test()
{
	int ia[] = {1, 2, 2, 3, 3, 3, 4, 4, 4, 4};
	const int sa = sizeof(ia)/sizeof(ia[0]);
	int ib[] = {2, 4, 4, 6};
	const int sb = sizeof(ib)/sizeof(ib[0]);
	int ic[20];
	int ir[] = {2, 4, 4};
	const int sr = sizeof(ir)/sizeof(ir[0]);

	auto set_intersection = ::make_testable_2<true, true>([](auto&&... args) {
		return stl2::set_intersection(stl2::forward<decltype(args)>(args)...);
	});

	set_intersection(Iter1(ia), Iter1(ia+sa),
					 Iter2(ib), Iter2(ib+sb), OutIter(ic)).check([&](OutIter ce)
		{
			CHECK((base(ce) - ic) == sr);
			CHECK(std::lexicographical_compare(ic, base(ce), ir, ir+sr) == 0);
			stl2::fill(ic, 0);
		});
	set_intersection(Iter1(ib), Iter1(ib+sb),
					 Iter2(ia), Iter2(ia+sa), OutIter(ic)).check([&](OutIter ce)
		{
			CHECK((base(ce) - ic) == sr);
			CHECK(std::lexicographical_compare(ic, base(ce), ir, ir+sr) == 0);
			stl2::fill(ic, 0);
		});


	set_intersection(Iter1(ia), Iter1(ia+sa),
					 Iter2(ib), Iter2(ib+sb), OutIter(ic), std::less<int>()).check([&](OutIter ce)
		{
			CHECK((base(ce) - ic) == sr);
			CHECK(std::lexicographical_compare(ic, base(ce), ir, ir+sr) == 0);
			stl2::fill(ic, 0);
		});
	set_intersection(Iter1(ib), Iter1(ib+sb),
					 Iter2(ia), Iter2(ia+sa), OutIter(ic), std::less<int>()).check([&](OutIter ce)
		{
			CHECK((base(ce) - ic) == sr);
			CHECK(std::lexicographical_compare(ic, base(ce), ir, ir+sr) == 0);
			stl2::fill(ic, 0);
		});
}

struct S
{
	int i;
};

struct T
{
	int j;
};

struct U
{
	int k;
	U& operator=(S s) { k = s.i; return *this;}
	U& operator=(T t) { k = t.j; return *this;}
};
