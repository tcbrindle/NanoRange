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

#include "set_union.hpp"
#include <stl2/detail/algorithm/lexicographical_compare.hpp>

int main()
{
	// Test projections
	{
		S ia[] = {S{1}, S{2}, S{2}, S{3}, S{3}, S{3}, S{4}, S{4}, S{4}, S{4}};
		T ib[] = {T{2}, T{4}, T{4}, T{6}};
		U ic[20];
		int ir[] = {1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6};
		const int sr = sizeof(ir)/sizeof(ir[0]);

		using R = std::tuple<S *, T*, U*>;
		R res = stl2::set_union(ia, ib, ic, std::less<int>(), &S::i, &T::j);
		CHECK((std::get<2>(res) - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, std::get<2>(res), ir, ir+sr, std::less<int>(), &U::k) == 0);
		stl2::fill(ic, U{0});

		using R2 = std::tuple<T *, S*, U*>;
		R2 res2 = stl2::set_union(ib, ia, ic, std::less<int>(), &T::j, &S::i);
		CHECK((std::get<2>(res2) - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, std::get<2>(res2), ir, ir+sr, std::less<int>(), &U::k) == 0);
	}

	// Test projections
	{
		S ia[] = {S{1}, S{2}, S{2}, S{3}, S{3}, S{3}, S{4}, S{4}, S{4}, S{4}};
		T ib[] = {T{2}, T{4}, T{4}, T{6}};
		U ic[20];
		int ir[] = {1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 6};
		const int sr = sizeof(ir)/sizeof(ir[0]);

		auto res = stl2::set_union(std::move(ia), std::move(ib), ic, std::less<int>(), &S::i, &T::j);
		CHECK(std::get<0>(res).get_unsafe() == stl2::end(ia));
		CHECK(std::get<1>(res).get_unsafe() == stl2::end(ib));
		CHECK((std::get<2>(res) - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, std::get<2>(res), ir, ir+sr, std::less<int>(), &U::k) == 0);
		stl2::fill(ic, U{0});

		auto res2 = stl2::set_union(std::move(ib), std::move(ia), ic, std::less<int>(), &T::j, &S::i);
		CHECK(std::get<0>(res2).get_unsafe() == stl2::end(ib));
		CHECK(std::get<1>(res2).get_unsafe() == stl2::end(ia));
		CHECK((std::get<2>(res2) - ic) == sr);
		CHECK(stl2::lexicographical_compare(ic, std::get<2>(res2), ir, ir+sr, std::less<int>(), &U::k) == 0);
	}

	return ::test_result();
}
