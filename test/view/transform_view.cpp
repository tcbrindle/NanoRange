// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/view/transform.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/detail/algorithm/count.hpp>
#include <stl2/detail/algorithm/transform.hpp>
#include <stl2/detail/iterator/insert_iterators.hpp>
#include <memory>
#include <vector>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace
{
	struct is_odd
	{
		bool operator()(int i) const
		{
			return (i % 2) == 1;
		}
	};
}

int main()
{
	using namespace ranges;

	int rgi[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

	auto rng = rgi | view::transform(is_odd());
	static_assert(Same<int &, decltype(*begin(rgi))>);
	static_assert(Same<bool, decltype(*begin(rng))>);
	static_assert(View<decltype(rng)>);
	static_assert(SizedRange<decltype(rng)>);
	static_assert(RandomAccessRange<decltype(rng)>);
	CHECK_EQUAL(rng, {true, false, true, false, true, false, true, false, true, false});

	std::pair<int, int> rgp[] = {{1,1}, {2,2}, {3,3}, {4,4}, {5,5}, {6,6}, {7,7}, {8,8}, {9,9}, {10,10}};
	auto rng2 = rgp | view::transform(&std::pair<int,int>::first);
	static_assert(Same<int &, decltype(*begin(rng2))>);
	static_assert(Same<iter_value_t<iterator_t<decltype(rng2)>>, int>);
	static_assert(Same<decltype(iter_move(begin(rng2))), int &&>);
	static_assert(View<decltype(rng2)>);
	static_assert(CommonRange<decltype(rng2)>);
	static_assert(SizedRange<decltype(rng2)>);
	static_assert(RandomAccessRange<decltype(rng2)>);
	CHECK_EQUAL(rng2, {1,2,3,4,5,6,7,8,9,10});
	// CHECK_EQUAL(rng2 | view::reverse, {10,9,8,7,6,5,4,3,2,1});
	// CHECK(&*begin(rng2) == &rgp[0].first);
	// CHECK(rng2.size() == 10u);

	return ::test_result();
}
