// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter
//  Copyright Eric Niebler
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/view/drop_while.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/take.hpp>
#include <list>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace view {
	using namespace ranges::view;
	using view::ext::drop_while;
}

int main()
{
	{
		auto rng0 = view::iota(10) | view::drop_while([](int i) { return i < 25; });
		//  static_assert(range_cardinality<decltype(rng0)>::value == unknown);
		static_assert(ranges::RandomAccessRange<decltype(rng0)>);
		static_assert(!ranges::CommonRange<decltype(rng0)>);
		auto b = rng0.begin();
		CHECK(*b == 25);
		CHECK(*(b+1) == 26);
		CHECK_EQUAL(rng0 | view::take(10), {25, 26, 27, 28, 29, 30, 31, 32, 33, 34});
	}

	std::list<int> vi{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
	auto rng1 = vi | view::drop_while([](int i) { return i != 50; });
	//  static_assert(range_cardinality<decltype(rng1)>::value == ranges::finite);
	static_assert(ranges::BidirectionalRange<decltype(rng1)>);
	static_assert(ranges::CommonRange<decltype(rng1)>);
	CHECK(rng1.begin() == rng1.end());


	// Check with a mutable predicate
	{
		auto rng0 = view::iota(10) | view::drop_while([b = true](int i) mutable { b = !b; return i < 25; });
		//  static_assert(range_cardinality<decltype(rng0)>::value == unknown);
		static_assert(ranges::RandomAccessRange<decltype(rng0)>);
		static_assert(!ranges::CommonRange<decltype(rng0)>);
		auto b = rng0.begin();
		CHECK(*b == 25);
		CHECK(*(b+1) == 26);
		CHECK_EQUAL(rng0 | view::take(10), {25, 26, 27, 28, 29, 30, 31, 32, 33, 34});
	}

	//  {
	//      // Check with move-only subview
	//      auto rng = debug_input_view<const int>{rgi} | view::drop_while([](int i){ return i < 4; });
	//      using R = decltype(rng);
	//      static_assert(InputView<R>());
	//      static_assert(!ForwardRange<R>());
	//      static_assert(!BoundedRange<R>());
	//      static_assert(Same<int const&, range_reference_t<R>>());
	//      CHECK_EQUAL(rng, {4,5,6,7,8,9});
	//  }

    return ::test_result();
}
