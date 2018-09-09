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
#include <stl2/view/drop.hpp>

#include <stl2/view/iota.hpp>
#include <stl2/view/join.hpp>
#include <stl2/view/reverse.hpp>
#include <stl2/view/subrange.hpp>
#include <stl2/view/take.hpp>
#include <list>
#include <vector>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace view {
	using namespace ranges::view;
	using ranges::view::ext::drop;
}

int main()
{
	{
		int rgi[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

		auto rng0 = rgi | view::drop(6);
		static_assert(ranges::View<decltype(rng0)>);
		static_assert(ranges::SizedRange<decltype(rng0)>);
		static_assert(ranges::RandomAccessIterator<decltype(ranges::begin(rng0))>);
		CHECK_EQUAL(rng0, {6, 7, 8, 9, 10});
		CHECK(ranges::size(rng0) == 5u);

		auto rng1 = rng0 | view::reverse;
		static_assert(ranges::View<decltype(rng1)>);
		static_assert(ranges::SizedRange<decltype(rng1)>);
		static_assert(ranges::RandomAccessIterator<decltype(ranges::begin(rng1))>);
		CHECK_EQUAL(rng1, {10, 9, 8, 7, 6});
	}

	{
		std::vector<int> v{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = v | view::drop(6) | view::reverse;
		static_assert(ranges::View<decltype(rng)>);
		static_assert(ranges::SizedRange<decltype(rng)>);
		static_assert(ranges::RandomAccessIterator<decltype(ranges::begin(rng))>);
		CHECK_EQUAL(rng, {10, 9, 8, 7, 6});
	}

	{
		std::list<int> l{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = l | view::drop(6);
		static_assert(ranges::View<decltype(rng)>);
		static_assert(ranges::SizedRange<decltype(rng)>);
		static_assert(ranges::BidirectionalIterator<decltype(ranges::begin(rng))>);
		CHECK_EQUAL(rng, {6, 7, 8, 9, 10});
	}

	{
		auto rng = view::iota(10) | view::drop(10);
		static_assert(ranges::View<decltype(rng)>);
		static_assert(!ranges::SizedRange<decltype(rng)>);
		// static_assert(ranges::is_infinite<decltype(rng4)>::value, "");
		auto b = ranges::begin(rng);
		CHECK(*b == 20);
		CHECK(*(b+1) == 21);
	}

	// Test DISABLED while view::take(n) | view::reverse isn't possible.
	// {
	// 	auto rng = view::iota(10) | view::drop(10) | view::take(10) | view::reverse;
	// 	static_assert(ranges::View<decltype(rng)>);
	// 	static_assert(ranges::SizedRange<decltype(rng)>);
	// 	static_assert(!ranges::is_infinite<decltype(rng5)>::value, "");
	// 	CHECK_EQUAL(rng, {29, 28, 27, 26, 25, 24, 23, 22, 21, 20});
	// 	CHECK(ranges::size(rng) == 10u);
	// }

	{
		// consolation for the above not being possible
		auto rng = view::iota(10) | view::drop(10) | view::take(10);
		static_assert(ranges::View<decltype(rng)>);
		static_assert(!ranges::SizedRange<decltype(rng)>);
		// static_assert(!ranges::is_infinite<decltype(rng5)>::value, "");
		CHECK_EQUAL(rng, {20, 21, 22, 23, 24, 25, 26, 27, 28, 29});
	}

	{
		int some_ints[] = {0,1,2};
		auto rng = ranges::subrange(some_ints + 0, some_ints + 1);
		auto rng2 = view::drop(rng, 2);
		CHECK(ranges::begin(rng2) == some_ints + 1);
		CHECK(ranges::size(rng2) == 0u);
	}

	// Test DISABLED until view::chunk becomes available.
	// {
	// 	// Regression test for https://github.com/ericniebler/range-v3/issues/413
	// 	auto skips = [](std::vector<int> xs) -> std::vector<std::vector<int>> {
	// 		return view::iota(0, (int)xs.size())
	// 			  | view::transform([&](int n) {
	// 					 return xs | view::chunk(n + 1)
	// 								  | view::transform(view::drop(n))
	// 								  | view::join;
	// 				 });
	// 	};
	// 	auto skipped = skips({1,2,3,4,5,6,7,8});
	// 	CHECK(skipped.size() == 8u);
	// 	if(skipped.size() >= 8u)
	// 	{
	// 		CHECK_EQUAL(skipped[0], {1,2,3,4,5,6,7,8});
	// 		CHECK_EQUAL(skipped[1], {2,4,6,8});
	// 		CHECK_EQUAL(skipped[2], {3,6});
	// 		CHECK_EQUAL(skipped[3], {4,8});
	// 		CHECK_EQUAL(skipped[4], {5});
	// 		CHECK_EQUAL(skipped[5], {6});
	// 		CHECK_EQUAL(skipped[6], {7});
	// 		CHECK_EQUAL(skipped[7], {8});
	// 	}
	// }

	// {
	// 	static int const some_ints[] = {0,1,2,3};
	// 	auto rng = debug_input_view<int const>{some_ints} | view::drop(2);
	// 	using R = decltype(rng);
	// 	CONCEPT_ASSERT(InputView<R>());
	// 	CONCEPT_ASSERT(!ForwardRange<R>());
	// 	CONCEPT_ASSERT(Same<int const&, ranges::iter_reference_t<R>>());
	// 	CHECK_EQUAL(rng, {2,3});
	// }

	// {
	// 	// regression test for ericniebler/range-v3#728
	// 	auto rng1 = view::iota(1) | view::chunk(6) | view::take(3);
	// 	int i = 2;
	// 	for (auto o1 : rng1) {
	// 		auto rng2 = o1 | view::drop(1);
	// 		CHECK_EQUAL(rng2, {i, i+1, i+2, i+3, i+4});
	// 		i += 6;
	// 	}
	// }

	{
		// regression test for ericniebler/range-v3#813
		static int const some_ints[] = {0,1,2,3};
		auto rng = some_ints | view::drop(10);
		CHECK(ranges::empty(rng));
	}

	return test_result();
}
