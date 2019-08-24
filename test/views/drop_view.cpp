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
#include <nanorange/views/drop.hpp>

#include <nanorange/views/iota.hpp>
#include <nanorange/views/reverse.hpp>
#include <nanorange/views/subrange.hpp>
#include <nanorange/views/take.hpp>
#include <list>
#include <vector>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;

namespace views {
	using namespace ranges::views;
	using ranges::views::drop;
}

TEST_CASE("views.drop")
{
	{
		int rgi[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

		auto rng0 = rgi | views::drop(6);
		static_assert(ranges::view<decltype(rng0)>);
		static_assert(ranges::sized_range<decltype(rng0)>);
		static_assert(ranges::random_access_iterator<decltype(ranges::begin(rng0))>);
		::check_equal(rng0, {6, 7, 8, 9, 10});
		CHECK(ranges::size(rng0) == 5u);

		auto rng1 = rng0 | views::reverse;
		static_assert(ranges::view<decltype(rng1)>);
		static_assert(ranges::sized_range<decltype(rng1)>);
		static_assert(ranges::random_access_iterator<decltype(ranges::begin(rng1))>);
		::check_equal(rng1, {10, 9, 8, 7, 6});
	}

	{
		std::vector<int> v{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = v | views::drop(6) | views::reverse;
		static_assert(ranges::view<decltype(rng)>);
		static_assert(ranges::sized_range<decltype(rng)>);
		static_assert(ranges::random_access_iterator<decltype(ranges::begin(rng))>);
		::check_equal(rng, {10, 9, 8, 7, 6});
	}

	{
		std::list<int> l{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = l | views::drop(6);
		static_assert(ranges::view<decltype(rng)>);
		static_assert(ranges::sized_range<decltype(rng)>);
		static_assert(ranges::bidirectional_iterator<decltype(ranges::begin(rng))>);
		::check_equal(rng, {6, 7, 8, 9, 10});
	}

	{
		auto rng = views::iota(10) | views::drop(10);
		static_assert(ranges::view<decltype(rng)>);
		static_assert(!ranges::sized_range<decltype(rng)>);
		// static_assert(ranges::is_infinite<decltype(rng4)>::value, "");
		auto b = ranges::begin(rng);
		CHECK(*b == 20);
		CHECK(*(b+1) == 21);
	}

//#if 0 // Test DISABLED while views::take(n) | views::reverse isn't possible.
	{
		auto rng = views::iota(10) | views::drop(10) | views::take(10) | views::reverse;
		static_assert(ranges::view<decltype(rng)>);
		static_assert(ranges::sized_range<decltype(rng)>);
		//static_assert(!ranges::is_infinite<decltype(rng5)>::value, "");
		::check_equal(rng, {29, 28, 27, 26, 25, 24, 23, 22, 21, 20});
		CHECK(ranges::size(rng) == 10u);
	}
//#endif
	{
		// consolation for the above not being possible
		auto rng = views::iota(10) | views::drop(10) | views::take(10);
		static_assert(ranges::view<decltype(rng)>);
		static_assert(!ranges::sized_range<decltype(rng)>);
		// static_assert(!ranges::is_infinite<decltype(rng5)>::value, "");
		::check_equal(rng, {20, 21, 22, 23, 24, 25, 26, 27, 28, 29});
	}

	{
		int some_ints[] = {0,1,2};
		auto rng = ranges::subrange(some_ints + 0, some_ints + 1);
		auto rng2 = views::drop(rng, 2);
		CHECK(ranges::begin(rng2) == some_ints + 1);
		CHECK(ranges::size(rng2) == 0u);
	}

#if 0 // Test DISABLED pending view implementations.
	{
		// Regression test for https://github.com/ericniebler/range-v3/issues/413
		auto skips = [](std::vector<int> xs) -> std::vector<std::vector<int>> {
			return views::iota(0, (int)xs.size())
				  | views::transform([&](int n) {
						 return xs | views::chunk(n + 1)
									  | views::transform(views::drop(n))
									  | views::join;
					 });
		};
		auto skipped = skips({1,2,3,4,5,6,7,8});
		CHECK(skipped.size() == 8u);
		if(skipped.size() >= 8u)
		{
			::check_equal(skipped[0], {1,2,3,4,5,6,7,8});
			::check_equal(skipped[1], {2,4,6,8});
			::check_equal(skipped[2], {3,6});
			::check_equal(skipped[3], {4,8});
			::check_equal(skipped[4], {5});
			::check_equal(skipped[5], {6});
			::check_equal(skipped[6], {7});
			::check_equal(skipped[7], {8});
		}
	}

	{
		static int const some_ints[] = {0,1,2,3};
		auto rng = debug_input_view<int const>{some_ints} | views::drop(2);
		using R = decltype(rng);
		CONCEPT_ASSERT(InputView<R>());
		CONCEPT_ASSERT(!forward_range<R>());
		CONCEPT_ASSERT(same_as<int const&, ranges::iter_reference_t<R>>());
		::check_equal(rng, {2,3});
	}

	{
		// regression test for ericniebler/range-v3#728
		auto rng1 = views::iota(1) | views::chunk(6) | views::take(3);
		int i = 2;
		for (auto o1 : rng1) {
			auto rng2 = o1 | views::drop(1);
			::check_equal(rng2, {i, i+1, i+2, i+3, i+4});
			i += 6;
		}
	}
#endif

	{
		// regression test for ericniebler/range-v3#813
		static int const some_ints[] = {0,1,2,3};
		auto rng = some_ints | views::drop(10);
		CHECK(ranges::empty(rng));
	}
}
