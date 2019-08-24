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
#include <nanorange/views/take.hpp>
#include <nanorange/views/iota.hpp>
#include <nanorange/views/filter.hpp>
#include <nanorange/views/subrange.hpp>
#include <nanorange/iterator/istream_iterator.hpp>
#include <list>
#include <memory>
#include <vector>
#include <sstream>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;

namespace {
	template<class I, class S>
	struct my_subrange : ranges::subrange<I, S> {
		my_subrange() = default;
		my_subrange(I i, S s)
		: ranges::subrange<I, S>{i, s} {}
		I begin() { return this->my_subrange::subrange::begin(); }
		S end() { return this->my_subrange::subrange::end(); }
	};
}

TEST_CASE("views.take")
{
	using namespace ranges;

	{
		auto rng = views::iota(0) | views::take(10);
		using R = decltype(rng);
		static_assert(view<R>);
		static_assert(!sized_range<R>);
		static_assert(!common_range<R>);
		static_assert(random_access_range<R>);
		static_assert(!contiguous_range<R>);
		static_assert(range<const R>);
		::check_equal(rng, {0,1,2,3,4,5,6,7,8,9});
	}

	{
		auto rng = views::iota(0, 100) | views::take(10);
		using R = decltype(rng);
		static_assert(view<R>);
		static_assert(sized_range<R>);
		static_assert(common_range<R>);
		static_assert(random_access_range<R>);
		static_assert(!contiguous_range<R>);
		static_assert(range<const R>);
		::check_equal(rng, {0,1,2,3,4,5,6,7,8,9});
	}

	{
		auto evens = [](int i) { return i % 2 == 0; };
		std::stringstream sin{"0 1 2 3 4 5 6 7 8 9"};
		my_subrange is{istream_iterator<int>{sin}, istream_iterator<int>{}};
		static_assert(input_range<decltype(is)>);
		auto rng = is | views::filter(evens) | views::take(3);
		using R = decltype(rng);
		static_assert(view<R>);
		static_assert(!sized_range<decltype(rng.base())>);
		static_assert(!sized_range<R>);
		static_assert(!common_range<R>);
		static_assert(input_range<R>);
		static_assert(!forward_range<R>);
		static_assert(!range<const R>);
		::check_equal(rng, {0,2,4});
	}

	{
		auto odds = [](int i) { return i % 2 == 1; };
		std::stringstream sin{"0 1 2 3 4 5 6 7 8 9"};
		my_subrange is{istream_iterator<int>{sin}, istream_iterator<int>{}};
		auto pipe = views::filter(odds) | views::take(3);
		auto rng = is | pipe;
		using R = decltype(rng);
		static_assert(view<R>);
		static_assert(!sized_range<decltype(rng.base())>);
		static_assert(!sized_range<R>);
		static_assert(!common_range<R>);
		static_assert(input_range<R>);
		static_assert(!forward_range<R>);
		static_assert(!range<const R>);
		::check_equal(rng, {1,3,5});
	}

	{
		int some_ints[] = {1,2,3};
		take_view v{some_ints, 2};
		(void) v;
	}

	{
		std::list<int> l{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = l | views::take(6);
		static_assert(ranges::view<decltype(rng)>);
		static_assert(ranges::sized_range<decltype(rng)>);
		static_assert(ranges::bidirectional_iterator<decltype(ranges::begin(rng))>);
		::check_equal(rng, {0, 1, 2, 3, 4, 5});
	}
}
