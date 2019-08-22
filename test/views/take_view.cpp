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
#include <stl2/view/take.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/filter.hpp>
#include <stl2/view/subrange.hpp>
#include <stl2/detail/iterator/istream_iterator.hpp>
#include <list>
#include <memory>
#include <vector>
#include <sstream>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace {
	template <class I, class S>
	struct my_subrange : ranges::subrange<I, S> {
		my_subrange() = default;
		my_subrange(I i, S s)
		: ranges::subrange<I, S>{i, s} {}
		I begin() { return this->my_subrange::subrange::begin(); }
		S end() { return this->my_subrange::subrange::end(); }
	};
}

int main()
{
	using namespace ranges;

	{
		auto rng = view::iota(0) | view::take(10);
		using R = decltype(rng);
		static_assert(View<R>);
		static_assert(!SizedRange<R>);
		static_assert(!CommonRange<R>);
		static_assert(RandomAccessRange<R>);
		static_assert(!ContiguousRange<R>);
		static_assert(Range<const R>);
		CHECK_EQUAL(rng, {0,1,2,3,4,5,6,7,8,9});
	}

	{
		auto rng = view::iota(0, 100) | view::take(10);
		using R = decltype(rng);
		static_assert(View<R>);
		static_assert(SizedRange<R>);
		static_assert(CommonRange<R>);
		static_assert(RandomAccessRange<R>);
		static_assert(!ContiguousRange<R>);
		static_assert(Range<const R>);
		CHECK_EQUAL(rng, {0,1,2,3,4,5,6,7,8,9});
	}

	{
		auto evens = [](int i) { return i % 2 == 0; };
		std::stringstream sin{"0 1 2 3 4 5 6 7 8 9"};
		my_subrange is{istream_iterator<int>{sin}, istream_iterator<int>{}};
		static_assert(InputRange<decltype(is)>);
		auto rng = is | view::filter(evens) | view::take(3);
		using R = decltype(rng);
		static_assert(View<R>);
		static_assert(!SizedRange<decltype(rng.base())>);
		static_assert(!SizedRange<R>);
		static_assert(!CommonRange<R>);
		static_assert(InputRange<R>);
		static_assert(!ForwardRange<R>);
		static_assert(!Range<const R>);
		CHECK_EQUAL(rng, {0,2,4});
	}

	{
		auto odds = [](int i) { return i % 2 == 1; };
		std::stringstream sin{"0 1 2 3 4 5 6 7 8 9"};
		my_subrange is{istream_iterator<int>{sin}, istream_iterator<int>{}};
		auto pipe = view::filter(odds) | view::take(3);
		auto rng = is | pipe;
		using R = decltype(rng);
		static_assert(View<R>);
		static_assert(!SizedRange<decltype(rng.base())>);
		static_assert(!SizedRange<R>);
		static_assert(!CommonRange<R>);
		static_assert(InputRange<R>);
		static_assert(!ForwardRange<R>);
		static_assert(!Range<const R>);
		CHECK_EQUAL(rng, {1,3,5});
	}

	{
		int some_ints[] = {1,2,3};
		take_view{some_ints, 2};
	}

	{
		std::list<int> l{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto rng = l | view::take(6);
		static_assert(ranges::View<decltype(rng)>);
		static_assert(ranges::SizedRange<decltype(rng)>);
		static_assert(ranges::BidirectionalIterator<decltype(ranges::begin(rng))>);
		CHECK_EQUAL(rng, {0, 1, 2, 3, 4, 5});
	}

	return ::test_result();
}
