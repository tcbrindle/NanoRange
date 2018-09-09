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
#include <stl2/view/filter.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/ref.hpp>
#include <stl2/view/take_exactly.hpp>
#include <stl2/detail/algorithm/count.hpp>
#include <stl2/detail/algorithm/transform.hpp>
#include <stl2/detail/iterator/insert_iterators.hpp>
#include <memory>
#include <list>
#include <vector>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace {
	struct is_odd {
		bool operator()(int i) const {
			return (i % 2) == 1;
		}
	};

	struct is_even {
		bool operator()(int i) const {
			return (i % 2) == 0;
		}
	};
}

int main() {
	using namespace ranges;

	int rgi[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	static_assert(size(view::all(rgi))==10);

	auto rng = rgi | view::filter(is_odd());
	static_assert(Same<int &, decltype(*begin(rgi))>);
	static_assert(Same<int &, decltype(*begin(rng))>);
	static_assert(View<decltype(rng)>);
	static_assert(InputRange<decltype(rng)>);
	static_assert(CommonRange<decltype(rng)>);
	static_assert(!SizedRange<decltype(rng)>);
	static_assert(BidirectionalRange<decltype(rng)>);
	static_assert(!RandomAccessRange<decltype(rng)>);
	CHECK_EQUAL(rng, {1,3,5,7,9});

	//CHECK_EQUAL(rng | view::reverse, {9,7,5,3,1});
	//auto tmp = rng | view::reverse;
	//CHECK(&*begin(tmp) == &rgi[8]);

	// auto rng2 = view::counted(rgi, 10) | view::remove_if(not_fn(is_odd()));
	// static_assert(Same<int &, decltype(*begin(rng2))>);
	// static_assert(BidirectionalView<__f<decltype(rng2)>>);
	// static_assert(!RandomAccessView<__f<decltype(rng2)>>);
	// static_assert(CommonView<__f<decltype(rng2)>>);
	// static_assert(!SizedView<__f<decltype(rng2)>>);
	// CHECK_EQUAL(rng2, {1,3,5,7,9});
	// CHECK(&*begin(rng2) == &rgi[0]);

	// auto rng3 = view::counted(bidirectional_iterator<int*>{rgi}, 10) | view::remove_if(is_even());
	// static_assert(Same<int &, decltype(*begin(rng3))>);
	// static_assert(BidirectionalView<__f<decltype(rng3)>>);
	// static_assert(!RandomAccessView<__f<decltype(rng3)>>);
	// static_assert(!CommonView<__f<decltype(rng3)>>);
	// static_assert(!SizedView<__f<decltype(rng3)>>);
	// CHECK_EQUAL(rng3, {1,3,5,7,9});
	// CHECK(&*begin(rng3) == &rgi[0]);
	// CHECK(&*prev(next(begin(rng3))) == &rgi[0]);

	// Test remove_if with a mutable lambda
	bool flag = false;
	auto f = [flag](int) mutable { return flag = !flag;};
	detail::semiregular_box<decltype(f)> b{f};
	auto b2 = b;
	b = b2;
	auto mutable_rng = view::filter(rgi, [flag](int) mutable { return flag = !flag;});
	CHECK_EQUAL(mutable_rng, {1,3,5,7,9});
	static_assert(Range<decltype(mutable_rng)>);
	static_assert(Copyable<decltype(mutable_rng)>);
	static_assert(!View<decltype(mutable_rng) const>);

	// {
	//	 const std::array<int, 3> a{{0, 1, 2}};
	//	 const std::vector<int> b{3, 4, 5, 6};

	//	 auto r = view::concat(a, b);
	//	 auto f = [](int i) { return i != 1 && i != 5; };
	//	 auto r2 = r | view::remove_if(f);
	//	 CHECK_EQUAL(r2, {1,5});
	// }

	// {
	//	 auto rng = debug_input_view<int const>{rgi} | view::remove_if(is_even{});
	//	 CHECK_EQUAL(rng, {1,3,5,7,9});
	// }

	{
		// Test operator-> with pointer
		std::pair<int, int> pairs[] = {{1, 99}, {2, 1}, {3, 99}, {4, 3}};
		auto rng = view::filter(pairs, [](auto&& p) { return p.first % 2 == 0; });
		auto i = ranges::begin(rng);
		auto const e = ranges::end(rng);
		int sum = 0;
		for (; i != e; ++i) {
			sum += i->second;
		}
		CHECK(sum == 4);
	}

	{
		// Test operator-> with non-pointer
		std::list<std::pair<int, int>> pairs = {{1, 99}, {2, 1}, {3, 99}, {4, 3}};
		auto rng = view::filter(pairs, [](auto&& p) { return p.first % 2 == 0; });
		auto i = ranges::begin(rng);
		auto const e = ranges::end(rng);
		int sum = 0;
		for (; i != e; ++i) {
			sum += i->second;
		}
		CHECK(sum == 4);
	}

	return test_result();
}
