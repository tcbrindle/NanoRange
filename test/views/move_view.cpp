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
#include <stl2/view/move.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/ref.hpp>
#include <stl2/view/take_exactly.hpp>
#include <stl2/detail/algorithm/count.hpp>
#include <stl2/detail/algorithm/transform.hpp>
#include <stl2/detail/iterator/insert_iterators.hpp>
#include <memory>
#include <vector>
#include "../simple_test.hpp"

namespace ranges = __stl2;

namespace {
	ranges::Integral{I}
	auto make_interval(I from, I to) {
		return ranges::view::iota(from, to);
	}

	void test(ranges::Range&& base) {
		auto rng = base | ranges::view::move;
		CHECK(static_cast<std::size_t>(ranges::size(rng)) == ranges::size(base));
		CHECK(!ranges::empty(rng));
		int count = 0;
		for (auto i : rng) {
			CHECK(count++ == *i);
		}
		CHECK(ranges::count(base, std::unique_ptr<int>{}) == ranges::distance(base));
	}
}

int main() {
	auto rng = make_interval(0, 4);
	auto as_unique_ptr = [](auto&& i) {
		using I = decltype(i);
		return std::make_unique<std::decay_t<I>>(std::forward<I>(i));
	};
	{
		std::unique_ptr<int> some_ints[] = {
			std::make_unique<int>(0), std::make_unique<int>(1),
			std::make_unique<int>(2), std::make_unique<int>(3),
		};
		ranges::transform(rng, ranges::begin(some_ints), as_unique_ptr);
		test(some_ints);
	}

	{
		std::vector<std::unique_ptr<int>> some_ints;
		ranges::transform(rng, ranges::back_inserter(some_ints), as_unique_ptr);
		test(some_ints);
	}

	return test_result();
}
