// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2018
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/view/empty.hpp>
#include "../catch.hpp"

namespace ranges = nano::ranges;

TEST_CASE("view.empty") {
	using namespace ranges;

	{
		auto rng = view::empty<double>;

		CHECK(ranges::begin(rng) == nullptr);
		CHECK(rng.begin() == nullptr);
		CHECK(ranges::end(rng) == nullptr);
		CHECK(rng.end() == nullptr);
		CHECK(ranges::data(rng) == nullptr);
		CHECK(rng.data() == nullptr);
		CHECK(ranges::size(rng) == 0);
		CHECK(rng.size() == 0);
	}

	{
		CHECK(ranges::begin(view::empty<int>) == nullptr);
		CHECK(ranges::end(view::empty<int>) == nullptr);
		CHECK(ranges::data(view::empty<int>) == nullptr);
		CHECK(ranges::size(view::empty<int>) == 0);
	}
}
