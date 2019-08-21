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
#include <nanorange/views/empty.hpp>
#include "../catch.hpp"

namespace ranges = nano::ranges;

TEST_CASE("views.empty") {
	using namespace ranges;

	{
		auto rng = views::empty<double>;

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
		CHECK(ranges::begin(views::empty<int>) == nullptr);
		CHECK(ranges::end(views::empty<int>) == nullptr);
		CHECK(ranges::data(views::empty<int>) == nullptr);
		CHECK(ranges::size(views::empty<int>) == 0);
	}
}
