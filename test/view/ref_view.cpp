// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter
//  Copyright Christopher Di Bella
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/view/ref.hpp>
#include <vector>
#include "../simple_test.hpp"
#include "../test_utils.hpp"
#include <stl2/detail/algorithm/transform.hpp>

namespace view = __stl2::view::ext;

int main()
{
	auto rng = std::vector<int>(10, 0);
	auto iota = 0;
	for (auto&& i : rng | view::ref) {
		i += iota;
		++iota;
	}
	CHECK_EQUAL(rng, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

	return ::test_result();
}

