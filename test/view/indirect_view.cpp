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
#include <stl2/view/indirect.hpp>
#include <vector>
#include "../simple_test.hpp"
#include "../test_utils.hpp"

namespace view = __stl2::view::ext;

int main()
{
	std::vector<std::shared_ptr<int>> vp;
	for(int i = 0; i < 10; ++i)
		vp.push_back(std::make_shared<int>(i));
	auto && rng = vp | view::indirect;
	CHECK(&*begin(rng) == vp[0].get());
	CHECK_EQUAL(rng, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

	// {
	// 	int const some_ints[] = {0,1,2,3};
	// 	int const *some_int_pointers[] = {
	// 		some_ints + 0, some_ints + 1, some_ints + 2, some_ints + 3
	// 	};
	// 	auto make_range = [&]{
	// 		return debug_input_view<int const *>{some_int_pointers} | view::indirect;
	// 	};
	// 	auto rng = make_range();
	// 	CHECK_EQUAL(rng, some_ints);
	// 	rng = make_range();
	// 	CHECK(&*begin(rng) == some_ints + 0);
	// }

	return ::test_result();
}
