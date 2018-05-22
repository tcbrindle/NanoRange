// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/view/repeat_n.hpp>
#include <stl2/iterator.hpp>
#include <stl2/algorithm.hpp>
#include "../simple_test.hpp"

namespace stl2 = __stl2;

int main() {
	static constexpr int N = 13;
	static constexpr int value = 42;
	auto v = stl2::ext::repeat_n_view<int>(value, N);
	using V = decltype(v);
	static_assert(stl2::models::View<V>);
	static_assert(stl2::models::SizedRange<V>);

	CHECK(stl2::size(v) == N);
	CHECK(stl2::count(v, value) == N);
	CHECK(stl2::equal(v, std::vector<int>(N, value)));

	static_assert(sizeof(v) == 2 * sizeof(std::ptrdiff_t));

	{
		struct empty {};
		auto v = stl2::ext::repeat_n_view<empty>{{}, (1ULL << 20)};
		static_assert(sizeof(decltype(v.begin())) == sizeof(std::ptrdiff_t));
	}

	return test_result();
}
