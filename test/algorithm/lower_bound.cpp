// Range v3 library
//
//  Copyright Eric Niebler 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3
//
//  Copyright 2005 - 2007 Adobe Systems Incorporated
//  Distributed under the MIT License(see accompanying file LICENSE_1_0_0.txt
//  or a copy at http://stlab.adobe.com/licenses.html)

#include <stl2/detail/algorithm/lower_bound.hpp>
#include <stl2/view/iota.hpp>
#include <vector>
#include <utility>
#include "../simple_test.hpp"

namespace stl2 = __stl2;

struct my_int
{
	int value;
};

bool compare(my_int lhs, my_int rhs)
{
	return lhs.value < rhs.value;
}

void not_totally_ordered()
{
	// This better compile!
	std::vector<my_int> vec;
	stl2::lower_bound(vec, my_int{10}, compare);
}

int main()
{
	using stl2::begin;
	using stl2::end;
	using stl2::size;
	using stl2::less;

	std::pair<int, int> a[] = {{0, 0}, {0, 1}, {1, 2}, {1, 3}, {3, 4}, {3, 5}};
	const std::pair<int, int> c[] = {{0, 0}, {0, 1}, {1, 2}, {1, 3}, {3, 4}, {3, 5}};

	CHECK(stl2::ext::lower_bound_n(begin(a), size(a), a[0]) == &a[0]);
	CHECK(stl2::ext::lower_bound_n(begin(a), size(a), a[1], less<>()) == &a[1]);
	CHECK(stl2::ext::lower_bound_n(begin(a), size(a), 1, less<>(), &std::pair<int, int>::first) == &a[2]);

	CHECK(stl2::lower_bound(begin(a), end(a), a[0]) == &a[0]);
	CHECK(stl2::lower_bound(begin(a), end(a), a[1], less<>()) == &a[1]);
	CHECK(stl2::lower_bound(begin(a), end(a), 1, less<>(), &std::pair<int, int>::first) == &a[2]);

	CHECK(stl2::lower_bound(a, a[2]) == &a[2]);
	CHECK(stl2::lower_bound(c, c[3]) == &c[3]);

	CHECK(stl2::lower_bound(a, a[4], less<>()) == &a[4]);
	CHECK(stl2::lower_bound(c, c[5], less<>()) == &c[5]);

	CHECK(stl2::lower_bound(a, 1, less<>(), &std::pair<int, int>::first) == &a[2]);
	CHECK(stl2::lower_bound(c, 1, less<>(), &std::pair<int, int>::first) == &c[2]);

	CHECK(stl2::lower_bound(stl2::move(a), 1, less<>(), &std::pair<int, int>::first).get_unsafe() == &a[2]);
	CHECK(stl2::lower_bound(stl2::move(c), 1, less<>(), &std::pair<int, int>::first).get_unsafe() == &c[2]);

	CHECK(*stl2::lower_bound(stl2::ext::iota_view<int>{}, 42).get_unsafe() == 42);

	return test_result();
}
