// Range v3 library
//
//  Copyright Andrew Sutton 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <nanorange/algorithm/count.hpp>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace {

struct S {
	int i;
};

}

TEST_CASE("alg.count")
{
	using namespace nano;
	using nano::make_subrange;

	int ia[] = {0, 1, 2, 2, 0, 1, 2, 3};
	constexpr unsigned cia = size(ia);

	CHECK(count(input_iterator<const int*>(ia),
				sentinel<const int*>(ia + cia), 2) == 3);
	CHECK(count(input_iterator<const int*>(ia),
				sentinel<const int*>(ia + cia), 7) == 0);
	CHECK(count(input_iterator<const int*>(ia),
				sentinel<const int*>(ia), 2) == 0);

	CHECK(count(make_subrange(input_iterator<const int*>(ia),
					  sentinel<const int*>(ia + cia)), 2) == 3);
	CHECK(count(make_subrange(input_iterator<const int*>(ia),
					  sentinel<const int*>(ia + cia)), 7) == 0);
	CHECK(count(make_subrange(input_iterator<const int*>(ia),
					  sentinel<const int*>(ia)), 2) == 0);

	S sa[] = {{0}, {1}, {2}, {2}, {0}, {1}, {2}, {3}};
	constexpr unsigned csa = size(ia);

	CHECK(count(input_iterator<const S*>(sa),
				sentinel<const S*>(sa + csa), 2, &S::i) == 3);
	CHECK(count(input_iterator<const S*>(sa),
				sentinel<const S*>(sa + csa), 7, &S::i) == 0);
	CHECK(count(input_iterator<const S*>(sa),
				sentinel<const S*>(sa), 2, &S::i) == 0);

	CHECK(count(make_subrange(input_iterator<const S*>(sa),
					  sentinel<const S*>(sa + csa)), 2, &S::i) == 3);
	CHECK(count(make_subrange(input_iterator<const S*>(sa),
					  sentinel<const S*>(sa + csa)), 7, &S::i) == 0);
	CHECK(count(make_subrange(input_iterator<const S*>(sa),
					  sentinel<const S*>(sa)), 2, &S::i) == 0);

	{
		auto l = {0, 1, 2, 2, 0, 1, 2, 3};
		CHECK(count(std::move(l), 2) == 3);
	}
	{
		auto l = {0, 1, 2, 2, 0, 1, 2, 3};
		CHECK(count(std::move(l), 7) == 0);
	}
}
