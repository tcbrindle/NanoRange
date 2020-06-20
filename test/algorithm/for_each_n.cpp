// Range v3 library
//
//  Copyright Eric Niebler 2014-present
//  Copyright Rostislav Khlebnikov 2017
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <nanorange/algorithm/for_each.hpp>
#include <nanorange/views/subrange.hpp>
#include <vector>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace stl2 = nano;

namespace {

struct S {
	void p() const { *p_ += i_; }

	int *p_;
	int i_;
};

}

TEST_CASE("alg.for_each_n")
{
	int sum = 0;
	auto fun = [&](int i){ sum += i; };
	std::vector<int> v1 { 1, 2, 4, 6 };
	CHECK(stl2::for_each_n(v1.begin(), 2, fun).in == v1.begin() + 2);
	CHECK(sum == 3);

	sum = 0;
	auto rfun = [&](int& i){ sum += i; };
	auto const sz = static_cast<int>(v1.size());
	CHECK(stl2::for_each_n(v1.begin(), sz, rfun).in == v1.end());
	CHECK(sum == 13);

	sum = 0;
	std::vector<S> v2{{&sum, 1}, {&sum, 2}, {&sum, 4}, {&sum, 6}};
	CHECK(stl2::for_each_n(v2.begin(), 3, &S::p).in == v2.begin() + 3);
	CHECK(sum == 7);

	sum = 0;
	CHECK(stl2::for_each_n(v2.begin(), 4, fun, &S::i_).in == v2.end());
	CHECK(sum == 13);
}
