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

#include <nanorange.hpp>
#include <vector>
#include "../catch.hpp"

namespace ranges = nanorange;

namespace {

bool even(int n) { return n % 2 == 0; }

struct S {
    S(bool p)
            : test(p) {}

    bool p() const { return test; }

    bool test;
};

}

TEST_CASE("alg.all_of")
{
	std::vector<int> all_even { 0, 2, 4, 6 };
	std::vector<int> one_even { 1, 3, 4, 7 };
	std::vector<int> none_even { 1, 3, 5, 7 };
	REQUIRE(ranges::all_of(all_even.begin(), all_even.end(), even));
	REQUIRE(!ranges::all_of(one_even.begin(), one_even.end(), even));
	REQUIRE(!ranges::all_of(none_even.begin(), none_even.end(), even));

	REQUIRE(ranges::all_of(all_even, even));
	REQUIRE(!ranges::all_of(one_even, even));
	REQUIRE(!ranges::all_of(none_even, even));

	{
		auto l = {0, 2, 4, 6};
		REQUIRE(ranges::all_of(std::move(l), [](int n) { return n % 2 == 0; }));
	}
	{
		auto l = {1, 3, 4, 7};
		REQUIRE(!ranges::all_of(std::move(l), [](int n) { return n % 2 == 0; }));
	}
	{
		auto l = {1, 3, 5, 7};
		REQUIRE(!ranges::all_of(std::move(l), [](int n) { return n % 2 == 0; }));
	}

	std::vector<S> all_true { true, true, true };
	std::vector<S> one_true { false, false, true };
	std::vector<S> none_true { false, false, false };
	REQUIRE(ranges::all_of(all_true.begin(), all_true.end(), &S::p));
	REQUIRE(!ranges::all_of(one_true.begin(), one_true.end(), &S::p));
	REQUIRE(!ranges::all_of(none_true.begin(), none_true.end(), &S::p));

	REQUIRE(ranges::all_of(all_true, &S::p));
	REQUIRE(!ranges::all_of(one_true, &S::p));
	REQUIRE(!ranges::all_of(none_true, &S::p));

	{
		auto l = {S(true), S(true), S(true)};
		REQUIRE(ranges::all_of(std::move(l), &S::p));
	}
	{
		auto l = {S(false), S(true), S(false)};
		REQUIRE(!ranges::all_of(std::move(l), &S::p));
	}
	{
		auto l = {S(false), S(false), S(false)};
		REQUIRE(!ranges::all_of(std::move(l), &S::p));
	}
}
